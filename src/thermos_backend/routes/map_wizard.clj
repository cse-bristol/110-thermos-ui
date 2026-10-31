;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.routes.map-wizard
  "Routes for the new map wizard. The wizard's state is kept in the
  session under ::wizards, keyed by a wizard id which is in the URL, so you
  can have more than one going at once."
  (:require [thermos-backend.routes.responses :refer :all]
            [thermos-backend.auth :as auth]
            [thermos-backend.map-wizard :as wizard]
            [thermos-backend.pages.map-wizard :as pages]
            [thermos-backend.pages.cache-control :as cache-control]
            [thermos-backend.db.maps :as maps]
            [thermos-backend.importer.core :as importer]
            [thermos-backend.importer.heat-degree-days :as heat-degree-days]
            [thermos-backend.project-lidar :as project-lidar]
            [thermos-backend.restrictions :as restrictions]
            [thermos-backend.config :refer [config]]
            [ring.util.response :as response]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.logging :as log]))

(def ^:private max-wizards
  "How many wizards to keep in someone's session"
  5)

(defn- save-state
  "Put `state` into the session for `response`"
  [response request state]
  (let [wid (:wizard-id (:params request))]
    (-> response
        (assoc :session
               (assoc-in (:session request) [::wizards wid]
                         {:state state :time (System/currentTimeMillis)}))
        (cache-control/no-store))))

(defn- param
  "Form fields which can be keywords are keywords, others are strings"
  [params k]
  (or (get params k) (get params (keyword k))))

(defn- parse-json [s]
  (when-not (string/blank? s)
    (json/parse-string s)))

(defmacro ^:private with-wizard
  "Bind `state` to the wizard state for `request` and do body, if the
  user can change the project. If the wizard has gone (perhaps the server
  restarted), start again."
  {:style/indent 1}
  [[state request] & body]
  `(let [request# ~request
         {project-id# :project-id wizard-id# :wizard-id} (:params request#)]
     (auth/verify [:modify :project project-id#]
       (let [~state (get-in request# [:session ::wizards wizard-id# :state])]
         (if (and ~state (= project-id# (:project-id ~state)))
           (do ~@body)
           (htmx-redirect request# (str "/project/" project-id# "/map/new")))))))

(defn- step-url [state request step]
  (pages/url state (:wizard-id (:params request)) step))

(defn- render-step
  [state request step & [extra]]
  (let [extra (cond-> extra
                (= :lidar step)
                (assoc :coverage (project-lidar/lidar-coverage-geojson
                                  (:project-id state)
                                  :include-system-lidar true)))]
    (-> (pages/wizard-page state (:wizard-id (:params request)) step extra)
        (html)
        (save-state request state))))

(defn- parse-step [step]
  (some #{(keyword step)} wizard/steps))

(defn start-wizard [{{:keys [project-id]} :params session :session}]
  (auth/verify [:modify :project project-id]
    (let [wid   (subs (str (java.util.UUID/randomUUID)) 0 8)
          state (assoc wizard/start-state :project-id project-id)
          ;; drop the oldest wizards
          wizards (->> (::wizards session)
                       (sort-by (comp :time val) >)
                       (take (dec max-wizards))
                       (into {}))]
      (-> (response/redirect (pages/url state wid :name) :see-other)
          (assoc :session (assoc session ::wizards
                                 (assoc wizards wid {:state state
                                                     :time (System/currentTimeMillis)})))
          (cache-control/no-store)))))

(defn- show-step [{{:keys [step]} :params :as request}]
  (with-wizard [state request]
    (if-let [step (parse-step step)]
      (render-step state request step)
      (response/not-found "No such step"))))

(defn- update-step
  "Put the fields submitted for `step` into the state"
  [state step params]
  (case step
    :name
    (assoc state
           :name (string/trim (str (param params "name")))
           :description (string/trim (str (param params "description"))))

    (:building-cols :road-cols)
    (let [n (count (wizard/columns-for-step state step))]
      (wizard/set-mapping state step
                          (for [i (range n)] (param params (str "col:" i)))))

    :other-parameters
    (let [dd (param params "degree-days")]
      (-> state
          (cond-> (and dd (re-matches #"\d+(\.\d*)?" dd))
            (assoc :degree-days (int (Double/parseDouble dd))))
          (assoc-in [:roads :group-buildings]
                    (pages/parse-group-buildings (param params "group-buildings")))))

    state))

(defn- add-degree-days
  "Look up the heating degree days for the middle of the map"
  [state]
  (let [{:keys [lat lng]} (wizard/centroid state)
        hdd (try (when (and lat lng)
                   (heat-degree-days/get-hdd (double lng) (double lat)))
                 (catch Exception e
                   (log/error e "Getting HDD for" lat lng)))]
    (if (and hdd (not= -1 (int hdd)))
      (assoc state :degree-days (int hdd) :hdd-from-server? true)
      (assoc state :hdd-from-server? false))))

(defn- create-map! [state request]
  (let [project-id (:project-id state)
        problems   (for [step [:name :geometry]
                         :let [messages (wizard/requirements state step)]
                         :when (seq messages)]
                     [step messages])]
    (cond
      (seq problems)
      (let [[step messages] (first problems)]
        (render-step state request step {:messages messages}))

      (restrictions/exceeded-gis-feature-count? auth/*current-user*)
      (render-step state request :other-parameters
                   {:messages ["You have exceeded the number of buildings and roads you can have in your projects, so you cannot create another map."]})

      :else
      (let [map-id (maps/create-map! project-id (:name state) (:description state)
                                     (wizard/parameters state))]
        (importer/queue-import {:map-id map-id :user-id (:id auth/*current-user*)})
        (-> (response/redirect (str "/project/" project-id) :see-other)
            (assoc :session (update (:session request) ::wizards
                                    dissoc (:wizard-id (:params request))))
            (cache-control/no-store))))))

(defn- submit-step [{{:keys [step go] :as params} :params :as request}]
  (with-wizard [state request]
    (if-let [step (parse-step step)]
      (let [state (update-step state step params)]
        (case go
          "back"
          (-> (response/redirect (step-url state request (wizard/previous-step state step)) :see-other)
              (save-state request state))

          "create"
          (create-map! state request)

          ;; otherwise next
          (let [messages (wizard/requirements state step)
                next     (wizard/next-step state step)]
            (if (or (seq messages) (nil? next))
              (render-step state request step {:messages messages})
              (-> (response/redirect (step-url state request next) :see-other)
                  (save-state request (cond-> state
                                        (= :other-parameters next)
                                        (add-degree-days))))))))
      (response/not-found "No such step"))))

(defn- set-source [{{:keys [source]} :params :as request}]
  (with-wizard [state request]
    (let [state (cond-> state
                  (#{"osm" "files"} source)
                  (assoc-in [:geometry :source] (keyword source)))]
      (render-step state request :geometry))))

(defn- save-osm-area
  "The map on the geometry step posts the area that has been chosen here."
  [{params :params :as request}]
  (with-wizard [state request]
    (let [osm-id       (param params "osm-id")
          boundary     (parse-json (param params "boundary"))
          centroid     (parse-json (param params "centroid"))
          map-position (parse-json (param params "map-position"))
          state (cond-> (assoc-in state [:geometry :map-position] map-position)
                  boundary
                  (assoc-in [:geometry :osm]
                            (cond-> {:boundary boundary}
                              (not (string/blank? osm-id)) (assoc :osm-id osm-id)))

                  centroid
                  (assoc-in [:geometry :centroid]
                            {:lat (get centroid "lat") :lng (get centroid "lng")}))]
      (-> (fragment (pages/step-nav state :geometry :oob true))
          (save-state request state)))))

(defn- store-upload
  "Move a group of uploaded files into the import directory and read what's
  in them."
  [{:keys [uploads] :as group}]
  (if (:state group)
    group ;; already invalid
    (try
      (let [ident      (str (java.util.UUID/randomUUID))
            target-dir (io/file (config :import-directory) ident)]
        (.mkdirs target-dir)
        (doseq [file uploads]
          (java.nio.file.Files/move
           (.toPath ^java.io.File (:tempfile file))
           (.toPath (io/file target-dir (.getName (io/file (:filename file)))))
           (into-array java.nio.file.CopyOption [])))
        (if-let [info (importer/get-file-info target-dir)]
          (merge group info {:id ident :state :uploaded})
          (assoc group :state :invalid :message "Unable to read this file")))
      (catch Exception e
        (log/error e "Reading uploaded files" (:base-name group))
        (assoc group :state :error :message (str "Error reading file: " (.getMessage e)))))))

(defn- upload-files [{{:keys [file]} :params :as request}]
  (with-wizard [state request]
    (let [uploads (filter :tempfile (if (sequential? file) file [file]))
          groups  (->> (wizard/group-uploads uploads)
                       (map (fn [[k v]] [k (dissoc (store-upload v) :uploads)]))
                       (into {}))
          state   (update-in state [:geometry :files] merge groups)]
      (-> (fragment (pages/file-list state (:wizard-id (:params request)))
                    (pages/step-nav state :geometry :oob true))
          (save-state request state)))))

(defn- remove-file [{{:keys [base]} :params :as request}]
  (with-wizard [state request]
    (let [state (update-in state [:geometry :files] dissoc base)]
      (-> (fragment (pages/file-list state (:wizard-id (:params request)))
                    (pages/step-nav state :geometry :oob true))
          (save-state request state)))))

(defn- set-osm-roads [{{:keys [include-osm]} :params :as request}]
  (with-wizard [state request]
    (let [state (assoc-in state [:roads :include-osm] (= "true" include-osm))]
      (-> (fragment)
          (save-state request state)))))

(defn- guess-fields [{{:keys [step harder] :as params} :params :as request}]
  (with-wizard [state request]
    (let [step (keyword step)]
      (if (contains? wizard/field-steps step)
        (let [state (-> state
                        (update-step step params)
                        (wizard/guess-mapping step (= "true" harder)))]
          (-> (fragment (pages/field-table state (:wizard-id (:params request)) step))
              (save-state request state)))
        (response/not-found "No such step")))))

(defn- field-help [{{:keys [step]} :params}]
  (let [step (keyword step)]
    (if (contains? wizard/field-steps step)
      (fragment (pages/field-help step))
      (response/not-found "No such step"))))

(defn- upload-lidar [{{:keys [file]} :params :as request}]
  (with-wizard [state request]
    (let [error (try
                  (when (seq (filter :tempfile (if (sequential? file) file [file])))
                    (project-lidar/upload-lidar! file (:project-id state)))
                  nil
                  (catch Exception e
                    (log/warn e "Uploading LIDAR")
                    (.getMessage e)))]
      (render-step state request :lidar {:error error}))))

(def wizard-routes
  "Routes under /project/<id>/map/new"
  [["" {:get start-wizard}]
   [["/" :wizard-id]
    [["/source" {:post set-source}]
     ["/osm" {:post save-osm-area}]
     ["/upload" {:post upload-files}]
     ["/remove-file" {:post remove-file}]
     ["/roads" {:post set-osm-roads}]
     [["/guess/" :step] {:post guess-fields}]
     [["/field-help/" :step] {:get field-help}]
     ["/lidar-upload" {:post upload-lidar}]
     [["/" :step] {:get show-step :post submit-step}]]]])

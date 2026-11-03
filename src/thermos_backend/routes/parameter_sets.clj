;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.routes.parameter-sets
  "The library of parameter sets, the parameter set editor (which is the
  network editor, with a parameter set in it), and what the network
  editor asks for to save and apply sets."
  (:require [thermos-backend.routes.responses :refer :all]
            [thermos-backend.auth :as auth]
            [thermos-backend.db.parameter-sets :as parameter-sets]
            [thermos-backend.pages.parameter-sets :as pages]
            [thermos-backend.pages.editor :as editor]
            [thermos-backend.pages.cache-control :as cache-control]
            [thermos-backend.network-transit :as network-transit]
            [thermos-specs.parameters :as parameters]
            [thermos-specs.defaults :as defaults]
            [ring.util.response :as response]
            [clojure.string :as string]))

(defn- library-data
  "What the library page and its dialogs need to know about the user;
  with `view-project`, for that project's page of the library"
  ([] (library-data nil))
  ([view-project]
   (let [user-id (:id auth/*current-user*)
         projects (parameter-sets/user-projects user-id)
         member (first (filter #(= view-project (:id %)) projects))]
     (cond->
         {:sets           (parameter-sets/visible-sets user-id :project-id view-project)
          :projects       projects
          :user-defaults  (parameter-sets/user-defaults user-id)
          :system-default (parameter-sets/system-default)
          :sysadmin       (auth/current-sysadmin?)
          :can-edit?      auth/can-edit-parameter-set?}
       view-project
       (assoc :view-project view-project
              :view-project-info
              (assoc (or member (parameter-sets/project-info view-project))
                     :can-set-project-default
                     (or (auth/current-sysadmin?) (= :admin (:auth member))))
              :default-choice (parameter-sets/default-choice user-id view-project))))))

(defn- view-project
  "The project whose page of the library this is, from ?project="
  [params]
  (some-> (:project params) str parse-long))

(defn- dialog [content]
  (-> (fragment content) (cache-control/no-store)))

(defn- library-page [{:keys [params]}]
  (let [project (view-project params)]
    (auth/verify (if project [:modify :project project] :logged-in)
      (-> (html (pages/library-page (library-data project)))
          (cache-control/no-store)))))

(defn- uploaded-content
  "The parameter set posted by the editor, as transit"
  [content]
  (when (:tempfile content)
    (network-transit/read-string (slurp (:tempfile content)))))

(defn- default-content
  "A new set has all the built-in parameters"
  []
  (parameters/extract defaults/default-document parameters/categories))

(defn- new-set-dialog [{:keys [params]}]
  (auth/verify :logged-in
    (dialog (pages/new-set-form (library-data) :project-id (view-project params)))))

(defn- create-set!
  "Create a set from the library (a form, with just a name and scope),
  or from the editor (which posts the content too, and wants the new
  set's ID back)."
  [{{:keys [name scope content]} :params :as request}]
  (auth/verify :logged-in
    (let [[scope project-id] (pages/parse-scope scope)
          posted (uploaded-content content)]
      (if (or (nil? scope)
              (string/blank? name)
              (not (auth/can-create-parameter-set? scope project-id)))
        auth/forbidden
        (let [id (parameter-sets/create-set!
                  {:name (string/trim name)
                   :scope scope
                   :user-id (:id auth/*current-user*)
                   :project-id project-id
                   :created-by (:id auth/*current-user*)
                   :content (or posted (default-content))})]
          (if posted
            (-> (transit-json {:id id})
                (response/status 201)
                (cache-control/no-store))
            (response/redirect (str "/parameters/" id) :see-other)))))))

(defn- set-list-json
  "The sets the user can apply to a network in `project`"
  [{{:keys [project]} :params}]
  (auth/verify :logged-in
    (-> (parameter-sets/visible-sets (:id auth/*current-user*)
                                     :project-id (some-> project Long/parseLong))
        (->> (map #(select-keys % [:id :name :scope :project-name :categories :revision])))
        (transit-json)
        (cache-control/no-store))))

(defn- revisions-json
  "The current revisions of the sets `ids` (a comma-separated list),
  so the editor can say if any has changed since it was applied"
  [{{:keys [ids]} :params}]
  (auth/verify :logged-in
    (let [ids (->> (string/split (or ids "") #",")
                   (remove string/blank?)
                   (keep #(parse-long (string/trim %))))]
      (-> (parameter-sets/current-revisions (:id auth/*current-user*) ids)
          (transit-json)
          (cache-control/no-store)))))

(defn- set-editor-page
  "The set, as transit for the network editor applying it, or the editor
  with the set in it."
  [{{:keys [set-id]} :params {accept "accept"} :headers}]
  (auth/verify [:read :parameter-set set-id]
    (let [ps (parameter-sets/get-set set-id)
          accept (set (string/split (or accept "") #","))]
      (-> (if (accept "application/transit+json")
            (transit-json (select-keys ps [:id :name :revision :categories :content]))
            (html
             (editor/editor-page
              (:name ps)
              (pr-str (parameters/complete (:content ps) defaults/default-document))
              nil nil
              (not (auth/can-edit-parameter-set? ps))
              nil
              :title (str "Parameter set " (:name ps))
              :preloads {:parameter-set
                         {:id (:id ps)
                          :scope (:scope ps)
                          :project-name (:project-name ps)
                          :categories (:categories ps)}})))
          (cache-control/no-store)))))

(defn- save-set!
  "Save the editor's parameter set, keeping only its categories"
  [{{:keys [set-id name content]} :params}]
  (auth/verify [:write :parameter-set set-id]
    (let [doc (uploaded-content content)
          content (parameters/extract doc (parameters/categories-in doc))]
      (parameter-sets/update-set! set-id (cond-> {:content content}
                                           (not (string/blank? name))
                                           (assoc :name (string/trim name))))
      (-> (response/response "")
          (response/status 200)
          (response/header "Location" (str "/parameters/" set-id))
          (cache-control/no-store)))))

(defn- delete-set! [{{:keys [set-id] :as params} :params}]
  (auth/verify [:delete :parameter-set set-id]
    (parameter-sets/delete-set! set-id)
    (dialog (pages/set-list (library-data (view-project params))))))

(defn- toggle-star!
  "Click on a star in the library: make the set a default, or stop it
  being one. `which` is mine or project, and `project` the project it
  applies to (blank for everywhere); `view` is the project whose page
  of the library this is, if any, to show it again."
  [{{:keys [set-id which project view]} :params}]
  (auth/verify [:read :parameter-set set-id]
    (let [user-id (:id auth/*current-user*)
          project (parse-long (str project))
          ps (parameter-sets/get-set set-id false)
          toggle (fn [current] (when-not (= current set-id) set-id))
          ;; a project's set can only be a default in its project
          usable (or (not= :project (:scope ps)) (= project (:project-id ps)))]
      (cond
        (or (not usable)
            (not (#{"mine" "project"} which))
            (and project (auth/verify* [:modify :project project]))
            ;; only a project admin can choose its default for everyone
            (and (= which "project")
                 (or (nil? project) (auth/verify* [:rename :project project]))))
        auth/forbidden

        :else
        (do (if (= which "mine")
              (parameter-sets/set-user-default!
               user-id project (toggle (get (parameter-sets/user-defaults user-id) project)))
              (parameter-sets/set-project-default!
               project (toggle (parameter-sets/project-default project))))
            (dialog (pages/set-list (library-data (parse-long (str view))))))))))

(defn- copy-set-dialog [{{:keys [set-id]} :params}]
  (auth/verify [:read :parameter-set set-id]
    (dialog (pages/copy-set-form (library-data) (parameter-sets/get-set set-id false)))))

(defn- copy-set! [{{:keys [set-id name scope]} :params}]
  (auth/verify [:read :parameter-set set-id]
    (let [[scope project-id] (pages/parse-scope scope)]
      (if (or (nil? scope) (not (auth/can-create-parameter-set? scope project-id)))
        auth/forbidden
        (let [id (parameter-sets/copy-set!
                  set-id {:name (when-not (string/blank? name) (string/trim name))
                          :scope scope
                          :user-id (:id auth/*current-user*)
                          :project-id project-id
                          :created-by (:id auth/*current-user*)})]
          (response/redirect (str "/parameters/" id) :see-other))))))

(defn- rename-set-dialog [{{:keys [set-id]} :params}]
  (auth/verify [:write :parameter-set set-id]
    (dialog (pages/rename-set-form (parameter-sets/get-set set-id false)))))

(defn- back-to-library
  "Back to the page of the library the user came from"
  [request]
  (let [referer (get-in request [:headers "referer"])
        path (some->> referer (re-find #"/parameters(\?project=\d+)?$") first)]
    (response/redirect (or path "/parameters") :see-other)))

(defn- rename-set! [{{:keys [set-id name description]} :params :as request}]
  (auth/verify [:write :parameter-set set-id]
    (when-not (string/blank? name)
      (parameter-sets/update-set! set-id {:name (string/trim name)
                                          :description (or description "")}))
    (back-to-library request)))

(defn- default-set-dialog [{{:keys [set-id]} :params}]
  (auth/verify [:read :parameter-set set-id]
    (dialog (pages/default-set-form (library-data) (parameter-sets/get-set set-id false)))))

(defn- set-default!
  "Make the set a default in the places ticked, and stop it being one in
  the places not ticked"
  [{{:keys [set-id] :as params} :params :as request}]
  (auth/verify [:read :parameter-set set-id]
    (let [{:keys [projects user-defaults system-default sysadmin]} (library-data)
          ps (parameter-sets/get-set set-id false)
          user-id (:id auth/*current-user*)
          ticked? #(= "on" (get params (keyword %)))
          usable-in (fn [pid] (or (not= :project (:scope ps)) (= pid (:project-id ps))))
          ;; tick or untick one place
          place! (fn [ticked current set-it!]
                   (cond
                     ticked                 (set-it! set-id)
                     (= current set-id)     (set-it! nil)))]
      (when-not (= :project (:scope ps))
        (place! (ticked? "mine") (get user-defaults nil)
                #(parameter-sets/set-user-default! user-id nil %)))
      (doseq [{pid :id auth :auth current :default-parameter-set-id} projects
              :when (usable-in pid)]
        (place! (ticked? (str "mine-" pid)) (get user-defaults pid)
                #(parameter-sets/set-user-default! user-id pid %))
        (when (or sysadmin (= :admin auth))
          (place! (ticked? (str "project-" pid)) current
                  #(parameter-sets/set-project-default! pid %))))
      (when (and sysadmin (= :system (:scope ps)))
        (place! (ticked? "system") system-default parameter-sets/set-system-default!))
      (back-to-library request))))

(def parameter-set-routes
  ["/parameters"
   [["" {:get library-page}]
    [".json" {:get set-list-json}]
    ["/revisions" {:get revisions-json}]
    ["/new" {:get new-set-dialog :post create-set!}]
    [["/" [long :set-id]]
     {"" {:get set-editor-page :post save-set! :delete delete-set!}
      "/copy" {:get copy-set-dialog :post copy-set!}
      "/rename" {:get rename-set-dialog :post rename-set!}
      "/default" {:get default-set-dialog :post set-default!}
      "/star" {:post toggle-star!}}]]])

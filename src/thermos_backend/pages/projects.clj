;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.pages.projects
  (:require [thermos-backend.pages.common :refer [page]]
            [thermos-pages.common :refer [style]]
            [thermos-pages.symbols :as symbols]
            [thermos-pages.spinner :refer [spinner]]
            [thermos-pages.restriction-components :as restriction-comps]
            [thermos-frontend.format :refer [si-number]]
            [thermos-util :refer [to-fixed format-seconds]]
            [ring.util.codec :refer [url-encode]]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.tools.logging :as log]))

(defn new-project-page []
  (page
   {:title "New Project"}
   [:form.flex-rows {:method :POST}
    [:input
     {:style (style :border "1px #aaa solid"
                    :font-size :2em
                    :border-radius :5px
                    :padding :5px
                    :margin-bottom :0.5em)
      :required "required"
      :minlength 1
      :pattern ".*[^ ].*"
      :name "name" :type "text" :placeholder "Project name"}]
    [:div
     [:textarea {:style (style :width :100%
                               :padding :5px
                               :font-family :Sans)
                 :name "description" :rows 5 :placeholder "Project description"}]]
    [:div {:style (style :margin-bottom :0.5em)}
     [:div "Project members"]
     [:textarea {:style (style :width :100%
                               :padding :5px
                               :font-family :Sans)
                 :name "members" :rows 5 :placeholder "Joe Smith <joe@smith.com>"}]]
    [:div.flex-rows
     [:input.button
      {:style (style :margin-left :auto)
       :type :submit :value "Create Project"}]]]))

;; Dialogs. These are loaded into the #modal dialog on the project page,
;; so relative URLs in them are relative to the project.

(defn- dialog-buttons [& buttons]
  (into [:div.flex-cols {:style (style :margin-top :1em :gap :1em :justify-content :flex-end)}
         [:button.button {:type :button :data-close true} "CANCEL"]]
        buttons))

(defn delete-project-form [project wrong-name]
  [:form {:method :POST :action (str "/project/" (:id project) "/delete")}
   [:h1 "Really delete " (:name project) "?"]
   [:div "Do you really want to delete this project?"]
   [:div "This will delete everything related to it:"
    [:ul
     (for [m (:maps project)]
       [:li [:b (:name m)] " (map)"
        [:ul
         (for [n (keys (:networks m))]
           [:li [:b n] " (network)"])]])]]
   [:div
    {:style (style :margin-bottom :1em)}
    "If you really want to delete the project enter its name "
    [:code (:name project)]
    " below and click delete."]
   (when wrong-name
     [:div "The project name was not entered correctly. "
      "You need to enter the project name below to delete the project."])
   [:input.text-input {:style (style :width :100%)
                       :type :text :name :project-name :required true
                       :autocomplete :off}]
   (dialog-buttons
    [:button.button.button--danger {:type :submit} "DELETE"])])

(defn delete-project-page [project wrong-name]
  (page {:title (str "Delete " (:name project) "?")}
        [:div.card (delete-project-form project wrong-name)]))

(defn leave-project-form [project]
  (let [me (:user project)
        other-admins (filter
                      #(and (= :admin (:auth %))
                            (not= me (:id %)))
                      (:users project))]
    [:form {:method :POST :action (str "/project/" (:id project) "/leave")}
     [:h1 "Leave " (:name project) "?"]
     [:p "If you leave this project, you will need to ask another admin user to invite you back in to undo this."]
     (when-not (seq other-admins)
       [:p "Because there are no other admins, all other users will be converted into admins if you leave."])
     (dialog-buttons
      [:button.button.button--danger {:type :submit} "LEAVE"])]))

(defn delete-map-form [map-info]
  [:form {:method :POST :action (str "/project/" (:project-id map-info) "/map/" (:id map-info) "/delete")}
   [:h1 "Really delete " (:name map-info) "?"]
   [:p "Are you sure you want to delete this map and the "
    (count (:networks map-info)) " network problems associated with it?"]
   (dialog-buttons
    [:button.button.button--danger {:type :submit} "DELETE"])])

(defn rename-project-form [project & [return-home]]
  [:form {:method :POST :action (str "/project/" (:id project) "/rename")}
   (when return-home
     [:input {:type :hidden :name "return" :value "/"}])
   [:h1 "Rename " (:name project)]
   [:div.flex-rows {:style (style :margin-top :1em :margin-bottom :1em :gap :0.5em)}
    [:label {:for "rename-project-name"} "Project name:"]
    [:input#rename-project-name.text-input
     {:type :text :name "name" :required true :pattern ".*[^ ].*"
      :value (:name project) :autofocus true}]
    [:label {:for "rename-project-description"} "Description:"]
    [:textarea#rename-project-description.text-input
     {:name "description" :rows 3}
     (:description project)]]
   (dialog-buttons
    [:button.button {:type :submit} "RENAME"])])

(defn rename-map-form [map-info]
  [:form {:method :POST :action (str "/project/" (:project-id map-info) "/map/" (:id map-info) "/rename")}
   [:h1 "Rename " (:name map-info)]
   [:div.flex-cols
    {:style (style :margin-top :1em :margin-bottom :1em :gap :1em)}
    [:label {:for "rename-map-name"} "Rename this map to:"]
    [:input#rename-map-name.text-input.flex-grow
     {:type :text :name "name" :required true :pattern ".*[^ ].*"
      :value (:name map-info) :autofocus true}]]
   (dialog-buttons
    [:button.button {:type :submit} "RENAME"])])

(defn rename-network-form [project-id map-id network-name]
  [:form {:method :POST :action (str "/project/" project-id "/map/" map-id "/net/rename")}
   [:h1 "Rename " network-name]
   [:input {:type :hidden :name "from-name" :value network-name}]
   [:div.flex-cols
    {:style (style :margin-top :1em :margin-bottom :1em :gap :1em)}
    [:label {:for "rename-to-name"} "Rename this network to:"]
    [:input#rename-to-name.text-input.flex-grow
     {:type :text :name "to-name" :required true :pattern ".*[^ ].*"
      :placeholder network-name :autofocus true}]]
   (dialog-buttons
    [:button.button {:type :submit} "RENAME"])])

(defn- download-format
  "Radio buttons for the format of a download, GeoPackage first and
  chosen, then GeoJSON, which is `geojson-description`. Anything in the
  form marked .gpkg-only is hidden while GeoJSON is chosen."
  [geojson-description]
  [:fieldset.download-formats
   [:legend "Format"]
   [:label
    [:input {:type :radio :name "format" :value "gpkg" :checked true}]
    [:span "GeoPackage "
     [:small "A GIS file for QGIS or ArcGIS, with the results, and parameters "
      "in tables like those in the spreadsheet"]]]
   [:label
    [:input {:type :radio :name "format" :value "geojson"}]
    [:span "GeoJSON " [:small geojson-description]]]])

(defn- download-form
  "A form in the modal dialog which downloads from `action`"
  [action & content]
  (into [:form {:method :GET
                :action action
                ;; the download doesn't leave the page, so close the dialog
                :onsubmit "this.closest('dialog').close()"}]
        (concat content
                [(dialog-buttons
                  [:button.button {:type :submit} "DOWNLOAD"])])))

(defn network-download-form
  "The dialog for choosing the format to download `network` in"
  [project-id map-id network]
  (download-form
   (str "/project/" project-id "/map/" map-id "/net/" (:id network) "/data")
   [:h1 "Download " (:name network)]
   (download-format "The network's buildings and paths, without results")))

(defn map-download-form
  "The dialog for downloading `map-info`. As a GeoPackage, this can have
  any of its `networks` (every version, newest first, as from
  projects/map-networks), and the map's own buildings and paths, whose
  latest version of each network is chosen to begin with. As GeoJSON,
  it's just the map's buildings and paths."
  [project-id map-info networks]
  (let [fmt (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm")
        version (fn [{:keys [id author created has-run]} checked]
                  [:label.flex-cols {:style (style :gap :0.5em :align-items :center)}
                   [:input {:type :checkbox :name "net" :value id :checked checked}]
                   [:span "v" id
                    (when created (str ", " (.format fmt created)))
                    (when author (str ", " author))
                    (when has-run " ✓")]])]
    (download-form
     (str "/project/" project-id "/map/" (:id map-info) "/data")
     [:h1 "Download " (:name map-info)]
     (download-format "All the buildings and paths on the map (its GIS data), without networks")
     [:div.gpkg-only
      [:p {:style (style :max-width :40em)}
       "The networks you choose share the GeoPackage's layers and tables, "
       "which have a network column to tell them apart; its networks table "
       "lists them."]
      [:input {:type :hidden :name "networks" :value "chosen"}]
      (if (seq networks)
        [:div {:style (style :max-height :50vh :overflow-y :auto)}
         (for [[network-name versions] (->> (group-by :name networks)
                                            (sort-by (comp - :id first second)))
               :let [[latest & older] versions]]
           [:div {:style (style :margin-bottom :0.5em)}
            [:b network-name]
            (version latest true)
            (when (seq older)
              [:details
               [:summary (count older) " older " (if (= 1 (count older)) "version" "versions")]
               (for [v older] (version v false))])])]
        [:p "This map has no networks yet."])
      [:label.flex-cols {:style (style :gap :0.5em :align-items :center :margin-top :1em)}
       [:input {:type :checkbox :name "map-data" :value "true"
                :checked (empty? networks)}]
       "All the buildings and paths on the map (its GIS data), which "
      "it needs to be uploaded into THERMOS again"]])))

(defn upload-map-form
  "The dialog for uploading a GeoPackage downloaded from THERMOS as a new
  map, with `message` about what went wrong last time, if anything"
  [project-id & {:keys [message map-name]}]
  [:form {:hx-post     (str "/project/" project-id "/map/upload")
          :hx-encoding "multipart/form-data"
          :hx-target   "#modal"}
   [:h1 "Upload a map"]
   [:p {:style (style :max-width :40em)}
    "Upload a GeoPackage you downloaded from THERMOS, with \"All the "
    "buildings and paths on the map\" ticked, to make a new map from it. "
    "Its networks come too. Changes you make in QGIS to the map's "
    "buildings and paths (map_buildings and map_paths) are kept, "
    "but changes to the networks are not."]
   (when message
     [:p {:style (style :color :red :max-width :40em)} message])
   [:div.flex-rows {:style (style :gap :0.5em)}
    [:label "GeoPackage: "
     [:input {:type :file :name "file" :accept ".gpkg" :required true}]]
    [:label "Name: "
     [:input.text-input {:type :text :name "name" :value map-name
                         :placeholder "The name it had in THERMOS"}]]]
   [:div.upload-progress {:hidden true}]
   (dialog-buttons
    [:button.button {:type :submit} "UPLOAD"])])

(defn project-users-form
  "The form for changing who is in a project. The add / remove / promote
  buttons post the form to users/edit, which sends back the form with
  the change made, and save posts it to users, which saves it."
  [project-id users is-public]
  [:form#project-users {:method :POST :action (str "/project/" project-id "/users")}
   [:h1 "Project participants"]
   [:div
    (for [[i u] (map-indexed vector users)]
      [:div.flex-cols {:style (style :padding :0.25em
                                     :border "1px #eee solid"
                                     :border-radius :0.5em
                                     :margin-top :0.5em
                                     :align-items :center
                                     :gap :0.5em)}
       [:input {:type :hidden :name (str "id:" i) :value (:id u)}]
       [:input {:type :hidden :name (str "name:" i) :value (:name u)}]
       [:input {:type :hidden :name (str "auth:" i) :value (name (:auth u :read))}]
       (when (:new u)
         [:input {:type :hidden :name (str "new:" i) :value "true"}])
       [:div.flex-grow
        [:a {:href (str "mailto:" (:id u))} (:name u)]]

       (when (= :admin (:auth u))
         [:span {:style (style :border "1px #eee solid" :border-radius :4px :padding :2px)}
          "project admin"])
       (when (:new u)
         [:span {:style (style :background "#afa" :border "1px #eee solid"
                               :border-radius :4px :padding :2px)}
          "New"])

       (let [edit {:type :button :name "op"
                   :hx-post (str "/project/" project-id "/users/edit") :hx-target "#project-users" :hx-swap "outerHTML"}]
         (list
          (if (= :admin (:auth u))
            [:button.button (assoc edit :value (str "demote:" i) :title "Demote from admin") "↓"]
            [:button.button (assoc edit :value (str "promote:" i) :title "Promote to admin") "↑"])
          [:button.button (assoc edit :value (str "remove:" i) :title "Remove from project")
           symbols/cross]))])

    [:div.flex-cols {:style (style :margin-top :1em :gap :1em)}
     [:input.input.text-input.flex-grow {:name "invite"
                                         :placeholder "comma-separated email addresses"}]
     [:button.button {:type :button :name "op" :value "add"
                      :hx-post (str "/project/" project-id "/users/edit") :hx-target "#project-users" :hx-swap "outerHTML"}
      "Add"]]]

   [:h1 "Global visibility"]
   [:div
    [:label
     [:input {:type :checkbox :name "public" :value "true" :checked (boolean is-public)}]
     "Make project, maps, and networks visible to anyone who knows the URL"]]

   (dialog-buttons
    [:button.button {:type :submit} "Save changes"])])

;; The list of maps & networks

(defn- table [{columns :columns rows :rows :as params}]
  [:table
   (dissoc params :columns :rows)
   [:thead
    (loop [columns   columns
           group-row []
           title-row [:tr]
           cur-title nil
           span      0]
      (if (empty? columns)
        (if (= [] group-row)
          (list title-row)
          (list (into [:tr.groups]
                      (conj group-row
                            [(if cur-title
                               :th.group
                               :th) {:colspan span} cur-title]))
                title-row))

        (let [[column & columns] columns
              group-title (:group column)
              next-group (or (not= group-title cur-title)
                             (nil? group-title))]
          (recur columns
                 (cond-> group-row
                   (and (pos? span)
                        next-group)
                   (conj  [(if cur-title
                             :th.group
                             :th) {:colspan span} cur-title]))

                 (conj title-row [(if (and group-title next-group)
                                    :th.group
                                    :th) (:title column)])
                 group-title
                 (if next-group 1 (inc span))))))]

   [:tbody
    (for [row rows]
      [:tr
       (for [{key :key value :value :as column} columns]
         (let [value (or value key)]
           [:td
            (try (value row)
                 (catch Exception e
                   (log/error e "In" (:title column) row)))]))])]])

(defn- today-date []
  (.format (java.text.SimpleDateFormat. "yyyy-MM-dd")
           (java.util.Date.)))

(defn- tidy-date [today date]
  (let [date-part (subs date 0 10)]
    (if (= date-part today)
      (subs date 11 19)
      date-part)))

(defn- desc [a b] (compare b a))

(defn- percentage [x]  (and x (str (to-fixed (* 100 x) 2) "%")))
(defn- cents [x]       (and x (str (* 100 x ) "c")))
(defn- kilo-number [x] (and x (si-number (* x 1000))))

(defn- maps-url
  "URL for the maps fragment, with the network `expanded` showing all its versions"
  [expanded]
  (if (string/blank? expanded)
    "maps"
    (str "maps?expanded=" (url-encode expanded))))

(defn- network-table [map-id networks user-auth expanded]
  (let [today (today-date)]
    (table
     {:class "network-table"
      :rows
      (let [networks
            (->>
             (for [[name versions] networks
                   :let            [versions (sort-by :id desc versions)]]
               (merge (first versions)
                      {:versions versions
                       :name     name}))
             (sort-by :id desc))]
        (if expanded
          (mapcat
           (fn [x]
             (if (= (:name x) expanded)
               (concat [x] (rest (:versions x)))
               [x]))
           networks)

          networks))

      :columns
      (let [meta-column
            (fn [& {:keys [fmt] :as m}]
              (assoc (dissoc m :fmt)
                     :value (comp (or fmt identity) (:value m (:key m)) :meta)))]

        [{:key   :controls
          :value (fn [r]
                   [:span
                    (when (and user-auth (:name r))
                      [:a
                       {:title      "Delete this network"
                        :href       "#"
                        :hx-delete  (str "map/" map-id "/net/" (url-encode (:name r)))
                        :hx-confirm (str "Are you sure you want to delete the network "
                                         (:name r) "?")
                        :hx-target  "#maps"
                        :hx-swap    "outerHTML"}
                       [:span symbols/delete]])
                    " "
                    [:a
                     {:title     "Download this network"
                      :href      (str "map/" map-id "/net/" (:id r) "/data.gpkg")
                      :hx-get    (str "map/" map-id "/net/" (:id r) "/download")
                      :hx-target "#modal"}
                     [:span symbols/download]]
                    " "
                    (when (and user-auth (:name r))
                      [:a
                       {:title     "Rename this network"
                        :href      "#"
                        :hx-get    (str "map/" map-id "/net/rename?name=" (url-encode (:name r)))
                        :hx-target "#modal"}
                       [:span symbols/rename]])])
          :title ""}
         {:key   :state
          :value #(cond
                    (:has-run %)
                    [:b {:title "Model run finished"} "✓"]

                    (:job-id %)
                    [:button
                     {:style      (style :border :none :background :none :cursor :pointer)
                      :title      "Cancel this job"
                      :hx-post    (str "/admin/job/" (:job-id %))
                      :hx-vals    (json/write-str {:action "cancel"})
                      :hx-confirm "Cancel running job?"
                      :hx-swap    "none"}
                     (spinner {:size 16})])
          :title ""}
         {:key   :name
          :value (fn [row]
                   [:a {:href  (str "map/" map-id
                                    "/net/" (:id row))
                        :title (str (:meta row))}
                    (or (:name row) [:span {:style (style :margin-left :1em)} "v" (:id row)])])
          :title "Name"}

         {:key   :author
          :value :user-name
          :title "Author"}

         {:key   :date
          :value (comp (partial tidy-date today) :created)
          :title "Date"}

         {:key   :expando
          :value (fn [row]
                   (when (seq (:versions row))
                     (let [is-expanded (= (:name row) expanded)]
                       [:a {:href      "#"
                            :style     (style :font-weight (if is-expanded :bold :normal))
                            :hx-get    (maps-url (when-not is-expanded (:name row)))
                            :hx-target "#maps"
                            :hx-swap   "outerHTML"}
                        (str (count (:versions row)) " versions")])))
          :title "History"}

         (meta-column :key :objective             :title "Objective"          )
         (meta-column :key :npv-term              :title "Term"               )
         (meta-column :key :npv-rate              :title "Discount"
                      :fmt percentage)
         (meta-column :key :input-building-count  :title "In problem" :group "Buildings")
         (meta-column :key :network-count :title "On network" :group "Buildings")
         (meta-column :key :input-building-kwh    :title "Demand (Wh/yr)"
                      :fmt si-number :group "Buildings")

         (meta-column :key :input-path-length     :title "In problem (m)" :group "Paths"
                      :fmt si-number)
         (meta-column :key :network-length :title "In network (m)" :fmt si-number :group "Paths")

         (meta-column :key :supply-prices         :title "Heat price"
                      :fmt #(string/join ", " (map cents (set %))) :group "Supply")

         (meta-column :key :network-kwh :title "Capacity (Wh)" :fmt kilo-number :group "Supply")
         (meta-column :key :network-kwp :title "Capacity (Wp)" :fmt kilo-number :group "Supply")

         (meta-column :key :solution-state     :title "State" :group "Solution")
         (meta-column :key :mip-gap     :title "Gap" :fmt percentage :group "Solution")
         (meta-column :key :objective-value :title "Value" :fmt si-number :group "Solution")
         (meta-column :key :runtime :title "Runtime" :fmt format-seconds :group "Solution")])})))

(defn- mostly-bad-estimates [stats]
  (try (let [total (reduce + 0 (vals stats))
             bad   (reduce + 0
                           (for [[k v] stats :when (string/starts-with? (name k) "2d-")]
                             v))]
         (> (/ bad total) 0.5))
       (catch Exception e)))

(defn- buttons [& content]
  [:div.flex-cols {:style (style :flex-wrap :wrap)}
   (for [item content :when item]
     [:div {:style (style :margin-bottom :0.5em :margin-left :1em)} item])])

(defn- map-component [m user-auth expanded]
  [:div.wide.card {:style (style :flex-grow 1)}
   [:div.flex-cols
    [:h1.map-title {:style (style :flex-grow 1)}
     (cond
       (#{:ready :running} (keyword (:state m)))
       [:div (spinner {:size 16})]

       (:import-completed m)
       ;; the icon is twice this size, for high resolution screens
       [:img.map-icon {:width 96 :height 96 :src (str "map/" (:id m) "/icon.png")
                       :alt (str "A picture of the map " (:name m)
                                 ", with its heat demand in red")}])

     [:span (:name m)]]

    (buttons
     (when (:import-completed m)
       [:a.button
        {:href      (str "map/" (:id m) "/data.gpkg")
         :hx-get    (str "map/" (:id m) "/download")
         :hx-target "#modal"
         :title     "Download the map's GIS data, or its networks with their results"}
        "DOWNLOAD " symbols/download])
     (when user-auth
       [:a.button
        {:href      "#"
         :hx-get    (str "map/" (:id m) "/rename")
         :hx-target "#modal"}
        "RENAME " symbols/rename])
     (when user-auth
       [:a.button
        {:href      (str "map/" (:id m) "/delete")
         :hx-get    (str "map/" (:id m) "/delete")
         :hx-target "#modal"}
        "DELETE " symbols/delete])
     (when (:import-completed m)
       [:a.button {:href (str "map/" (:id m) "/net/new")} "HEAT NET " symbols/plus])
     (when (:import-completed m)
       [:a.button {:href (str "map/" (:id m) "/net/new?mode=cooling")} "COLD NET " symbols/plus]))]

   (when-let [description (:description m)] [:div description])

   (if (:import-completed m)
     [:div.flex-rows.flex-grow {:style (style :max-width :100%)}
      [:div.flex-grow
       {:style (style :margin-top :1em :max-width :100%)}
       (if-let [networks (seq (:networks m))]
         [:div {:style (style :max-width :100%)} [:b "Networks:"]
          (network-table (:id m) networks user-auth expanded)]

         "This map has no network designs associated with it yet.")]

      [:div {:style (style :margin-top :1em)}
       [:b "Demand estimates: "]
       (let [stats (:estimation-stats m)
             tot (reduce + 0 (vals stats))]
         (interpose
          ", "
          (for [[k v] (sort-by second desc stats)]
            [:span
             (Math/round (* 100.0 (/ v tot))) "% "
             (name k)])))

       (when (mostly-bad-estimates (:estimation-stats m))
         [:div {:style (style :margin-top :1em)}
          [:span {:style (style :background :pink :color :white
                                :border-radius :0.5em
                                :padding-left :0.25em
                                :padding-right :0.25em
                                :margin-right :0.25em
                                :font-weight :bold)} "Warning: "]
          "A majority of the demand estimates in this map are using low-quality 2d predictors. "
          "The estimates may be inaccurate."])]]

     (case (keyword (:state m))
       :ready
       [:div "Map import is queued, and waiting to start"]
       :running
       [:div
        (str (:message m)
             ", "
             (or (:progress m) 0)
             "% complete")]
       :completed
       [:div "Map import is completed, but not ready for presentation"]
       :failed
       [:div "Map has failed to import, because of an error:"
        [:div (:message m)]]
       :cancel
       [:div (spinner) "Map import cancel requested"]
       :cancelling
       [:div (spinner) "Map import cancelling"]
       :cancelled
       [:div "Map import cancelled"]

       [:div "Map import status unknown!"
        [:pre {:style (style :white-space :pre-wrap)}
         (str m)]]))])

(defn- anything-running?
  "Is anything in the project going to change by itself, so it's worth
  refreshing the page?"
  [project]
  (some (fn [m]
          (or (contains? #{:ready :running :cancel :cancelling} (keyword (:state m)))
              (some (fn [n] (and (:job-id n) (not (:has-run n))))
                    (apply concat (vals (:networks m))))))
        (:maps project)))

(defn project-maps
  "The maps in a project. This refreshes itself while anything is happening,
  and the `expanded` network shows all its versions."
  [project expanded]
  (let [user-auth (:user-auth project)]
    [:div#maps
     (when (anything-running? project)
       {:hx-get     (maps-url expanded)
        :hx-trigger "every 5s"
        :hx-swap    "outerHTML"
        ;; otherwise the buttons inside inherit these
        :hx-disinherit "*"})
     (if-let [maps (seq (sort-by :name (:maps project)))]
       [:div {:style (style :display :flex :flex-direction :column)}
        (for [m maps]
          (map-component m user-auth expanded))]
       [:div.wide.card
        "This project has no maps in it yet. "
        "Get started by creating a new map above."])]))

(defn project-page [project expanded]
  (let [am-admin (:user-is-admin project)
        user-auth (:user-auth project)
        restriction-info (:restriction-info project)]
    (page {:title (:name project)
           :css ["/css/projects.css"]
           :body-style {}}
          [:div
           (restriction-comps/show-user-restrictions restriction-info :as-card true)]

          [:div.wide.card {:style (style :margin-top :0.1em)}
           [:div.flex-cols
            [:h1 {:style (style :flex-grow 1)}
             (let [d (:description project)]
               (if (string/blank? d)
                 "Project options:"
                 d))]

            (buttons
             (when user-auth
               [:a.button {:href "#" :hx-get "users" :hx-target "#modal"}
                (let [n (count (:users project))]
                  (str n " USER" (when (> n 1) "S") " " symbols/person))])

             (when am-admin
               [:a.button {:href "#" :hx-get "rename" :hx-target "#modal"}
                "RENAME " symbols/rename])

             (when am-admin
               [:a.button {:href "delete" :hx-get "delete" :hx-target "#modal"}
                "DELETE " symbols/delete])

             (when (and user-auth (> (count (:users project)) 1))
               [:a.button {:href "#" :hx-get "leave" :hx-target "#modal"}
                "LEAVE 👋"])

             (when user-auth
               (if (and (:max-gis-features restriction-info)
                        (> (:num-gis-features restriction-info) (:max-gis-features restriction-info)))
                 [:button.button.button--disabled {:disabled true} "MAP " symbols/plus]
                 [:a.button {:href "map/new"} "MAP " symbols/plus]))

             (when (and user-auth
                        (not (and (:max-gis-features restriction-info)
                                  (> (:num-gis-features restriction-info)
                                     (:max-gis-features restriction-info)))))
               [:a.button {:href "#" :hx-get "map/upload" :hx-target "#modal"
                           :title "Upload a map, and its networks, in a GeoPackage downloaded from THERMOS"}
                "UPLOAD MAP " symbols/upload])

             (when user-auth
               [:a.button {:href (str "/parameters?project=" (:id project))} "PARAMETERS"])

             (when user-auth
               [:a.button {:href "lidar"} "LIDAR"]))]]

          (project-maps project expanded))))

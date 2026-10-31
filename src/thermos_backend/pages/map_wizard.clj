;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.pages.map-wizard
  "Pages for the steps of the new map wizard. See thermos-backend.map-wizard
  for the state these show, and thermos-backend.routes.map-wizard for
  what the forms in them post to."
  (:require [thermos-backend.pages.common :refer [page]]
            [thermos-backend.map-wizard :as wizard]
            [thermos-pages.common :refer [style]]
            [thermos-pages.symbols :as symbols]
            [clojure.data.json :as json]
            [clojure.string :as string]))

(defn url
  "URL for something in the wizard `wizard-id` for `state`"
  [state wizard-id & parts]
  (apply str "/project/" (:project-id state) "/map/new/" wizard-id
         (for [p parts] (str "/" (if (keyword? p) (name p) p)))))

(defn step-nav
  "Things needing doing before you can go on, and the back / next buttons.
  This goes inside the form for the step. With `oob` it is for a htmx
  response to swap into the page as well as whatever else is in there."
  [state step & {:keys [oob messages]}]
  (let [messages (or messages (wizard/requirements state step))]
    [:div#step-nav
     (when oob {:hx-swap-oob "true"})
     (when (seq messages)
       [:ul (for [m messages] [:li m])])
     ;; row-reverse so pressing enter in a field means next, not back
     [:div.flex-cols {:style (style :flex-direction :row-reverse
                                    :justify-content :flex-end
                                    :gap :1em)}
      (if (= step :other-parameters)
        [:button.button {:type :submit :name "go" :value "create"} "Create map"]
        [:button.button {:type :submit :name "go" :value "next"} "Next"])
      (when (wizard/previous-step state step)
        [:button.button {:type :submit :name "go" :value "back" :formnovalidate true} "Back"])]]))

(defn- drop-target [input-attrs label]
  [:div.drop-target {:style (style :height :100px :display :flex :flex-direction :column)}
   [:span {:style (style :margin :auto :text-align :center)}
    [:label
     [:input (merge {:type :file :name "file" :multiple true
                     :style (style :width 0 :height 0)}
                    input-attrs)]
     label]
    ", or drag files here"]])

(defn- name-step [state wid messages]
  [:form {:method :POST :action (url state wid :name)}
   [:h1 "Create a new map"]
   [:p
    "In THERMOS, a map contains information about buildings and piping routes in an area. "
    "Given the buildings and routes within a map you can define networks to optimise."]
   [:p
    "First you need to give your map a name, and you can give it a description. "
    "This information is just to help you and your colleagues know what the map is for."]
   [:div.flex-cols
    [:label {:for "map-name"} [:b "Map name: "]]
    [:input#map-name.flex-grow {:name "name" :placeholder "Metropolis"
                                :value (:name state)
                                :autofocus true}]]
   [:div.flex-cols
    [:label {:for "map-description"} [:b "Description: "]]
    [:input#map-description.flex-grow
     {:name "description"
      :placeholder "A map for planning heat networks in metropolis"
      :value (:description state)}]]
   (step-nav state :name :messages messages)])

(defn file-list
  "The uploaded files on the geometry step"
  [state wid]
  (let [files (vals (get-in state [:geometry :files]))
        has-roads (some pos? (map wizard/road-count files))]
    [:div#file-list
     (for [{:keys [base-name extensions message] :as file} (sort-by :base-name files)
           :let [ok (not (#{:invalid :error} (:state file)))]]
       [:div {:style (style :font-size :1.2em
                            :border-radius :8px
                            :background (if ok "#00ff7f" "#f44")
                            :padding :0.1em
                            :margin :0.5em)}
        [:div.flex-cols base-name (interpose ", " extensions)
         (when ok
           [:span {:style (style :margin-left :auto)}
            (wizard/building-count file) " buildings, "
            (wizard/road-count file) " roads"])
         [:span {:style (style :margin-left :auto)} " "
          [:button {:style     (style :border-radius :16px)
                    :title     "Remove this file"
                    :hx-post   (url state wid "remove-file")
                    :hx-vals   (json/write-str {:base base-name})
                    :hx-target "#file-list"
                    :hx-swap   "outerHTML"}
           symbols/delete]]]
        (when-not ok [:div message])])

     [:div
      [:label
       [:input {:type     :checkbox
                :name     "include-osm"
                :value    "true"
                :disabled (boolean has-roads)
                :checked  (and (not has-roads) (boolean (get-in state [:roads :include-osm])))
                :hx-post  (url state wid "roads")
                :hx-swap  "none"}]
       "Also import roads from OpenStreetMap for the area covered by these files"]]]))

(defn- geometry-step [state wid messages]
  (let [source (get-in state [:geometry :source])
        osm    (get-in state [:geometry :osm])
        source-radio (fn [value]
                       [:input {:name      "source" :type :radio :value (name value)
                                :checked   (= source value)
                                :hx-post   (url state wid "source")
                                :hx-target "#wizard-body"
                                :hx-select "#wizard-body"
                                :hx-swap   "outerHTML"}])]
    [:div
     [:h1 "Buildings and roads"]
     [:p
      "Heat demands and supplies are associated with buildings in the map, and "
      "potential heat pipe routes are associated with roads and paths in the map. "
      "You can acquire map data from OpenStreetMap, or you can upload your own GIS data."]

     [:div.card
      [:label (source-radio :osm)
       [:h1 "Use OpenStreetMap for buildings and roads"]]
      (when (= :osm source)
        [:form#osm-form {:hx-post    (url state wid "osm")
                         :hx-trigger "mapchange"
                         :hx-swap    "none"}
         (for [n ["osm-id" "boundary" "centroid" "map-position"]]
           [:input {:type :hidden :name n}])
         [:p "You can search for a named area in OpenStreetMap, or draw a box."]
         [:div.search
          [:input.map-search {:type :search :placeholder "Search..."}]
          [:div.results]]
         [:div.map-box {:style (style :width :100% :height :400px)
                        :data-config (json/write-str
                                      {:boundary (:boundary osm)
                                       :bounds   (get-in state [:geometry :map-position])
                                       :draw     true})}]])]

     [:div.card
      [:label (source-radio :files)
       [:h1 "Upload GIS Files"]]
      (when (= :files source)
        [:div
         [:p "You can upload geometry in these formats:"]
         [:ul
          [:li [:a {:href "http://geojson.org/" :target "_blank"} "GeoJSON"]]
          [:li [:a {:href "https://en.wikipedia.org/wiki/Shapefile" :target "_blank"} "ESRI Shapefile"]
           " - don't forget to include the "
           [:b ".shp"] ", " [:b ".dbf"] ", "
           [:b ".shx"] ", " [:b ".prj"] " and "
           [:b ".cpg"] " files!"]
          [:li [:a {:href "https://www.geopackage.org/" :target "_blank"} "GeoPackage"]]]

         [:form {:hx-post     (url state wid "upload")
                 :hx-encoding "multipart/form-data"
                 :hx-trigger  "change"
                 :hx-target   "#file-list"
                 :hx-swap     "outerHTML"}
          (drop-target {} "Choose files to upload")
          [:div.upload-progress {:hidden true}]]

         (file-list state wid)])]

     [:form {:method :POST :action (url state wid :geometry)}
      (step-nav state :geometry :messages messages)]]))

(defn field-table [state wid step]
  (let [{:keys [key options]} (wizard/field-steps step)
        mapping (get-in state [key :mapping])
        columns (map-indexed vector (wizard/columns-for-step state step))]
    [:div#field-table.flex-grid
     (for [[file columns] (group-by (comp first second) columns)]
       [:div.card
        [:h1 file]
        [:table
         [:thead [:tr [:th "Field"] [:th "Meaning "]]]
         [:tbody
          (for [[i [_ field]] columns
                :let [current (get-in mapping [file field])]]
            [:tr
             [:td field]
             [:td
              [:select {:name (str "col:" i)}
               [:option {:value "none"} "None"]
               (for [option options]
                 [:option {:value (name (:value option))
                           :selected (= current (:value option))}
                  (:label option)])]]])]]])]))

(defn field-help [step]
  [:div
   [:h1 "Field help"]
   [:dl {:style (style :max-height :70vh :overflow-y :auto)}
    (for [f (:options (wizard/field-steps step))]
      (list
       [:dt (:label f)]
       [:dd {:style (style :max-width :30em)} (:doc f)]))]
   [:div.flex-cols
    [:button.button {:style (style :margin-left :auto) :data-close true} "Close"]]])

(defn- field-step [state wid step messages]
  (let [{:keys [label]} (wizard/field-steps step)]
    [:form {:method :POST :action (url state wid step)}
     [:div.flex-cols
      [:h1 (str "Allocate fields for " label " ")
       [:button
        {:type      :button
         :title     "What do these mean?"
         :hx-get    (url state wid "field-help" step)
         :hx-target "#modal"
         :style     (style :background :blue
                           :border :none
                           :text-align :center
                           :padding 0
                           :margin 0
                           :color :white
                           :border-radius :0.6em
                           :width :1.2em
                           :height :1.2em)}
        "?"]]
      [:div.flex-cols {:style (style :margin-left :auto :gap :1em :align-items :center)}
       [:label {:title "If a field's meaning can't be guessed, keep it as a user-defined field"}
        [:input {:type :checkbox :name "harder" :value "true"}]
        "Keep other fields"]
       [:button.button
        {:type      :button
         :hx-post   (url state wid "guess" step)
         :hx-target "#field-table"
         :hx-swap   "outerHTML"}
        "Guess"]]]

     (field-table state wid step)
     (step-nav state step :messages messages)]))

(defn- lidar-step [state wid messages coverage error]
  [:div
   [:h1 "Check LIDAR coverage"]
   [:p "Check your map area for LIDAR coverage. LIDAR is used
          to calculate building heights and volumes for heat demand estimation."]
   [:p "LIDAR data is shared across all maps in this project."]
   [:p "LIDAR coverage is not mandatory, but building height data of some sort will
          improve the quality of any demand estimates produced from the built-in regression model.
          This can be from a field on the building polygons, or from OpenStreetMap if that
          is the source of buildings. OpenStreetMap does not always contain height data."]

   [:div.card
    [:div.map-box {:style (style :width :100% :height :400px)
                   :data-config (json/write-str
                                 {:boundary (wizard/extent-geojson state)
                                  :coverage coverage})}]]

   [:h1 "Add more LIDAR"]
   [:p "Upload any other LIDAR tiles you want to use:"]
   [:form {:hx-post     (url state wid "lidar-upload")
           :hx-encoding "multipart/form-data"
           :hx-trigger  "change"
           :hx-target   "#wizard-body"
           :hx-select   "#wizard-body"
           :hx-swap     "outerHTML"}
    (drop-target {:accept ".tif,.tiff"} "Choose LIDAR files to upload")
    [:div.upload-progress {:hidden true}]]
   (when error
     [:div {:style (style :background "#f44" :border-radius :8px :padding :0.5em :margin :0.5em)}
      error])

   [:form {:method :POST :action (url state wid :lidar)}
    (step-nav state :lidar :messages messages)]])

(def ^:private group-none "__nil__")
(def ^:private group-segment "__geo-id__")

(defn parse-group-buildings [value]
  (cond
    (or (nil? value) (= value group-none)) nil
    (= value group-segment) :geo-id
    :else value))

(defn- other-parameters-step [state wid messages]
  [:form {:method :POST :action (url state wid :other-parameters)}
   [:h1 "Set other parameters"]

   [:div.flex-rows
    [:div.card.flex-grow
     [:h1 "Heating degree days"]
     [:div.flex-cols
      [:input.flex-grow {:type :number :name "degree-days"
                         :min 0 :max 10000
                         :value (:degree-days state)}] " °C × days"]
     [:p "The number of heating degree days per year in this location, relative to a 17° base temperature."]
     (if (:hdd-from-server? state)
       [:p "This value was derived from "
        [:a {:href "https://ec.europa.eu/eurostat/cache/metadata/en/nrg_chdd_esms.htm" :target "_blank"} "Eurostat"]
        " heating degree day data for this region."]

       [:p "The default value above is not from Eurostat data as your map is outside the coverage area. "
        "You may want to use "
        [:a {:href "https://www.degreedays.net" :target "_blank"} "degree-days.net"]
        " to generate a value."])]

    [:div.card.flex-grow
     [:h1 "Automatic building groups"]
     [:p "For buildings that have not been placed in a group, THERMOS can assign a group using the road they are connected to."]
     (let [geom-state (:geometry state)
           current    (get-in state [:roads :group-buildings])]
       (if (-> geom-state :source (= :files))
         [:div.flex-cols
          [:label "Use field: "
           [:select {:name "group-buildings"}
            [:option {:value group-none :selected (nil? current)} "None - do not group"]
            [:option {:value group-segment :selected (= :geo-id current)} "Road segment"]
            ;; this is a bit shonky as we could have 2 files
            ;; with differently named fields, or same named fields.
            (for [[field files]
                  (sort-by first (group-by second (wizard/mappable-columns
                                                   geom-state wizard/road-geometry-type)))]
              [:option {:value field :selected (= field current)}
               field " (in " (string/join ", " (map first files)) ")"])]]]
         [:div
          [:label [:input {:type :checkbox :name "group-buildings" :value group-segment
                           :checked (= :geo-id current)}]
           "Use road segment to group buildings"]]))]]

   (step-nav state :other-parameters :messages messages)])

(defn wizard-page
  "The page for `step` of the wizard. `extra` has :messages to show, and
  for the lidar step the :coverage geojson and any upload :error"
  [state wid step {:keys [messages coverage error]}]
  (page
   {:title    "Create a new map"
    :css      ["/css/create-map-form.css"]
    :js       ["/js/vendor/leaflet.js" "/js/vendor/leaflet-draw.js" "/js/map-box.js"]
    :set-base false}
   [:div#wizard-body.card {:hx-sync "this:queue all"}
    (case step
      :name             (name-step state wid messages)
      :geometry         (geometry-step state wid messages)
      :building-cols    (field-step state wid :building-cols messages)
      :road-cols        (field-step state wid :road-cols messages)
      :lidar            (lidar-step state wid messages coverage error)
      :other-parameters (other-parameters-step state wid messages))]))

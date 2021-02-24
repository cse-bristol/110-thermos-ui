(ns thermos-pages.map-import-components
  (:require
   [thermos-pages.common :refer [fn-js] :refer-macros [fn-js]]
   
   [clojure.string :as string]
   [ajax.core :refer [GET POST]]
   [rum.core :as rum]
   [clojure.pprint :refer [pprint]]
   [clojure.set :refer [map-invert] :as set]
   [thermos-pages.symbols :as symbols]
   [com.rpl.specter :refer [setval MAP-VALS NONE]]
   [thermos-pages.spinner :refer [spinner]]
   
   #?@(:cljs
       [[thermos-pages.dialog :refer [show-dialog! close-dialog!]]
        [cljsjs.leaflet]
        [cljsjs.leaflet-draw]])
   [clojure.set :as set]))

(rum/defc dump < rum/reactive [form-state]
  [:pre
   (with-out-str
     (pprint
      (if (instance? #?(:cljs cljs.core.Atom
                        :clj clojure.lang.IDeref) form-state)
        (rum/react form-state)
        form-state)))])

(rum/defc name-page < rum/static rum/reactive
  [form-state]
  (let [map-name (rum/cursor-in form-state [:name])
        description (rum/cursor-in form-state [:description])]
    [:div
     [:h1 "Create a new map"]
     [:p
      "In THERMOS, a map contains information about buildings and piping routes in an area. "
      "Given the buildings and routes within a map you can define networks to optimise."]
     [:p
      "First you need to give your map a name, and you can give it a description. "
      "This information is just to help you and your colleagues know what the map is for."]
     [:div.flex-cols
      [:label [:b "Map name: "]]
      [:input.flex-grow {:placeholder "Metropolis"
               :pattern ".+"
               :on-change #(reset! map-name (.. % -target -value))
               :value (or (rum/react map-name) "")}]]
     [:div.flex-cols
      [:label [:b "Description: "]]
      [:input.flex-grow {:placeholder "A map for planning heat networks in metropolis"
               :on-change #(reset! description (.. % -target -value))
               :value (or (rum/react description) "")}]]]))

(defn bounds->extent
  "Convert a leaflet LatLngBounds object to a map with keys :x1 :y1 :x2 :y2"
  [bounds]
  {:x1 (.getWest bounds)
   :y1 (.getSouth bounds)
   :x2 (.getEast bounds)
   :y2 (.getNorth bounds)})

(rum/defc osm-map-box
  < {:did-mount
     (fn-js [state]
       (let [{[{[lat0 lat1 lon0 lon1] :map-position
                boundary-geojson :boundary-geojson
                on-draw-box :on-draw-box
                allow-drawing :allow-drawing
                lidar-coverage-geojson :lidar-coverage-geojson}]
              :rum/args} state

             node (rum/ref state :container)

             map (js/L.map node (clj->js {:minZoom 2 :maxZoom 20
                                          :zoom 15 :center [51.553356 -0.109271]}))

             layer (js/L.tileLayer
                    "https:///stamen-tiles-{s}.a.ssl.fastly.net/toner-background/{z}/{x}/{y}.png"
                    (clj->js {:subdomains "abcd"
                              :minZoom 0 :maxZoom 20}))
             labels (js/L.tileLayer
                     "https://cartodb-basemaps-{s}.global.ssl.fastly.net/rastertiles/voyager_only_labels/{z}/{x}/{y}{r}.png"
                     (clj->js {:subdomains "abcd"
                               :minZoom 0 :maxZoom 20}))

             boundary (js/L.geoJSON (clj->js boundary-geojson)) ;; yech
             lidar-coverage
             (js/L.geoJSON (clj->js lidar-coverage-geojson)
                           #js{:style
                               (fn [feature]
                                 (if (= (.. feature -properties -source) "system")
                                   #js{:color "#DD0077"}
                                   #js{:color "#33ff88"}))
                               :onEachFeature
                               (fn [feature layer]
                                 (.bindTooltip layer
                                               (if (= (.. feature -properties -source) "system")
                                                 "system LIDAR"
                                                 (.. feature -properties -filename))
                                               #js{"direction" "center"}))})

             draw-control (js/L.Control.Draw.
                           #js {"position" "topleft"
                                "draw" #js {"polyline" false
                                            "polygon" false
                                            "marker" false
                                            "circle" false
                                            "circlemarker" false}})]

         (cond
           (not (nil? lat0))
           (do (.invalidateSize map)
               (.fitBounds map (js/L.latLngBounds #js [lat0 lon0] #js [lat1 lon1])))
           (not (nil? boundary-geojson))
           (do (.invalidateSize map)
               (.fitBounds map (.getBounds boundary))))

         (.addLayer map layer)
         (.addLayer map labels)
         (.addLayer map boundary)
         (.addLayer map lidar-coverage)
         (when allow-drawing
           (.addControl map draw-control))

         (.on map (.. js/L.Draw -Event -CREATED)
              (fn [^js/L.Draw.Event e]
                (on-draw-box 
                 {:bounds (-> e .-layer (.toGeoJSON) (js->clj) (assoc-in [:properties :bounds] 
                                                                         (bounds->extent (-> e .-layer (.getBounds)))))
                  :centroid (-> e .-layer (.getBounds) (.getCenter) (js->clj) (select-keys ["lat" "lng"]))})))
                           
         (assoc state
                ::map map
                ::boundary boundary
                ::lidar-coverage lidar-coverage)))
          
     :will-unmount
     (fn-js [state] (dissoc state ::map))
     
     :after-render
     (fn-js [state]
       (let [{[{[lat0 lat1 lon0 lon1] :map-position
                boundary-geojson :boundary-geojson
                lidar-coverage-geojson :lidar-coverage-geojson}] :rum/args
              ^js/L.LayerGroup boundary ::boundary
              ^js/L.LayerGroup lidar-coverage ::lidar-coverage
              map ::map} state]

         (when-not (nil? lat0)
           (.invalidateSize map)
           (.fitBounds map (js/L.latLngBounds #js [lat0 lon0] #js [lat1 lon1])))
         (.clearLayers boundary)
         (.clearLayers lidar-coverage)
         (when boundary-geojson
           (.addData boundary (clj->js boundary-geojson)))
         (when lidar-coverage-geojson
           (.addData lidar-coverage (clj->js lidar-coverage-geojson))))
       state)}
  
  rum/static
  [{boundary-geojson :boundary-geojson
    on-draw-box :on-draw-box
    map-position :map-position}]
  
  [:div {:style {:width :100% :height :500px}
         :ref :container}])

#?(:cljs
   (defn query-nominatim [query results]
     (GET "https://nominatim.openstreetmap.org/search"
         {:handler #(reset! results %)
          :params {:format :json
                   :q @query
                   :featuretype "settlement"
                   :polygon_geojson 1}
          :format :text})))

(defn- file-extension [filename]
  (.substring filename (.lastIndexOf filename \.)))

(defn- file-non-extension [filename]
  (.substring filename 0 (.lastIndexOf filename \.)))

(defn- as-int [value]
  (cond
    (number? value) (int value)
    (string? value) #?(:cljs (js/parseInt value)
                       :clj (Integer/parseInt value))))

(rum/defcs nominatim-searchbox <
  rum/static
  (rum/local [] ::results)
  (rum/local "" ::query)
  (rum/local nil ::debounce)
  (rum/local nil ::boundary)
  
  [{results ::results
    debounce ::debounce
    query ::query :as state}
   {on-select :on-select}
   ]

  [:div.search
   [:input {:type :search
            :value @query
            :placeholder "Search..."
            :on-change
            (fn-js [e]
              (reset! query (.. e -target -value))

              (when-let [debounce @debounce]
                (js/clearTimeout debounce))
              
              (if (string/blank? @query)
                (reset! results [])
                (reset! debounce
                        (js/setTimeout #(query-nominatim query results) 500))))}]
   (when-let [rs (seq @results)]
     [:div.results
      (for [result rs]
        [:div.result {:key (get result "osm_id")
                      :on-click #(do (on-select result)
                                     (reset! results [])
                                     (reset! query ""))}
         [:b (get result "display_name")]
         [:div
          (get result "class") " - " (get result "type") " - " (get result "osm_type")
          ]])])])


(rum/defcs drag-drop-box <
  rum/static
  (rum/local 0 ::drag-counter)
  [{drag-counter ::drag-counter} {on-files :on-files}]
  (let [on-files (fn [file-list]
                   (when on-files
                     (when-let [files (for [i (range (.-length file-list))]
                                        (.item file-list i))]
                       (on-files files))))]
    
    [:div
     {:class ["drop-target" (when (pos? @drag-counter) "dragging")]
      :style {:height :100px :display :flex
              :flex-direction :column}
      :on-drag-enter
      (fn-js [e]
        (.preventDefault e)
        (.stopPropagation e)
        (swap! drag-counter inc))
      
      :on-drag-leave
      (fn-js [e]
        (.preventDefault e)
        (.stopPropagation e)
        (swap! drag-counter dec))
      
      :on-drag-over (fn-js [e]
                      (doto e
                        (.preventDefault)
                        (.stopPropagation)))
      :on-drop
      (fn-js [e]
        (.preventDefault e)
        (.stopPropagation e)
        (on-files (.. e -dataTransfer -files))
        
        (reset! drag-counter 0))}
     
     [:span {:style {:margin :auto :text-align :center}}
      [:label
       [:input {:on-change #(on-files (.. % -target -files))
                :value ""
                :style {:width 0 :height 0}
                :type :file :multiple :multiple}]
       "Choose files to upload"] ", or drag files here"]]))

(def validate-and-upload
  (fn-js [{files :files ext :extensions} status]
    (let [ext (set (map string/lower-case ext))]
      (cond
        (not (or (ext ".shp")
                 (ext ".json")
                 (ext ".geojson")
                 (ext ".gpkg")
                 (ext ".geopackage")))
        (swap! status
               assoc
               :state :invalid
               :message "Unsupported file type")

        (and (ext ".shp")
             (not (and (ext ".shx")
                       (ext ".dbf")
                       (ext ".prj"))))
        (swap! status
               assoc
               :state :invalid
               :message "Missing component of shapefile")

        :else
        (let [form-data (js/FormData.)]
          
          (doseq [file files]
            (.append form-data
                     "file" file (.-name file)))
          
          ;; upload all the files
          (POST "add-file"
              {:body form-data
               :response-format :transit
               :handler
               (fn [{:keys [fields-by-type] :as x}]
                 (swap! status
                        merge
                        (assoc x :state :uploaded)))
               
               :error-handler
               (fn [x]
                 (swap! status
                        assoc
                        :state :error
                        :message (str "Error uploading file: "
                                      (:status-text x)
                                      " "
                                      (:status x))))
               :progress-handler
               (fn [x]
                 (let [loaded (.-loaded x)
                       total (.-total x)
                       progress (/ (* 100 loaded) total)]
                   (swap! status
                          assoc
                          :state :uploading
                          :progress progress)))}))
        ))))

(defn progress-bar-background [state progress]
  (case state
    :ready :white
    :uploading
    (str "linear-gradient(90deg ,#87ceeb "
         progress "%,#eee " progress "%)")
    :error "#f44"
    :invalid "#f44"
    :uploaded "#00ff7f"))

(def building-geometry-type #{:polygon :point :multi-polygon})
(def road-geometry-type #{:line-string :multi-line-string})

(defn building-count [file]
  (reduce + 0 (vals (select-keys (:count-by-type file) building-geometry-type))))

(defn road-count [file]
  (reduce + 0 (vals (select-keys (:count-by-type file) road-geometry-type))))


(rum/defc file-uploader <
  rum/static
  rum/reactive
  [_ files]
  [:div
   (drag-drop-box
    {:on-files
     (fn [selected]
       (let [selected
             (for [f selected]
               {:name (.-name f)
                :base-name (file-non-extension (.-name f))
                :extension (file-extension (.-name f))
                :file f})
             selected (group-by :base-name selected)
             selected (into {}
                            (for [[n group] selected]
                              [n
                               {:base-name n
                                :files (map :file group)
                                :extensions (map :extension group)
                                :state :ready}]))]

         (swap! files merge selected)

         (doseq [[i f] @files]
           (when (= :ready (:state f))
               (let [status (rum/cursor-in files [i])]
                 (validate-and-upload f status))))
         ))})
   
   (for [[i file] (rum/react files)]
     (let [{progress :progress
            message :message
            state :state
            base-name :base-name
            extensions :extensions} file]
       
       [:div {:key i
              :style
              {:font-size :1.2em
               :border-radius :8px
               :background
               (progress-bar-background state progress)
               :padding :0.1em
               :margin :0.5em}}
        [:div.flex-cols base-name (interpose ", " extensions)
         (when (= :uploaded state)
           (let [b (building-count file)
                 r (road-count file)]
             [:span {:style {:margin-left :auto} } (str b) " buildings, " (str r) " roads"]))
         
         [:span {:style {:margin-left :auto}} " "
          (if (= :uploading state)
            (spinner {:size 16})
            [:button
             {:style {:border-radius :16px}
              :on-click #(swap! files dissoc i)}
             symbols/delete])
          ]]
        (when (or (= :error state)
                  (= :invalid state))
          [:div message])
        ]))])

(rum/defcs geometry-data-page <
  rum/static
  rum/reactive

  (rum/local nil ::map-position)
  (rum/local nil ::boundary-geojson)
  
  [{map-position ::map-position
    boundary-geojson ::boundary-geojson}

   form-state]
  
  (let [*osm-roads   (rum/cursor-in form-state [:roads :include-osm])
        *data-source (rum/cursor-in form-state  [:geometry :source])
        *data-files (rum/cursor-in form-state   [:geometry :files])
        *osm-position (rum/cursor-in form-state [:geometry :osm])
        *map-position (rum/cursor-in form-state [:geometry :map-position])
        *centroid (rum/cursor-in form-state [:geometry :centroid])
        
        data (rum/react *data-source)
        data-files (rum/react *data-files)
        osm-roads (rum/react *osm-roads)
        ]
    [:div
     [:h1 "Buildings and roads"]
     [:p
        "Heat demands and supplies are associated with buildings in the map. "
        "You can acquire building data from OpenStreetMap, or you can upload your own GIS data."]

     [:p "Potential heat pipe routes are associated with roads and paths in the map. "
        "You can acquire roads and paths from OpenStreetMap, or you can upload your own GIS data."]

     [:div.card
      [:label [:input {:name :building-source :type :radio :value :osm
                       :checked (= :osm data)
                       :on-click #(reset! *data-source :osm)}]
       [:h1 "Use OpenStreetMap for buildings and roads"]]
      (when (= :osm data)
        [:div
         [:p "You can search for a named area in OpenStreetMap, or draw a box."]
         (nominatim-searchbox
          {:on-select (fn [place]
                        (reset! *map-position (get place "boundingbox"))
                        (when (#{"way" "relation"}
                               (get place "osm_type"))
                          (reset! *osm-position
                                  {:osm-id (str
                                            (get place "osm_type")
                                            ":"
                                            (get place "osm_id"))
                                   :boundary  (get place "geojson")})))})
         (osm-map-box {:map-position (rum/react *map-position)
                       :boundary-geojson (:boundary (rum/react *osm-position))
                       :on-draw-box #(do (reset! *osm-position {:boundary (:bounds %)})
                                         (reset! *centroid (:centroid %))
                                         (reset! *map-position nil))
                       :allow-drawing true})])]
     
     [:div.card
      [:label [:input {:name :building-source :type :radio :value :files
                       :checked (= :files data)
                       :on-click #(reset! *data-source :files)}]
       
       [:h1 "Upload GIS Files"]]
      (when (= :files data)
        [:div
         [:p "You can upload geometry in these formats:"]
         [:ul
          [:li [:a {:href "http://geojson.org/" :target "_blank"} "GeoJSON"]]
          [:li [:a {:href "https://en.wikipedia.org/wiki/Shapefile" :target "_blank"} "ESRI Shapefile"]
           " - don't forget to include the "
           [:b ".shp"] ", " [:b ".dbf"] ", "
           [:b ".shx"] ", " [:b ".prj"] " and "
           [:b ".cpg"] " files!"]]
         
         (file-uploader {} *data-files)

         [:div
          (let [has-roads (some pos? (map road-count (vals data-files)))]
            [:label [:input {:type :checkbox
                             :disabled (not has-roads)
                             :checked (or (not has-roads) osm-roads)
                             :on-change #(swap! *osm-roads not)}]
             "Also import roads from OpenStreetMap for the area covered by these files"])
          
          ]])]
     ]))

(rum/defc field-help-page [fields]
  [:div {:on-click (fn-js [] (close-dialog!))}
   [:h1 "Field help - click to close"]
   [:dl {:style {:max-height :80% :overflow-y :scroll}}
    (for [f fields]
      (list
       [:dt {:key (:value f)} (:label f)]
       [:dd {:style {:max-width :30em}
             :key (str (:value f) "-")} (:doc f)]))]])

(rum/defc field-selection-page <
  rum/reactive rum/static
  [label fields options *field-selection]
  (let [field-selection (rum/react *field-selection)]
    [:div
     [:div.flex-cols
      [:h1 (str "Allocate fields for " label " ")
       [:button
        {:on-click
         (fn-js []
           (show-dialog! (field-help-page options)))
         :style
         {:background :blue
          :border :none
          :text-align :center
          :padding 0
          :margin 0
          :color :white
          :border-radius :0.6em
          :width :1.2em
          :height :1.2em}}
        "?"]
       ]
      [:div {:style {:margin-left :auto}}
       [:button.button
        {:title "Control-click to guess harder"
         :on-click
         (fn [e]
           (let [default (and (.-ctrlKey e) :user-fields)
                 
                 option-names
                 (->>
                  (for [option options]
                    [(name (:value option)) (:value option)])
                  (into {}))

                 guess-option
                 (fn [field-name]
                   (or (-> field-name
                           (string/trim)
                           (string/lower-case)
                           (string/replace #"[^a-z0-9]+" "-")
                           (->> (get option-names)))
                       default))
                 
                 result
                 (reduce
                  (fn [a [file field]]
                    (if-let [option (guess-option field)]
                      (assoc-in a [file field] option)
                      a))
                  {} fields)
                 ]
             (swap! *field-selection
                    (fn [st] (merge-with (fn [a b] (merge b a)) st result)))
             ))
         }
        "Guess"]
       ]
      ]
     
     [:div.flex-grid
      (for [[file fields] (group-by first fields)]
        [:div.card {:key file}
         [:h1 file]
         [:table 
          [:thead [:tr [:th "Field"] [:th "Meaning "]]]
          [:tbody
           (for [[_ field] fields]
             [:tr
              {:key field}
              [:td field]
              [:td
               [:select {:value (name (get-in field-selection [file field] :none))
                         :on-change #(let [value (.. % -target -value)
                                           value (keyword value)]
                                       (if (= :none value)
                                         (swap! *field-selection
                                                update file
                                                dissoc field)
                                         (swap! *field-selection
                                                assoc-in [file field] value)))}
                [:option {:value "none"} "None"]
                (for [option options]
                  [:option {:key (:value option)
                            :value (name (:value option))}
                   (:label option)])]]])]]])
      ]]))

(defn- all-columns [list-of-files]
  (->> list-of-files (mapcat :keys) (into #{})))

(defn- mappable-columns [{files :files} geometry-types]
  (->>
   (for [[_ meta] files
         g geometry-types
         f (get (:fields-by-type meta) g)]
     [(:base-name meta) f])
   (into #{})
   (sort)))

(defn- next-page-map [form-state]
  (let [using-geometry-files (-> form-state :geometry :source (= :files))

        using-road-files (-> form-state :roads :source (= :files))

        building-cols        (-> form-state :geometry (mappable-columns building-geometry-type))
        road-cols            (-> form-state :geometry (mappable-columns road-geometry-type))
        
        result
        (cond->
            {:name :geometry
             :geometry (if (and using-geometry-files (seq building-cols))
                         :building-cols
                         :lidar)
             :lidar :other-parameters
            }
          (and using-geometry-files (seq building-cols) (seq road-cols))        (assoc :building-cols :road-cols)
          (and using-geometry-files (seq building-cols) (not (seq road-cols)))  (assoc :building-cols :lidar)
          (and using-geometry-files (seq road-cols))                            (assoc :road-cols :lidar))
        ]
    result))

(defn- too-large [boundary]
  (let [geo (or (get-in boundary ["geometry" "coordinates"])
                (get-in boundary ["coordinates"]))
        ring (first geo)
        xs (map first ring)
        ys (map second ring)
        minx (reduce min xs)
        maxx (reduce max xs)
        miny (reduce min ys)
        maxy (reduce max ys)
        dx (- maxx minx)
        dy (- maxy miny)]
    (or (> dx 0.15)
        (> dy 0.15))))

(defn- validate-osm-area [geo]
  (cond
    (and (= :osm (:source geo))
         (not (or (:osm-id (:osm geo))
                  (:boundary (:osm geo)))))
    ["Select an area on the map to continue"]

    ;; here we really want to know whether the region drawn is huge

    (and (= :osm (:source geo))
         (:boundary (:osm geo))
         (too-large (:boundary (:osm geo))))
    ["Select a smaller area on the map to continue"]))


(defn- validate-files [geo]
  (when (= :files (:source geo))
    (let [files (:files geo)]
      (cond
        (zero? (reduce + 0 (map building-count (vals files))))
        ["Upload some GIS files with polygons or multipolygons to continue"]

        (some (comp #{:invalid :error} :state) (vals files))
        ["Remove any invalid files or failed uploads to continue"]

        (some (comp #{:uploading} :state) (vals files))
        ["Waiting for files to upload..."]))))


(defn- page-requirements [form-state]
  (case (:current-page form-state)
    :name (when (string/blank? (:name form-state))
            ["Enter a name for your map"])
    :geometry (concat (validate-files    (:geometry form-state))
                      (validate-osm-area (:geometry form-state)))
    nil))

(rum/defc other-parameters-page < rum/reactive rum/static
  [*form-state]
  [:div
   [:h1 "Set other parameters"]

   [:div.flex-rows
    [:div.card.flex-grow
     [:h1 "Heating degree days"]
     [:div.flex-cols
      [:input.flex-grow {:type :number
                         :min 0 :max 10000
                         :value (:degree-days (rum/react *form-state))
                         :on-change #(swap! *form-state assoc :degree-days
                                            (as-int (.. % -target -value)))}] " °C × days"]
     [:p "The number of heating degree days per year in this location, relative to a 17° base temperature."]
     [:p "THERMOS attempts to calculate a default value for the heating degree days for the location of your map from " 
      [:a {:href "https://ec.europa.eu/eurostat/cache/metadata/en/nrg_chdd_esms.htm" :target "_blank"} "Eurostat"] 
      " heating degree day data."]
     (let [*hdd-from-server? (rum/cursor-in *form-state [:hdd-from-server?])]
       (when-not @*hdd-from-server?
         [:p "The default value above is not from Eurostat data as your map is outside the coverage area. "
          "You may want to use "
          [:a {:href "https://www.degreedays.net" :target "_blank"} "degree-days.net"]
          " to generate a value."]))]

    [:div.card.flex-grow
     [:h1 "Automatic building groups"]
     [:p "For buildings that have not been placed in a group, THERMOS can assign a group using the road they are connected to."]
     (let [form-state (rum/react *form-state)
           geom-state (:geometry form-state)
           road-state (:roads form-state)]
       (if (-> geom-state :source (= :files))
         [:div.flex-cols
          [:label "Use field: "
           (let [road-fields (mappable-columns geom-state road-geometry-type)
                 nil-value "__nil__" ;; yuck, DOM strings.
                 seg-id "__geo-id__"
                 cur-value (:group-buildings road-state)
                 cur-value (case cur-value
                             nil nil-value
                             :geo-id seg-id
                             cur-value)
                 ]
             [:select {:value cur-value
                       :on-change
                       #(swap!
                         *form-state
                         assoc-in
                         [:roads :group-buildings]
                         (let [v (-> % .-target .-value)]
                           (cond
                             (= v nil-value) nil
                             (= v seg-id) :geo-id
                             :else v)))}
              [:option {:value nil-value} "None - do not group"]
              [:option {:value seg-id} "Road segment"]
              ;; this is a bit shonky as we could have 2 files
              ;; with differently named fields, or same named fields.
              (for [[field files]
                    (sort-by first (group-by second road-fields))]
                [:option {:value field} field
                 " (in "
                 (string/join ", " (map first files))
                 ")"])])]]
         [:div
          [:label [:input {:type :checkbox
                           :checked (= :geo-id (:group-buildings road-state))
                           :on-change
                           #(swap!
                             *form-state
                             assoc-in
                             [:roads :group-buildings]
                             (if (-> % .-target .-checked)
                               :geo-id nil))}]
           "Use road segment to group buildings"]]
         
         ))
     
     ]
    ]
   ])

(def road-fields
  [{:value :identity :label "Identity (text)"
    :doc
    [:span "An identifier - these are stored on the roads in the database and visible in downloaded GIS files."]}
   
   {:value :user-fields :label "User-defined field (any)"
    :doc
    [:span "Any other field you want to keep, like a classification, address, etc."]}
   ])

(def building-fields
  [{:value :annual-demand
    :label "Annual heat demand (kWh/yr)"
    :doc
    [:span
     "A value for annual demand will be used in preference to any other estimate. "
     "Otherwise, a benchmark estimate will be used if available, or the built-in regression model otherwise."]}

   {:value :maximum-annual-demand
    :label "Max. heat demand (kWh/yr)"
    :doc
    [:span "An upper bound to apply to the modelled annual heat demand. If the demand estimation model is used, and it produces an annual demand above this value, this value will be used instead."]}

   {:value :minimum-annual-demand
    :label "Min. heat demand (kWh/yr)"
    :doc
    [:span "A lower bound to apply to the modelled annual heat demand. If the demand estimation model is used, and it produces an annual demand below this value, this value will be used instead."]}

   {:value :annual-cooling-demand
    :label "Annual cooling demand (kWh/yr)"
    :doc
    [:span
     "A value for annual cooling demand will be used in preference to a cooling estimate."]}

   {:value :cooling-peak
    :label "Peak cooling demand (kW)"
    :doc
    [:span
     "A value for peak cooling demand will be used in preference to an estimate."]}
   
   {:value :peak-demand :label "Peak heat demand (kW)"
    :doc
    [:span
     "A value for peak demand will be used in preference to any other estimate. "
     "Otherwise, the peak/base ratio will be applied to the demand if available, or the build in regression model otherwise."]}
   
   {:value :height :label "Building height (m)"
    :doc
    [:span
     "A value for building height will be used in preference to any LIDAR data on the server."
     [:br]
     "Building height will improve the quality of any demand estimates produced from the built-in regression model."]}

   {:value :fallback-height :label "Fallback building height (m)"
    :doc
    [:span
     "A value for building height to be used if LIDAR data on the server and the building height field are missing."]}

   {:value :floor-area :label "Floor area (m2)"
    :doc
    [:span "A value for floor area will be used in benchmark-based estimates. "
     "If no value is provided, a value will be estimated from the building geometry and height (if known)."]}
   
   {:value :benchmark-c :label "Heat benchmark (kWh/yr)"
    :doc
    [:span
     "A constant benchmark - this is used in combination with the variable benchmark term. "
     "If a building has associated benchmarks and no specified demand, demand will be estimated as this constant plus floor area times the variable benchmark."]
    
    }
   {:value :benchmark-m :label "Heat benchmark (kWh/m2/yr)"
    :doc [:span "A variable benchmark per floor area."]}

   {:value :cooling-benchmark-c :label "Cooling benchmark (kWh/yr)"
    :doc
    [:span
     "A constant benchmark - this is used in combination with the variable benchmark term. "
     "If a building has associated benchmarks and no specified demand, demand will be estimated as this constant plus floor area times the variable benchmark."]
    
    }
   {:value :cooling-benchmark-m :label "Cooling benchmark (kWh/m2/yr)"
    :doc [:span "A variable benchmark per floor area."]}
   
   {:value :peak-base-ratio :label "Peak/base ratio"
    :doc
    [:span "If present, and no peak demand value is known, the peak demand will be estimated as the annual demand (converted into kW) multiplied with this factor."
     [:br]
     "Otherwise, a built in regression will be used."]}
   
   {:value :connection-count :label "Connection count"
    :doc
    [:span "The number of end-user connections the building contains. This affects only the application of diversity curves within the model."]}
   
   {:value :identity :label "Identity (text)"
    :doc
    [:span "An identifier - these are stored on the buildings in the database and visible in downloaded GIS files."]}

   {:value :user-fields :label "User-defined field (any)"
    :doc
    [:span "Any other field you want to keep, like a classification, address, etc."]}
   
   {:value :residential :label "Residential (logical)"
    :doc
    [:span "A logical value, represented either as a boolean column, or the text values yes, no, true, false, 1 or 0. "
     "If available, this will improve the quality of built-in regression model results. Otherwise, this is assumed to be true."]}

   {:value :group :label "Building group (any value)"
    :doc
    [:span "Any buildings which have the same value in this field (except null or an empty string) will be grouped together. "
     "Buildings in a group must all be connected to a network at once, or not at all."]}
   ])


(defn centroid 
  "Get the centroid of the map area, either from the osm boundary box or
   as the average centroid of all the uploaded files' centroids."
  [*form-state]
  (let [data-source (get-in @*form-state [:geometry :source])]
    (case data-source
      :osm (get-in @*form-state [:geometry :centroid])
      :files (let [centroids (->> (vals (get-in @*form-state [:geometry :files]))
                                  (map (fn [file] (:centroid file)))
                                  (filter (fn [c] (not (nil? c)))))]
               (merge-with / 
                           (apply merge-with + centroids) 
                           {:lat (count centroids) :lng (count centroids)})))))

(defn extent-geojson
  "Get the extent of the map area, either from the osm boundary box or
   as the extent of all the uploaded files' extents."
  [*form-state]
  (let [data-source (get-in @*form-state [:geometry :source])]
    (case data-source
      :osm (get-in @*form-state [:geometry :osm :boundary])
      :files (let [extents (->> (vals (get-in @*form-state [:geometry :files]))
                                (map (fn [file] (:extent file)))
                                (filter (fn [c] (not (nil? c)))))
                   x1 (apply min (map :x1 extents))
                   y1 (apply min (map :y1 extents))
                   x2 (apply max (map :x2 extents))
                   y2 (apply max (map :y2 extents))]
               {:type "Feature"
                :properties {:bounds {:x1 x1 :y1 y1 :x2 x2 :y2 y2}}
                :geometry {:type "Polygon"
                           :coordinates [[[x1 y1]
                                          [x2 y1]
                                          [x2 y2]
                                          [x1 y2]
                                          [x1 y1]]]}}))))

(def upload-lidar
  (fn-js [project-id *file-state *lidar-coverage-geojson]
         (let [file (:file @*file-state)
               ext (file-extension (.-name file))]
           (if (and (not= ext ".tif") (not= ext ".tiff"))
             (swap! *file-state
                    assoc
                    :state :invalid
                    :message "Unsupported file type")
             (let [form-data (js/FormData.)]
               (.append form-data "file" file (.-name file))

               (POST (str "/project/" project-id "/lidar/from-wizard")
                 {:body form-data
                  :response-format :transit
                  :handler
                  (fn [x] (swap! *file-state merge {:state :uploaded})
                          (reset! *lidar-coverage-geojson x))

                  :error-handler
                  (fn [x] (swap! *file-state assoc
                                 :state :error
                                 :message (str "Error uploading file: "
                                               (:status-text x)
                                               " "
                                               (:status x))))
                  :progress-handler
                  (fn [x] (let [loaded (.-loaded x)
                                total (.-total x)
                                progress (/ (* 100 loaded) total)]
                            (swap! *file-state
                                   assoc
                                   :state :uploading
                                   :progress progress)))}))))))

(rum/defc lidar-upload-state [i file-state]
  (let [{progress :progress
         message :message
         state :state
         file :file} file-state]

    [:div {:key i
           :style
           {:font-size :1.2em
            :border-radius :8px
            :background
            (progress-bar-background state progress)
            :padding "0.1em 0.5em"
            :margin :0.5em}}
     [:div.flex-cols {:style {:align-items :center}}
      (.-name file)
      [:span {:style {:margin-left :auto}} " "
       (when (= :uploading state)
         (spinner {:size 16}))]]
     (when (or (= :error state) (= :invalid state))
       [:div message])]))

(rum/defc lidar-page <
  rum/static
  rum/reactive

  [form-state]

  (let [extent (extent-geojson form-state)
        *lidar-uploaded (rum/cursor-in form-state [:lidar :files])
        *lidar-coverage-geojson (rum/cursor-in form-state [:lidar :coverage-geojson])]
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
      (osm-map-box {:map-position (let [{:keys [x1 y1 x2 y2]} (get-in extent [:properties :bounds])]
                                    [y1 y2 x1 x2])
                    :boundary-geojson extent
                    :allow-drawing false
                    :lidar-coverage-geojson (rum/react *lidar-coverage-geojson)})]

     [:h1 "Add more LIDAR"]
     [:p "Upload any other LIDAR tiles you want to use:"]
     [:div
      (drag-drop-box
       {:on-files
        (fn [selected]
          (let [selected (map (fn [file] {:file file :state :ready}) selected)]
            (swap! *lidar-uploaded #(apply conj %1 %2) selected)
            (doseq [[i f] (map-indexed vector @*lidar-uploaded)]
              (when (= (:state f) :ready)
                (let [file (rum/cursor-in *lidar-uploaded [i])]
                  (upload-lidar (:project-id @form-state) file *lidar-coverage-geojson))))))})

      (map-indexed (fn [i file-state] (lidar-upload-state i file-state))
           (rum/react *lidar-uploaded))]]))

(rum/defc button-strip < rum/reactive rum/static [*form-state *current-page]
  (let [state (rum/react *form-state)
        current-page (rum/react *current-page)
        messages (page-requirements state)]
    [:div
     (when (seq messages)
       [:ul
        (for [[i m] (map-indexed vector messages)]
          [:li {:key i} m])])

     (when-not (= :name current-page)
       [:button.button {:on-click
                        (fn [e]
                          (swap! *current-page (map-invert (next-page-map @*form-state))))} "Back"])
     (case current-page
       :other-parameters
       [:button.button {:disabled (not-empty messages)
                        :on-click
                        (fn [e]
                          (POST "../new" ;; urgh yuck
                            {:params
                             (->> @*form-state
                                    ;; :geometry :files ;; values need
                                    ;; their :files bit removing
                                    ;; because :files are not
                                    ;; encodable
                                  (setval [:geometry :files MAP-VALS :files] NONE))

                             :handler (fn-js [e]
                                             (js/window.location.replace "../.."))}))} "Create map"]
       :geometry
       [:button.button {:disabled (not-empty messages)
                        :on-click
                        (fn [e]
                          (GET (str "/project/" (:project-id @*form-state) "/lidar/coverage.json")
                            {:handler (fn [res]
                                        (reset! (rum/cursor-in *form-state [:lidar :coverage-geojson]) res)
                                        (swap! *current-page (next-page-map @*form-state)))
                             :error-handler #(swap! *current-page (next-page-map @*form-state)) }))} "Next"]

       :lidar
       [:button.button {:disabled (not-empty messages)
                        :on-click
                        (fn [e]
                          (GET "heat-degree-days"
                            {:params (centroid *form-state)
                             :handler (fn [res]
                                        (let [res-val (int res)
                                              use-val? (not= res-val -1)]
                                          (swap! *current-page (next-page-map @*form-state))
                                          (when use-val? (reset! (rum/cursor-in *form-state [:degree-days]) res-val))
                                          (reset! (rum/cursor-in *form-state [:hdd-from-server?]) use-val?)))
                             :error-handler #(swap! *current-page (next-page-map @*form-state))}))} "Next"]


       [:button.button {:disabled (not-empty messages)
                        :on-click
                        (fn [e]
                          (swap! *current-page (next-page-map @*form-state)))} "Next"])]))


(rum/defc map-creation-form < rum/reactive rum/static [form-state]
  (let [*current-page (rum/cursor-in form-state [:current-page])
        current-page  (rum/react *current-page)]
    (when (nil? current-page) (reset! *current-page :name))
    [:div.card
     (case (or current-page :name)
       :name (name-page form-state)
       :geometry (geometry-data-page form-state)

       :building-cols (field-selection-page
                       "buildings"
                       
                       (mappable-columns (:geometry @form-state) building-geometry-type)

                       building-fields
                       (rum/cursor-in form-state [:buildings :mapping]))
       
       :road-data (geometry-data-page :roads form-state)

       :road-cols
       
       (let [road-columns (mappable-columns (:geometry @form-state) road-geometry-type)
             ;; this is [[file column]]
             ]
         (field-selection-page
          "roads"
          road-columns
          road-fields
          (rum/cursor-in form-state [:roads :mapping])))

       :lidar (lidar-page form-state)
       :other-parameters (other-parameters-page form-state)

       ;; then we press go and wait ages

       [:div "Not sure what has happened here. "
        "There is no page called " (str current-page)])

     (button-strip form-state *current-page)
     #_ (dump form-state)

     ]))


(def start-state
  {:current-page :name
   :name ""
   :description ""
   :geometry {:source :osm :files {}}
   :roads    {:include-osm false :group-buildings nil}
   :degree-days 2000
   :hdd-from-server? false
   :project-id nil
   :lidar {:files [] :coverage-geojson nil}
   :default-fixed-civil-cost 350.0
   :default-variable-civil-cost 700.0})


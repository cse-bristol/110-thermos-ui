(ns thermos-pages.map-import-components
  (:require
   [thermos-pages.common :refer [fn-js] :refer-macros [fn-js]]
   
   [clojure.string :as string]
   [ajax.core :refer [GET POST]]
   [rum.core :as rum]
   [clojure.pprint :refer [pprint]]
   [clojure.set :refer [map-invert]]
   #?@(:cljs
       [[thermos-pages.dialog :refer [show-dialog! close-dialog!]]
        [cljsjs.leaflet]
        [cljsjs.leaflet-draw]])))

;; TODO make x button work on files uploaded
;; TODO make delete button work in joins list
;; TODO delete invalid joins
;; TODO tick-boxes for applying defaults in OSM?

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
     [:div
      [:label "Map name: "]
      [:input {:placeholder "Metropolis"
               :pattern ".+"
               :on-change #(reset! map-name (.. % -target -value))
               :value (or (rum/react map-name) "")}]]
     [:div
      [:label "Description: "]
      [:input {:placeholder "A map for planning heat networks in metropolis"
               :on-change #(reset! description (.. % -target -value))
               :value (or (rum/react description) "")}]]]))

(rum/defc osm-map-box
  < {:did-mount
     (fn-js [state]
       (let [{[{[lat0 lat1 lon0 lon1] :map-position
                boundary-geojson :boundary-geojson
                on-draw-box :on-draw-box}]
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

             draw-control (js/L.Control.Draw.
                           #js {"position" "topleft"
                                "draw" #js {"polyline" false
                                            "polygon" false
                                            "marker" false
                                            "circle" false
                                            "circlemarker" false}})
             ]

         (when-not (nil? lat0)
           (.invalidateSize map)
           (.fitBounds map (js/L.latLngBounds #js [lat0 lon0] #js [lat1 lon1])))
         
         (.addLayer map layer)
         (.addLayer map labels)
         (.addLayer map boundary)
         (.addControl map draw-control)

         (.on map (.. js/L.Draw -Event -CREATED)
              #(on-draw-box (js->clj (.. % -layer toGeoJSON))))
         
         (assoc state
                ::map map
                ::boundary boundary)))
          
     :will-unmount
     (fn-js [state] (dissoc state ::map))
     
     :after-render
     (fn-js [state]
       (let [{[{[lat0 lat1 lon0 lon1] :map-position
                boundary-geojson :boundary-geojson}] :rum/args
              boundary ::boundary
              map ::map} state]

         (when-not (nil? lat0)
           (.invalidateSize map)
           (.fitBounds map (js/L.latLngBounds #js [lat0 lon0] #js [lat1 lon1])))
         
         (.clearLayers boundary)
         (when boundary-geojson
           (.addData boundary (clj->js boundary-geojson))))
       state)}
  
  rum/static
  [{boundary-geojson :boundary-geojson
    on-draw-box :on-draw-box
    map-position :map-position}]
  
  [:div {:style {:width :100% :height :300px}
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
                 (ext ".geopackage")
                 (ext ".csv")
                 (ext ".tab")))
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
               (fn [x]
                 (let [geom-types (:geometry-types x)]
                   (if (and (not (nil? geom-types))
                            (not= (set geom-types) #{:polygon}))
                     (swap! status assoc
                            :state :invalid
                            :message (str "File contains unsupported geometry types: "
                                          (disj geom-types :polygon)))
                     (swap! status
                            merge
                            (assoc x :state :uploaded)
                            ))))
               
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

(rum/defc file-uploader <
  rum/static
  rum/reactive
  [files]
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
               :background
               (progress-bar-background state progress)
               :padding :0.1em
               :margin :0.5em}}
        [:div.flex-cols base-name (interpose ", " extensions)
         [:span {:style {:margin-left :auto}} " ✗"]]
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

   category
   form-state]
  
  (let [*data-source (rum/cursor-in form-state [category :source])
        *data-files (rum/cursor-in form-state [category :files])
        *osm-position (rum/cursor-in form-state [category :osm])
        *map-position (rum/cursor-in form-state [category :map-position])
        
        data (rum/react *data-source)
        category-name (case category
                      :buildings "building"
                      :roads "road")]
    [:div
     [:h1 "Choose " category-name " data"]
     (case category
       :buildings
       [:p
        "Heat demands and supplies are associated with buildings in the map. "
        "You can acquire building data from OpenStreetMap, or you can upload your own GIS data."]

       :roads
       [:p "Potential heat pipe routes are associated with roads and paths in the map. "
        "You can acquire roads and paths from OpenStreetMap, or you can upload your own GIS data."])

     [:div.card
      [:label [:input {:name :building-source :type :radio :value :osm
                       :checked (= :osm data)
                       :on-click #(reset! *data-source :osm)}]
       [:h1 "Use OpenStreetMap " (string/capitalize category-name) "s"]]
      (when (= :osm data)
        (case category
          :buildings
          [:div
           [:p "You can search for a named area in OpenStreetMap, or draw a box."]
           (nominatim-searchbox
            {:on-select (fn [place]
                          (reset! *map-position (get place "boundingbox"))
                          (when (#{"way" "relation"}
                                 (get place "osm_type"))
                            (reset! *osm-position
                                    {:osm-id (get place "osm_id")
                                     :boundary  (get place "geojson")})))})
           (osm-map-box {:map-position (rum/react *map-position)
                         :boundary-geojson (:boundary (rum/react *osm-position))
                         :on-draw-box #(do (reset! *osm-position {:boundary %})
                                           (reset! *map-position nil))})]
          
          :roads
          [:p "Roads will be loaded from OpenStreetMap in the same area as your buildings."]))]
     
     [:div.card
      [:label [:input {:name :building-source :type :radio :value :files
                       :checked (= :files data)
                       :on-click #(reset! *data-source :files)}]
       
       [:h1 "Upload GIS Files"]]
      (when (= :files data)
        [:div
         [:p "You can upload " category-name " geometry in these formats:"]
         [:ul
          [:li [:a {:href "http://geojson.org/" :target "_blank"} "GeoJSON"]]
          [:li [:a {:href "https://en.wikipedia.org/wiki/Shapefile" :target "_blank"} "ESRI Shapefile"]
           " - don't forget to include the "
           [:b ".shp"] ", " [:b ".dbf"] ", "
           [:b ".shx"] ", " [:b ".prj"] " and "
           [:b ".cpg"] " files!"]]
         [:p "The feature geometry must be " (case category
                                               :buildings [:b "POLYGON"]
                                               :roads [:span [:b "LINESTRING"] " or " [:b "MULTILINESTRING"]])
          " type geometry. "
          (when (= :buildings category) [:span"At the moment " [:b "MULTIPOLYGON"] " geometry is not supported."])]
         [:p "You can also upload " [:b "csv"] " and " [:b "tsv"] " files to relate to your GIS data."]
         
         (file-uploader *data-files)
         ])]
     ]))

(rum/defcs join-page <
  rum/reactive rum/static
  (rum/local {:gis-file nil :table-file nil :gis-column nil :table-column nil} ::bottom-row)
  [{*bottom-row ::bottom-row} *files *joins]
  
  (let [files (rum/react *files)
        joins (rum/react *joins)
        gis-files (vec (filter (comp seq :geometry-types) (vals files)))
        table-files (vec (filter #(and (not (seq (:geometry-types %)))
                                       (seq (:keys %))) (vals files)))
        bottom-row @*bottom-row]

    ;; This next is to ensure that the options in the bottom row are set
    ;; correctly. Unfortunately we end up running all this all the time :(
    (let [{gis-file   :gis-file   table-file :table-file
           gis-column :gis-column table-column :table-column} bottom-row
          
          change (cond-> {}
                   (nil? gis-file)
                   (assoc :gis-file (:base-name (first gis-files)))
                   (nil? table-file)
                   (assoc :table-file (:base-name (first table-files))))

          {new-gis-file :gis-file new-table-file :table-file} change
          
          gis-file   (files (or gis-file new-gis-file))
          table-file (files (or table-file new-table-file))
          
          change (cond-> change
                   (not (contains? (:keys gis-file) gis-column))
                   (assoc :gis-column (first (sort (:keys gis-file))))
                   (not (contains? (:keys table-file) table-column))
                   (assoc :table-column (first (sort (:keys table-file)))))]

      (when (seq change)
        (println "Fix bottom row" change)
        (swap! *bottom-row merge change)))
    
    [:div
     [:h1 "Join tabular data"]
     [:p "You have uploaded some tables along with your geometry."]
     [:p "If you want to use the columns from these tables, you need to relate (join) them to the geometry."]
     [:p "Only simple 1:1 "
      [:a {:href "https://en.wikipedia.org/wiki/Join_(SQL)"} "left joins"] " are supported. "
      "For more complex joins we suggest using "
      [:a {:href "https://www.gaia-gis.it/gaia-sins/"} "another" ] " "
      [:a {:href "https://www.qgis.org/"} "tool" ] "."]

     ;; for now only noddy joins
     (let [current-gis-file (files (:gis-file @*bottom-row))
           current-table-file (files (:table-file @*bottom-row))

           current-gis-cols (sort (:keys current-gis-file))
           current-table-cols (sort (:keys current-table-file))]
       
       [:table
        [:thead
         [:tr [:th "GIS file"] [:th "Table"] [:th "GIS column"] [:th "Table column"] [:th]]]
        [:tbody
         (for [[i join] (map-indexed vector joins)]
           [:tr {:key i}
            [:td (:gis-file join)]
            [:td (:table-file join)]
            [:td (:gis-column join)]
            [:td (:table-column join)]
            [:td [:button "🗑"]]])
         
         [:tr
          [:td
           [:select {:value (:gis-file @*bottom-row)
                     :on-change (fn-js [e]
                                  (swap! *bottom-row assoc
                                         :gis-file (.. e -target -value)))}
            (for [file gis-files]
              [:option {:key (:base-name file)
                        :value (:base-name file)}
               (:base-name file)])]]

          [:td
           [:select {:value (:table-file @*bottom-row)
                     :on-change (fn-js [e]
                                  (swap! *bottom-row assoc
                                         :table-file (.. e -target -value)))}
            (for [file table-files]
              [:option {:key (:base-name file)
                        :value (:base-name file)}
               (:base-name file)])]]

          [:td
           [:select {:value (:gis-column @*bottom-row)
                     :on-change #(swap! *bottom-row assoc
                                        :gis-column (.. % -target -value))}
            (for [col current-gis-cols]
              [:option {:key col :value col} col])]]
          

          [:td
           [:select {:value (:table-column @*bottom-row)
                     :on-change #(swap! *bottom-row assoc
                                        :table-column (.. % -target -value))}
            (for [col current-table-cols]
              [:option {:key col :value col} col])]]
          [:td [:button
                {:on-click
                 (fn-js []
                   (swap! *joins
                          #(into #{} (conj %1 %2))
                          @*bottom-row))}
                "+"]]]]])]))

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
  [fields options *field-selection]
  (let [field-selection *field-selection]
    [:div
     [:h1 "Allocate fields "
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
       "?"]]
     
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
               [:select {:value (name (get field-selection [file field] :none))
                         :on-change #(let [value (.. % -target -value)
                                           value (keyword value)]
                                       (if (= :none value)
                                         (swap! *field-selection
                                                dissoc [file field])
                                         (swap! *field-selection
                                                assoc [file field] value)))}
                [:option {:value "none"} "None"]
                (for [option options]
                  [:option {:key (:value option)
                            :value (name (:value option))}
                   (:label option)])]]])]]])
      ]]))

(defn- all-columns [list-of-files]
  (->> list-of-files (mapcat :keys) (into #{})))

(defn- mappable-columns [{files :files joins :joins}]
  (let [gis-files   (filter (comp seq :geometry-types) (vals files))
        table-files (remove (comp seq :geometry-types) (vals files))
        joined-tables (set (map :table-file joins))
        joined-tables (filter (comp joined-tables :base-name) table-files)]
    (into #{}
          (concat
           (for [f gis-files key (:keys f)] [(:base-name f) key])
           (for [f joined-tables key (:keys f)] [(:base-name f) key])))))

(defn- page-map [form-state]
  (let [using-building-files (-> form-state :buildings :source (= :files))
        has-extra-columns (-> form-state :buildings :files vals all-columns not-empty)
        has-joinable-files (and has-extra-columns
                                (->>
                                 form-state :buildings :files
                                 vals
                                 (mapcat :extensions)
                                 (some #{".csv" ".tab" ".tsv"})))
        result
        (cond->
            {:name :building-data
             :building-data (cond
                              (not using-building-files) :road-data
                              has-joinable-files :building-join
                              has-extra-columns :building-cols
                              :else :road-data)
             :road-data :other-parameters}
          
          has-joinable-files (assoc :building-join :building-cols)
          has-extra-columns (assoc :building-cols :road-data))
        ]
    (println result)
    result))

(rum/defc other-parameters-page < rum/reactive rum/static [*form-state]
  ;; TODO make this work
  [:div
   [:h1 "Other parameters"]

   [:div.flex-cols
    [:div.card.flex-grow
     [:h1 "Heating degree days"]
     [:div.flex-cols [:input.flex-grow {:type :number}] " °C"]
     [:p "The number of heating degree days per year in this location, relative to a 17° base temperature."]]
    

    [:div.card.flex-grow
     [:h1 "Default connection cost"]
     [:div.flex-cols [:input.flex-grow {:type :number}] " ¤"]
     [:p "The default cost of connecting a building to the network. This is the cost of work within the building, separate from the cost of pipes."]]]

   [:div.card
    [:h1 "Default pipe costs"]
    [:table
     [:thead
      [:tr [:th "Cost"] [:th "Fixed"] [:th "Variable"]]]
     [:tbody
      [:tr
       [:td "Mechanical"]
       [:td [:input {:type :number}]]
       [:td [:input {:type :number}]]
       ]
      [:tr [:td "Civil"]
       [:td [:input {:type :number}]]
       [:td [:input {:type :number}]]]]]]])

(def road-fields
  [{:value :fixed-cost
    :label "Fixed cost (¤/m)"
    :doc [:span "The fixed civil engineering cost per metre of road - this is fixed in the sense that it is unrelated to diameter, but can vary by road"]}
   {:value :mechanical-cost
    :label "Variable cost (¤/m/mm ^ 1.3)"
    :doc [:span "The variable civil engineering costs per metre of pipe."]}])

(def building-fields
  [{:value :annual-demand
    :label "Annual demand (kWh/yr)"
    :doc
    [:span
     "A value for annual demand will be used in preference to any other estimate. "
     "Otherwise, a benchmark estimate will be used if available, or the built-in regression model otherwise."]}
   
   {:value :peak-demand :label "Peak demand (kWh)"
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
   
   {:value :floor-area :label "Floor area (m2)"
    :doc
    [:span "A value for floor area will be used in benchmark-based estimates. "
     "If no value is provided, a value will be estimated from the building geometry and height (if known)."]}
   
   {:value :benchmark-c :label "Benchmark (kWh/yr)"
    :doc
    [:span
     "A constant benchmark - this is used in combination with the variable benchmark term. "
     "If a building has associated benchmarks and no specified demand, demand will be estimated as this constant plus floor area times the variable benchmark."]
    
    }
   {:value :benchmark-m :label "Benchmark (kWh/m2/yr)"
    :doc [:span "A variable benchmark per floor area."]}
   
   {:value :peak-base-ratio :label "Peak/base ratio"
    :doc
    [:span "If present, and no peak demand value is known, the peak demand will be estimated as the annual demand (converted into kW) multiplied with this factor."
     [:br]
     "Otherwise, a built in regression will be used."]}
   
   {:value :connection-count :label "Connection count"
    :doc
    [:span "The number of end-user connections the building contains. This affects only the application of diversity curves within the model."]}
   
   {:value :connection-cost :label "Connection cost (¤)"
    :doc
    [:span "A fixed cost for connecting this building - this is separate to the cost for the connecting pipe."]}
   
   {:value :identity :label "Identity (text)"
    :doc
    [:span "An identifier - these are stored on the buildings in the database and visible in downloaded GIS files."]}
   
   {:value :classification :label "Classification (text)"
    :doc
    [:span "Text describing the type of building; this can be whatever you want, and is visible in the network editor."]}
   
   {:value :residential :label "Residential (logical)"
    :doc
    [:span "A logical value, represented either as a boolean column, or the text values yes, no, true, false, 1 or 0. "
     "If available, this will improve the quality of built-in regression model results. Otherwise, this is assumed to be true."]}])

(rum/defc map-creation-form < rum/reactive rum/static [form-state]
  (let [*current-page (rum/cursor-in form-state [:current-page])
        current-page  (rum/react *current-page)]
    (when (nil? current-page) (reset! *current-page :name))
    [:div.card
     (case (or current-page :name)
       :name (name-page form-state)
       :building-data (geometry-data-page :buildings form-state)
       :building-join (join-page (rum/cursor-in form-state [:buildings :files])
                                 (rum/cursor-in form-state [:buildings :joins]))
       :building-cols (field-selection-page
                       (mappable-columns (:buildings @form-state))
                       building-fields
                       (rum/cursor-in form-state [:buildings :mapping]))

       
       :road-data (geometry-data-page :roads form-state)
       :road-join (join-page (rum/cursor-in form-state [:roads :files])
                             (rum/cursor-in form-state [:roads :joins]))
       :road-cols (field-selection-page
                   (mappable-columns (:roads @form-state))
                   road-fields
                   (rum/cursor-in form-state [:roads :mapping]))

       :other-parameters (other-parameters-page form-state)

       ;; next page needs to be about demand estimation
       ;; then a final page about paths and connectors

       ;; then we press go and wait ages

       [:div "Not sure what has happened here. "
        "There is no page called " (str current-page)])
     
     [:div
      (when-not (= :name current-page)
        [:button.button {:on-click
                         (fn [e]
                           (swap! *current-page (map-invert (page-map @form-state))))
                         } "Back"]
        )

      [:button.button {:on-click
                       (fn [e]
                         (swap! *current-page (page-map @form-state)))
                       } "Next"]

      ]

     
     
     (dump form-state)
     
     ]))

(def start-state
  {:page :name
   :buildings {:source :osm :files {}}
   :roads {:source :osm :files {}}
   :degree-days 2000
   })

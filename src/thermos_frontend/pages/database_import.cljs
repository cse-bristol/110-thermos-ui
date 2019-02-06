(ns thermos-frontend.pages.database-import
  (:require [reagent.core :as reagent]))

(enable-console-print!)

(defonce state
  (reagent/atom
   {:building-source :openstreetmap
    :road-source :openstreetmap
    :osm-area "rel:52189"
    :building-field-map []
    :road-field-map []
    }))

(defn- radio [{value-atom :value-atom value :value :as props}]
  [:input
   (assoc (dissoc props :value-atom)
          :type :radio
          :checked (= @value-atom value)
          :on-change #(reset! value-atom value))])

(def mappable-building-fields
  {:height "Height"
   :demand "Annual demand"
   :peak "Peak demand"
   :floor-area "Floor area"
   :classification "Classification"
   :orig-id "Identifier"
   :name "Name / address"
   :connection-count "Connection count"
   })

(def mappable-road-fields
  {:classification "Classification"
   :unit-cost "Cost / m"
   :orig-id "Identifier"
   :name "Name / address"})

(def field-documentation
  {:height "The building height in m - this will improve demand estimates based on shape."
   :demand "The annual demand in kWh - this will be used in preference to any other estimate."
   :peak "The peak demand in kW - this will be used in preference to any other estimate."
   :floor-area "The floor area in m2 - this will be used with benchmarks, in preference to a value determined from the shape."
   :classification "This is presented in the user interface to help select buildings. If using benchmarks, it is also available as a joining field for benchmarks."
   :orig-id "This is preserved internally to identify a particular object, but isn't that important."
   :name "This is shown in the user interface as the name of the road or building."
   :unit-cost "This is the unit cost of digging up a bit of road, for a small pipe."
   :connection-count "This is the number of consumers connected in this building, for diversity."
   })

(defn- field-map-list [value-atom mappable-fields]
  (println @value-atom)
  [:table
   [:thead
    [:tr
     [:th "Input field"]
     [:th "Target field"]
     [:th [:button
           {:type :button
            :on-click
            #(swap! value-atom
                    conj {:field-name ""
                          :field-target (first (keys mappable-fields))})}
           "+"]]]
    ]
   [:tbody
    (for [[i map] (map-indexed vector @value-atom)]
      [:tr {:key i}
       [:td
        [:input {:type :text :value (:field-name map)
                 :on-change #(swap! value-atom
                                    assoc-in [i :field-name]
                                    (.. % -target -value))
                 }]]
       [:td
        [:select {:title (field-documentation (:field-target map))
                  :on-change #(swap! value-atom
                                     assoc-in [i :field-target]
                                     (keyword (.. % -target -value)))
                  :value (:field-target map)}
         (for [[k v] mappable-fields]
           [:option {:key k :value (name k)} v])]]
       [:td [:button
             {:type :button
              :on-click
              #(swap! value-atom
                      (fn [es]
                        (vec
                         (keep-indexed
                          (fn [j e] (when-not (= j i) e))
                          es))))}
             "-"]]
       ])]])

(defn wizard []
  (reagent/with-let
    [building-source-atom (reagent/cursor state [:building-source])
     road-source-atom (reagent/cursor state [:road-source])
     building-field-map (reagent/cursor state [:building-field-map])
     road-field-map (reagent/cursor state [:road-field-map])
     ]
    
    (let [{building-source :building-source
           road-source :road-source
           
           osm-area :osm-area

           use-lidar :use-lidar
           lidar-wms-address :lidar-wms-address

           use-benchmarks :use-benchmarks}
          @state

          use-osm-buildings (= building-source :openstreetmap)
          use-osm-roads (= road-source :openstreetmap)
          ]
      [:div.container
       [:input {:type :hidden :name :osm-area :value osm-area}]
       [:div.card
        [:h1.s "Building shapes"]
        [:p "The database needs some shapes (polygons) for buildings. "
         "These can come from OpenStreetMap or from a GIS file."]

        ["div" {:style {:display :flex :flex-direction :row}}
         [:div {:style {:flex 1}}
          [:label
           [radio {:name :building-source :value :openstreetmap :value-atom building-source-atom}]
           [:h2 "OpenStreetMap area"]]
          " "
          [:input {:disabled (not use-osm-buildings)
                   :type :text
                   :value osm-area
                   :title "Can be an area name from nominatim, or rel:osm_id or way:osm_id"
                   :on-change #(swap! state assoc :osm-area (.. % -target -value))}]
          ]
         
         [:div {:style {:flex 1}}
          [:label
           [radio {:name :building-source :value :files :value-atom building-source-atom}]
           [:h2 "GIS files "]]
          " "
          [:input {:disabled use-osm-buildings
                   :type :file :name :building-gis-files :multiple true}]
          ]
         ]

        [:input {:name :building-field-map :type :hidden :value (str @building-field-map)}]
        (when-not use-osm-buildings
          [:div
           [:h1.s "Building field mapping"]
           [:p "If your input GIS file contains some useful information you can relate it here."]
           [field-map-list building-field-map mappable-building-fields]])
        
        [:h1.s "Demand estimation"]
        [:p
         "Buildings need an annual demand estimate and peak demand estimate to be used in the model."]

        [:p "For each building, demand estimates are produced using the following process:"]
        
        [:ol
         [:li "Find building height, if possible, using:"
          [:ul
           [:li "Any user-specified height fields"]
           [:li "LIDAR heightmap data, if available"]]]
         
         [:li "Find building floor area, using:"
          [:ul
           [:li "Any user-specified floor area fields"]
           [:li "The footprint area and height, if available"]
           [:li "The footprint area alone"]]]
         
         [:li "Determine annual demand, using:"
          [:ul
           [:li "Any user-specified demand fields"]
           [:li
            "Any benchmarks which join to the other fields on the building; "
            "the benchmarks are applied using the floor area"]

           [:li "A shape-based regression model using 3D information and degree days"]
           [:li "A shape-based regression model using 2D information and degree days, if height is not available"]]]

         [:li "Determine peak demand, using:"
          [:ul
           [:li "Any user-specified peak fields"]
           [:li "Any benchmarks which join to the other fields on the building; "
            "the benchmarks are applied using the floor area"]
           [:li "A built-in regression model based on annual demand"]]]]

        [:p
         "Benchmarks are applied by joining a benchmarks table to the buildings table. "
         "The default set of benchmarks is designed for use with OSM building classifications."]
        
        [:div {:style {:display :flex}}
         [:label {:style {:flex 1}}
          [:input {:type :checkbox :name :use-benchmarks
                   :value use-benchmarks
                   :on-change #(swap! state assoc :use-benchmarks (.. % -target -checked))
                   }]
          "Apply benchmark estimates "]
         [:label {:style {:flex 1}} "Custom benchmarks file: "
          [:input {:disabled (not use-benchmarks)
                   :multiple true
                   :type :file :name :benchmarks-file}]]
         ]
        
        [:p
         "Shape and benchmark predictions can be improved by providing information about building height. "
        "The application can derive heights from a LIDAR surface model. "
         "At the moment, the LIDAR data need to be manually put on the server."]
        
        [:div
         [:label 
          [:input {:type :checkbox :name :use-lidar
                   :value use-lidar
                   :on-change #(swap! state assoc :use-lidar (.. % -target -checked))}]
          "Derive height from LIDAR data"]]

        [:p
         "Shape based predictions are corrected for annual heating degree days, so we also need the heating degree days in your location. "
         "This should be relative to a 17° C base temperature."]
        
        [:div
         [:label
          "Annual heating degree days in location: "
          [:input {:type :number :name :degree-days
                   :min 0 :max 5000 :step 0 :default-value 2000}]]]
        
        [:h1.s "Paths and connectors"]
        [:p
         "To consider networks the model needs a set of candidate paths, which it will connect to buildings. "
         "These can come from OpenStreetMap or from a GIS file."]

        ["div" {:style {:display :flex :flex-direction :row}}
         [:div {:style {:flex 1}}
          [:label
           [radio {:name :road-source :value :openstreetmap :value-atom road-source-atom}]
           [:h2 "OpenStreetMap area"]]
          " "
          [:input {:disabled (not use-osm-roads)
                   :type :text
                   :value osm-area
                   :title "Can be an area name from nominatim, or rel:osm_id or way:osm_id"
                   :on-change #(swap! state assoc :osm-area (.. % -target -value))}]
          ]
         
         [:div {:style {:flex 1}}
          [:label
           [radio {:name :road-source :value :files :value-atom road-source-atom}]
           [:h2 "GIS files "]]
          " "
          [:input {:disabled use-osm-roads
                   :type :file :name :road-gis-files :multiple true}]
          ]
         ]
        [:input {:name :road-field-map :type :hidden :value (str @road-field-map)}]
        (when-not use-osm-roads
          [:div
           [:h1.s "Path field mapping"]
           [field-map-list road-field-map mappable-road-fields]])
        [:input {:type :submit :value "Import"}]]
       ]))
  )

(reagent/render
 [wizard]
 (js/document.getElementById "form"))

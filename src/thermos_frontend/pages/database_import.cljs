(ns thermos-frontend.pages.database-import
  (:require [reagent.core :as reagent]))

(enable-console-print!)

(defonce state
  (reagent/atom
   {:building-source :openstreetmap
    :road-source :openstreetmap
    :osm-area "rel:52189"
    :use-given-demand false
    }))

(defn- radio [{value-atom :value-atom value :value :as props}]
  [:input
   (assoc (dissoc props :value-atom)
          :type :radio
          :checked (= @value-atom value)
          :on-change #(reset! value-atom value))])

;; our form should render from our state
(defn wizard []
  
  (reagent/with-let
    [building-source-atom (reagent/cursor state [:building-source])
     road-source-atom (reagent/cursor state [:road-source])]
    
    (let [{building-source :building-source
           road-source :road-source
           
           osm-area :osm-area
           
           use-given-demand :use-given-demand
           use-given-peak :use-given-peak
           use-given-height :use-given-height

           given-demand-field :given-demand-field
           given-height-field :given-height-field
           given-peak-field :given-peak-field

           use-lidar :use-lidar
           lidar-wms-address :lidar-wms-address

           use-benchmarks :use-benchmarks
           }
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
        
        [:h1.s "Demand estimation"]
        [:p
         "Buildings need an annual demand estimate to be used in the model. "
         "These can be given in the input data, predicted from benchmarks, or predicted from building shape. "
         "If no other data are given, a prediction based on two-dimensional features of the building will be used."]
        [:div
         [:label {:title "Not available for data from OpenStreetMap"}
          [:input {:type :checkbox :name :use-given-demand
                   :disabled use-osm-buildings
                   :value use-given-demand
                   :on-change #(swap! state assoc :use-given-demand (.. % -target -checked))
                   }]
          "Use demand value from input data field "]
         [:input {:type :text :name :given-demand-field
                  :value given-demand-field
                  :disabled (or use-osm-buildings (not use-given-demand))
                  :on-change #(swap! state assoc :given-demand-field (.. % -target -value))}]]

        [:p
         "Benchmarks are applied by joining a benchmarks table to the buildings table. "
         "If a benchmark is available for a building, it will be used ahead of other predictions."
         "The default set of benchmarks is designed for use with harmonized OSM building classifications."]
        
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
         "Shape and benchmark predictions can be improved by providing information about building height. "]
        
        [:div
         [:label 
          [:input {:type :checkbox :name :use-given-height
                   :disabled use-osm-buildings
                   :value use-given-height
                   :on-change #(swap! state assoc :use-given-height (.. % -target -checked))
                   }]
          "Use height from input data field "]
         [:input {:type :text :name :given-height-field
                  :value given-height-field
                  :disabled (or use-osm-buildings (not use-given-height))
                  :on-change #(swap! state assoc :given-height-field (.. % -target -value))}]]

        [:p "The application can derive heights from a LIDAR surface model. "
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
                   :min 0 :max 5000 :step 0 :value 2000}]]]
        
        [:h1.s "Peak estimation"]
        [:p
         "Buildings need a peak demand estimate to be used in the model. "
         "If no input data is given, a prediction based on the annual demand will be used."]

        [:div
         [:label 
          [:input {:type :checkbox :name :use-given-peak
                   :disabled use-osm-buildings
                   :value use-given-peak
                   :on-change #(swap! state assoc :use-given-peak (.. % -target -checked))
                   }]
          "Use peak demand from input data field "]
         [:input {:type :text :name :given-peak-field
                  :value given-peak-field
                  :disabled (or use-osm-buildings (not use-given-peak))
                  :on-change #(swap! state assoc :given-peak-field (.. % -target -value))}]]
        
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
        [:input {:type :submit
                 :value "Import"
                 }]
        ]
       ]))
  )

(reagent/render
 [wizard]
 (js/document.getElementById "form"))

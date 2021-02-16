(ns thermos-frontend.pages.manage-lidar
  (:require [cljsjs.leaflet]
            [ajax.core :refer [GET]]))

(enable-console-print!)

(defn init-lidar-map []
  (let [map (js/L.map "manage-lidar-map" #js{:minZoom 2 :maxZoom 20
                                             :zoom 15 :center #js[51.553356 -0.109271]})

        layer (js/L.tileLayer
               "https:///stamen-tiles-{s}.a.ssl.fastly.net/toner-background/{z}/{x}/{y}.png"
               #js{:subdomains "abcd"
                   :minZoom 0 :maxZoom 20})
        labels (js/L.tileLayer
                "https://cartodb-basemaps-{s}.global.ssl.fastly.net/rastertiles/voyager_only_labels/{z}/{x}/{y}{r}.png"
                #js{:subdomains "abcd"
                    :minZoom 0 :maxZoom 20})]

    (.addLayer map layer)
    (.addLayer map labels)))

(defn show-lidar-on-map [map lidar-coverage-geojson]
  (let [lidar-coverage
        (js/L.geoJSON (clj->js lidar-coverage-geojson)
                      #js{:style (fn [_] #js {:color "#33ff88"})
                          :onEachFeature (fn [feature layer]
                                           (.bindTooltip layer
                                                         (.. feature -properties -filename)
                                                         #js{"direction" "center"}))})]
    
    (.addLayer map lidar-coverage)
    (.fitBounds map (.getBounds lidar-coverage))))

(defn load-lidar-coverage-geojson [map]
  (GET (str "coverage.json") {:handler (fn [res] (show-lidar-on-map map res))}))

(defn main []
  (let [map (init-lidar-map)]
    (load-lidar-coverage-geojson map)))

(main)

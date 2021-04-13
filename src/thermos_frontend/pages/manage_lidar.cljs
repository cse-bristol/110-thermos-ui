;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.pages.manage-lidar
  (:require [cljsjs.leaflet]
            [ajax.core :refer [GET]]
            [goog.object :as o]))

(enable-console-print!)

(defn init-lidar-map []
  (let [map (js/L.map "manage-lidar-map" #js{:minZoom 2 :maxZoom 20
                                             :zoom 15 :center #js[51.553356 -0.109271]})
        
        tile-layer (js/L.tileLayer
                    "https:///stamen-tiles-{s}.a.ssl.fastly.net/toner-background/{z}/{x}/{y}.png"
                    #js{:subdomains "abcd"
                        :minZoom 0 :maxZoom 20})
        labels (js/L.tileLayer
                "https://cartodb-basemaps-{s}.global.ssl.fastly.net/rastertiles/voyager_only_labels/{z}/{x}/{y}{r}.png"
                #js{:subdomains "abcd"
                    :minZoom 0 :maxZoom 20})]

    (.addLayer map tile-layer)
    (.addLayer map labels)))

(defn- feature-style [feature]
  (if (= "system" (o/getValueByKeys feature "properties" "source"))
    #js{:color "#DD0077"}
    #js{:color "#33ff88"}))

(defn- feature-tooltip [feature]
  (if (= "system" (o/getValueByKeys feature "properties" "source"))
    "system LIDAR"
    (o/getValueByKeys feature "properties" "filename")))

;; need the type hint below to make .bindTooltip exist when minified. Urgh.
(defn- add-feature-tooltip [feature ^js/L.LayerGroup layer]
  (.bindTooltip layer
                (feature-tooltip feature)
                #js{"direction" "center"}))

(defn show-lidar-on-map [^js/L.Map map lidar-coverage-geojson]
  (let [lidar-coverage
        (js/L.geoJSON (clj->js lidar-coverage-geojson)
                      #js {:style feature-style :onEachFeature add-feature-tooltip})]
    
    (.addLayer map lidar-coverage)
    (.fitBounds map (.getBounds lidar-coverage))))

(defn load-lidar-coverage-geojson [map]
  (GET (str "coverage.json") {:handler (fn [res] (show-lidar-on-map map res))}))

(defn main []
  (let [map ^js/L.Map (init-lidar-map)]
    (load-lidar-coverage-geojson map)))

(main)

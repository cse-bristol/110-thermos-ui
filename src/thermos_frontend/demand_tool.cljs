;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.demand-tool
  (:require [reagent.core :as reagent]
            [cljts.core :as jts]
            [thermos-frontend.tile :as tile]
            [thermos-frontend.spatial :as spatial]

            [thermos-frontend.editor-state :as editor-state]

            [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.cooling :as cooling]
            ))

(def start-state {::drawing false})

(defonce state
  (reagent/atom start-state))

(defn toggle!
  "Start drawing if not drawing. Stop drawing if drawing."
  []
  (swap! state
         (fn [s]
           (if (::drawing s)
             start-state
             (assoc start-state ::drawing true)))))

(defn create-control [leaflet-map]
  [:div.leaflet-control-group.leaflet-bar
   [:div
    [:button.leaflet-control-button
     {:title "Draw a new building"
      :style {:font-size "28px"
              :color
              (if (::drawing @state) "#00bfff")
              :border
              (if (::drawing @state) "#00bfff")}
      :on-click toggle!}
     "⊕"]]])


(defn is-drawing? [] (::drawing @state))

(defn draw-at! [point]
  (swap! state assoc ::geometry point))

(defn- rads [degs]
  (* degs (/ Math/PI 180.0)))

(defn- degs [rads]
  (* rads (/ 180.0 Math/PI)))

(defn- geo-buffer
  "Since point is in 4326, we can't just buffer it.
  Instead we need to do our own buffering. For this we need to have a
  way to go from point to another point at variable angle and constant
  radius.
  "
  [point]
  (let [point (.getCoordinate point)

        radius
        (for [phi (range 0 360 20)]
          (.getCoordinate (jts/geodesic-translation point 5 phi)))

        radius (conj radius (last radius))
        ]
    (jts/create-polygon radius)))

(defn mouse-clicked! []
  (let [geometry (::geometry @state)]
    (when geometry
      (let [geometry (geo-buffer geometry)

            candidate
            {::candidate/type :building
             ::candidate/user-fields {"Category" "User-created building"}
             ::candidate/id (jts/ghash geometry)
             ::candidate/geometry (jts/geom->json geometry)
             ::candidate/modified true
             ::candidate/selected true
             ::spatial/jsts-geometry geometry
             ::candidate/inclusion :optional
             ::demand/kwh 0
             ::demand/kwp 0
             ::cooling/kwh 0
             ::cooling/kwp 0
             }
            ]
        (editor-state/edit-geometry!
         editor-state/state
         (fn [doc]
           (document/add-candidate doc candidate)))))
    
    (reset! state start-state)))


(defn render-tile! [canvas coords layer]
  (let [{drawing ::drawing geometry ::geometry} @state
        ctx (.getContext canvas "2d")
        size (.getTileSize layer)
        width  (.-x size)
        height (.-y size)]
    (tile/fix-size canvas layer)
    (if (and drawing geometry)
      (let [project (tile/projection canvas layer)]
        (set! (.. ctx -lineWidth) 6)
        (set! (.. ctx -strokeStyle) "#00bfff")
        (.setLineDash ctx #js [5 3])
        
        (if geometry
          (tile/render-geometry
           geometry
           ctx project false false false)))
      
      (.clearRect ctx 0 0 width height))))


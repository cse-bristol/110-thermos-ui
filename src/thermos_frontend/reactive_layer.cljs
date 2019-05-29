(ns thermos-frontend.reactive-layer
  "Common code for creating a leaflet layer that is painted reactively with canvas."
  (:require [cljsjs.leaflet]
            [goog.object :as o]
            [reagent.core :as reagent]))

(defn create [& {:keys [paint-tile internal-id
                        constructor-args
                        ]
                 :or {constructor-args {}}}]
  (let [make-canvas
        (fn [coords]
          (this-as layer
            (let [canvas (js/document.createElement "canvas")
                  zoom (.-z coords)
                  map-control (o/get layer "_map")
                  size (.getTileSize layer)

                  north-west (.unproject map-control (.scaleBy coords size) zoom)
                  south-east (.unproject map-control (.scaleBy (.add coords (js/L.point 1 1)) size) zoom)

                  bbox {:minY (min (.-lat north-west) (.-lat south-east))
                        :maxY (max (.-lat north-west) (.-lat south-east))
                        :minX (min (.-lng north-west) (.-lng south-east))
                        :maxX (max (.-lng north-west) (.-lng south-east))}]

              (set! (.. canvas -coords) coords)

              (let [tracks (paint-tile canvas coords layer bbox)]
                (set! (.. canvas -tracks) tracks))

              canvas)))
        

        destroy-tile
        (fn [e]
          (doseq [t (-> e
                        (o/get "tile")
                        (o/get "tracks"))]
            (reagent/dispose! t)))

        initialize
        (fn [options]
          (this-as this
            (.call (.. js/L.GridLayer -prototype -initialize) this options)
            (.on this "tileunload" destroy-tile)))

        constructor (->> {:initialize initialize
                          :createTile make-canvas
                          :internalId internal-id}
                         (clj->js)
                         (.extend js/L.GridLayer))
        ]
    (constructor.
     (clj->js constructor-args)
     )))

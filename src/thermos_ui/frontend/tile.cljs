(ns thermos-ui.frontend.tile
  (:require [reagent.core :as reagent]
            [leaflet :as leaflet]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.editor-state :as state]

            [thermos-ui.specs.document :as document]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.frontend.spatial :as spatial]

            [thermos-ui.frontend.theme :as theme]

            [goog.object :as o]
            ))

(declare render-candidate render-geometry render-linestring)

;; most of the work is in here - how to paint an individual tile onto the map
(defn render-tile [contents tile map]
  "Draw a tile.
  `document` should be a document map (not an atom containing a document map),
  `tile` a canvas element,
  `map` an instance of our map component's layer class."
  (let [coords (.-coords tile)
        size (.getTileSize map)
        ctx (.getContext tile "2d")
        width (.-x size)
        height (.-y size)
        zoom (.-z coords)
        map-control (o/get map "_map")
        project (fn [x y]
                  (let [pt (.project map-control
                                     (js/L.latLng x y)
                                     zoom)
                        pt (.unscaleBy pt size)
                        pt (.subtract pt coords)
                        pt (.scaleBy pt size)
                        ]
                    pt
                    ;; [(.-x pt) (.-y pt)]
                    ))

        geometry-key
        (if (> zoom 15)
          ::spatial/jsts-geometry
          ::spatial/jsts-simple-geometry)
        ]

    (.setAttribute tile "width" width)
    (.setAttribute tile "height" height)

    (.clearRect ctx 0 0 width height)

    (doseq [candidate contents]
      (render-candidate candidate ctx project geometry-key))
    ))

(defn render-candidate
  "Draw a shape for the candidate on a map.
  `candidate` is a candidate map,
  `ctx` is a Canvas graphics context (2D)
  `project` is a function to project from real space into the canvas pixel space"
  [candidate ctx project geometry-key]

  (set! (.. ctx -lineWidth)
        (cond
          (::candidate/selected candidate) 4
          (= :path (::candidate/type candidate)) 1
          (not (= (::candidate/inclusion candidate) :forbidden)) 1.5
          true 1))
  (set! (.. ctx -strokeStyle) (case (::candidate/inclusion candidate)
                                :required theme/red
                                :optional theme/blue
                                theme/grey))
  (set! (.. ctx -fillStyle) theme/light-grey)

  (render-geometry (candidate geometry-key) ctx project
     true false)
  )

(defn render-geometry
  [geom ctx project fill? close?]

  (case (.getGeometryType geom)
    "Polygon"
    (do (.beginPath ctx)
      (render-linestring (.getExteriorRing geom) ctx project true)
      ;; draw the inner rings
      (dotimes [n (.getNumInteriorRing geom)]
               (render-linestring (.getInteriorRingN geom n) ctx project true))
      ;; draw the outline
      (when fill? (.fill ctx))
      (.stroke ctx))
    "LineString"
    (do (.beginPath ctx)
      (render-linestring geom ctx project false)
      (.stroke ctx))
    )
  )

(defn render-linestring [line-string ctx project close?]
  (-> line-string
      (.getCoordinates)
      (.forEach
       (fn [coord ix]
         (let [pt;; [x y]
               (project (.-y coord) (.-x coord))]
           (if (= 0 ix)
             (.moveTo ctx (.-x pt) (.-y pt))
             (.lineTo ctx (.-x pt) (.-y pt)))))))

  (when close? (.closePath ctx)))

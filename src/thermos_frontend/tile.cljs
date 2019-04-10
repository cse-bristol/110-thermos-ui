(ns thermos-frontend.tile
  (:require [reagent.core :as reagent]
            [leaflet :as leaflet]
            [thermos-frontend.operations :as operations]
            [thermos-frontend.editor-state :as state]

            [thermos-specs.document :as document]
            [thermos-specs.path :as path]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.solution :as solution]
            
            [thermos-frontend.spatial :as spatial]

            [thermos-frontend.theme :as theme]

            [goog.object :as o]
            ))

(declare render-candidate render-geometry render-linestring)

;; most of the work is in here - how to paint an individual tile onto the map
;; if there is a solution we probably want to show the solution cleanly
(defn render-tile [has-solution? contents tile map]
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
;        (if (> zoom 15)
        ::spatial/jsts-geometry
;          ::spatial/jsts-simple-geometry)
        ]

    (.setAttribute tile "width" width)
    (.setAttribute tile "height" height)

    (.clearRect ctx 0 0 width height)

    
    (let [paths (atom nil)]
      (doseq [candidate contents]
        (if (= :path (::candidate/type candidate))
          (swap! paths conj candidate)
          (render-candidate zoom has-solution? candidate ctx project geometry-key)))
      (doseq [path @paths]
        (render-candidate zoom has-solution? path ctx project geometry-key)))))

(defn render-candidate
  "Draw a shape for the candidate on a map.
  `candidate` is a candidate map,
  `ctx` is a Canvas graphics context (2D)
  `project` is a function to project from real space into the canvas pixel space"
  [zoom solution candidate ctx project geometry-key]

  (let [selected (::candidate/selected candidate)
        inclusion (::candidate/inclusion candidate)

        in-solution (candidate/in-solution? candidate)

        unreachable (candidate/unreachable? candidate)

        is-supply (candidate/has-supply? candidate)
        supply-in-solution (candidate/supply-in-solution? candidate)
        
        included (candidate/is-included? candidate)
        forbidden (not included)
        filtered (:filtered candidate)
        ]
    (set! (.. ctx -lineWidth)
          (+
           (if (> zoom 17) 0.5 0)
           (cond
             selected 4
             included 1.5
             true 1)))
    
    (set! (.. ctx -strokeStyle)
          (cond
            unreachable theme/magenta
            (and solution
                 (not in-solution)
                 (= inclusion :optional)) theme/cyan
            (= inclusion :required) theme/red
            (= inclusion :optional) theme/blue

            :otherwise theme/white))

    (set! (.. ctx -fillStyle)
          (if is-supply
            (.createPattern ctx
                            (cond
                              (and selected supply-in-solution) theme/blue-dark-grey-stripes
                              selected                          theme/white-dark-grey-stripes
                              supply-in-solution                theme/blue-light-grey-stripes
                              :else                             theme/white-light-grey-stripes)
                            "repeat")
            
            (if selected theme/dark-grey theme/light-grey)))

    (set! (.. ctx -globalAlpha)
          (if filtered 0.25 1))

    
    )

  (comment
    (when (candidate/is-path? candidate)
      (let [coords (.getCoordinates (candidate geometry-key))
            start (first coords)
            end (last coords)
            start (project (.-y start) (.-x start))
            end (project (.-y end) (.-x end))
            ]
        (set! (.. ctx -font) "10px Sans")
        (set! (.. ctx -fillStyle) "#000000")
        (.fillText ctx
                   (::path/start candidate)
                   (.-x start) (.-y start))
        (.fillText ctx
                   (::path/end candidate)
                   (.-x end) (.-y end))
        )
      ))
  
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

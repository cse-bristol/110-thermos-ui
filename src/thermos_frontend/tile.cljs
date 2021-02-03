(ns thermos-frontend.tile
  (:require [reagent.core :as reagent]
            [leaflet :as leaflet]
            [thermos-frontend.operations :as operations]
            [thermos-frontend.editor-state :as state]

            [thermos-specs.document :as document]
            [thermos-specs.path :as path]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.solution :as solution]
            [thermos-specs.view :as view]

            [thermos-frontend.spatial :as spatial]

            [thermos-frontend.theme :as theme]

            [goog.object :as o]))

(declare render-candidate render-candidate-shadow render-geometry render-linestring)

(defn projection
  "Create a projection function"
  [tile layer]

  (let [coords      (.-coords tile)
        size        (.getTileSize layer)
        zoom        (.-z coords)
        map-control (o/get layer "_map")]
    (fn [x y]
      (let [pt (.project map-control
                         (js/L.latLng x y)
                         zoom)
            pt (.unscaleBy pt size)
            pt (.subtract pt coords)
            pt (.scaleBy pt size)]
        pt))))

(defn fix-size [tile layer]
  (let [size (.getTileSize layer)
        width (.-x size)
        height (.-y size)]
    (.setAttribute tile "width" width)
    (.setAttribute tile "height" height)))

;; most of the work is in here - how to paint an individual tile onto the map
;; if there is a solution we probably want to show the solution cleanly
(defn render-tile [has-solution? contents tile layer map-view min-max-diameter]
  "Draw a tile.
  `document` should be a document layer (not an atom containing a document layer),
  `tile` a canvas element,
  `layer` an instance of our layer component's layer class,
  `map-view` is either ::view/constraints or ::view/solution,
  `min-diameter` and max are the min and max, used for a linear scaling"
  (let [coords (.-coords tile)
        size (.getTileSize layer)
        ctx (.getContext tile "2d")
        width (.-x size)
        height (.-y size)
        zoom (.-z coords)
        project (projection tile layer)
        geometry-key ::spatial/jsts-geometry]

    (fix-size tile layer)
    (.clearRect ctx 0 0 width height)

    ;; Render order: non-selected buildings, selected building shadows, selected buildings,
    ;;               non-selected paths, selected path shadows, selected paths
    (let [{buildings :building paths :path} (group-by ::candidate/type contents)
          {selected-buildings true non-selected-buildings false}
          (group-by
           (fn [x] (boolean (or (::candidate/selected x) (:in-selected-group x))))
           buildings)
          
          {selected-paths true non-selected-paths false} (group-by (comp boolean ::candidate/selected) paths)

          pipe-diam-line-width
          (or
           (and
            min-max-diameter
            (let [[min-diameter max-diameter] min-max-diameter]
              (and min-diameter max-diameter
                   (fn [path]
                     (if-let [diameter (::solution/diameter-mm path)]
                       (let [rel-diam (/ (- diameter min-diameter)
                                         (inc (- max-diameter min-diameter)))]
                         (+ 0.5 (* 10 (Math/sqrt rel-diam))))
                       nil)))))

           (constantly nil))

          ]

      ;; Non-selected buildings
      (doseq [candidate non-selected-buildings]
        (render-candidate zoom has-solution? candidate ctx project  map-view))
      ;; Selected building shadows
      (doseq [candidate selected-buildings]
        (render-candidate-shadow zoom has-solution? candidate ctx project  map-view))
      ;; Selected buildings
      (doseq [candidate selected-buildings]
        (render-candidate zoom has-solution? candidate ctx project map-view))

      (if (> zoom 14)
        (do
          ;; Non-selected paths
          (doseq [path non-selected-paths]
            (let [line-width (pipe-diam-line-width path)]
              (render-candidate zoom has-solution? path ctx project map-view line-width)))
          ;; Selected path shadows
          (doseq [path selected-paths]
            (let [line-width (pipe-diam-line-width path)]
              (render-candidate-shadow zoom has-solution? path ctx project  map-view line-width)))
          
          ;; Selected paths
          (doseq [path selected-paths]
            (let [line-width (pipe-diam-line-width path)]
              (render-candidate zoom has-solution? path ctx project map-view line-width))))
        
        (when has-solution?
          (doseq [path paths]
            (when (candidate/in-solution? path)
              (let [line-width (pipe-diam-line-width path)]
                (render-candidate zoom has-solution? path ctx project map-view line-width)))))
        ))
    ))

(defn render-candidate
  "Draw a shape for the candidate on a map.
  `candidate` is a candidate map,
  `ctx` is a Canvas graphics context (2D),
  `project` is a function to project from real space into the canvas pixel space,
  `map-view` is either ::view/constraints or ::view/solution,
  `line-width` is an optionally specified line width for when you want to represent the pipe diameter."
  [zoom solution candidate ctx project map-view line-width]

  (let [filtered          (:filtered candidate)
        in-selected-group (:in-selected-group candidate)]

    (set! (.. ctx -lineCap) "round")

    (case map-view
      ;; Colour by constraints
      ::view/constraints
      (let [selected  (::candidate/selected candidate)
            inclusion (::candidate/inclusion candidate)
            is-supply (candidate/has-supply? candidate)
            included  (candidate/is-included? candidate)
            forbidden (not included)]

        ;; Line width
        (set! (.. ctx -lineWidth)
              (if (> zoom 17) 1.5 1))

        ;; Line colour
        (set! (.. ctx -strokeStyle)
              (cond
                (= inclusion :required) theme/red
                (= inclusion :optional) theme/blue
                :else                   theme/white))

        ;; Fill
        (set! (.. ctx -fillStyle)
              (cond
                is-supply
                theme/supply-orange
                
                selected
                theme/dark-grey

                in-selected-group
                theme/white
                
                :else
                theme/light-grey
                )
              ))

      ;; Colour by results of the optimisation
      ::view/solution
      (let [selected           (::candidate/selected candidate)
            inclusion          (::candidate/inclusion candidate)
            in-solution        (candidate/in-solution? candidate)
            unreachable        (candidate/unreachable? candidate)
            alternative        (candidate/got-alternative? candidate)
            connected          (candidate/is-connected? candidate)
            supply-in-solution (candidate/supply-in-solution? candidate)]

        ;; Line width
        (set! (.. ctx -lineWidth)
              (+ (if (> zoom 17) 0.5 0)
                 (cond
                   line-width line-width
                   true       1)))

        ;; Line colour
        (set! (.. ctx -strokeStyle)
              (cond
                (= unreachable :peripheral)
                theme/peripheral-yellow

                unreachable
                theme/magenta
                
                (and solution (not connected) (not alternative) (= inclusion :optional))
                theme/beige
                
                alternative
                theme/green
                
                in-solution
                theme/in-solution-orange
                
                :else
                theme/white))

        ;; Fill
        (set! (.. ctx -fillStyle)
              (cond
                supply-in-solution
                theme/supply-orange
                
                selected
                theme/dark-grey

                in-selected-group
                theme/white
                
                :else
                theme/light-grey
                ))))

    (set! (.. ctx -globalAlpha)
          (if filtered 0.25 1)))

  (render-geometry (::spatial/jsts-geometry candidate) ctx project
                   true false false))

(defn render-candidate-shadow
  "Draw some kind of ephemeral outline for selected candidate.
  Arguments are the same as render-candidate."
  [zoom solution candidate ctx project map-view line-width]
  (set! (.. ctx -lineCap) "round")
  (set! (.. ctx -lineJoin) "round")
  (case map-view
    ;; Colour by constraints
    ::view/constraints
    (let [in-selected-group (:in-selected-group candidate)
          selected          (::candidate/selected candidate)
          inclusion         (::candidate/inclusion candidate)
          is-supply         (candidate/has-supply? candidate)
          included          (candidate/is-included? candidate)
          forbidden         (not included)]

      ;; Line width
      (set! (.. ctx -lineWidth)
            (+ (* 2 (- zoom 16)) 1))

      ;; Line colour
      (set! (.. ctx -strokeStyle)
            (cond
              in-selected-group       "#000000"
              (= inclusion :required) theme/red-light
              (= inclusion :optional) theme/blue-light
              :else                   theme/white)))

    ;; Colour by results of the optimisation
    ::view/solution
    (let [in-selected-group (:in-selected-group candidate)
          selected           (::candidate/selected candidate)
          inclusion          (::candidate/inclusion candidate)
          in-solution        (candidate/in-solution? candidate)
          unreachable        (candidate/unreachable? candidate)
          alternative        (candidate/got-alternative? candidate)
          connected          (candidate/is-connected? candidate)
          supply-in-solution (candidate/supply-in-solution? candidate)]

      ;; Line width
      (set! (.. ctx -lineWidth)
            (+ (* 2 (- zoom 16))
               (cond
                 line-width line-width
                 true       1)))

      ;; Line colour
      (set! (.. ctx -strokeStyle)
            (cond
              (= unreachable :peripheral)
              theme/peripheral-yellow-light

              unreachable
              theme/magenta-light

              (and solution (not connected) (not alternative) (= inclusion :optional))
              theme/beige-light

              alternative
              theme/green-light
              
              in-solution
              theme/in-solution-orange-light

              in-selected-group       "#000000"

              :else
              theme/white))))

  (set! (.. ctx -globalAlpha) 1)
  (render-geometry (::spatial/jsts-geometry candidate) ctx project
                   true false true))

(def point-radius
  "The screen-units radius for a Point geometry." 8.0)

(defn render-geometry
  [geom ctx project fill? close? shadow?]

  (case (.getGeometryType geom)
    "Polygon"
    (do (.beginPath ctx)
      (render-linestring (.getExteriorRing geom) ctx project true)
      ;; draw the inner rings
      (dotimes [n (.getNumInteriorRing geom)]
               (render-linestring (.getInteriorRingN geom n) ctx project true))
      ;; draw the outline
      (when fill? (.fill ctx))
      (if shadow?
        (.stroke ctx)
        (do
          (.save ctx)
          (.clip ctx)
          (set! (.. ctx -lineWidth)
            (* 2 (.. ctx -lineWidth)))
          (.stroke ctx)
          (.restore ctx))))

    "Point"
    (do (.beginPath ctx)
        (let [pt (project (.getY geom) (.getX geom))]
          (.arc ctx
                (.-x pt) (.-y pt)
                point-radius 0 (* 2 Math/PI)))
        (when fill? (.fill ctx))
        (.stroke ctx))

    "LineString"
    (do (.beginPath ctx)
      (render-linestring geom ctx project false)
      (.stroke ctx))

    ("MultiLinestring" "MultiPolygon" "MultiPoint")
    (dotimes [n (.getNumGeometries geom)]
      (render-geometry (.getGeometryN geom n)
                       ctx project fill? close?))))

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

(defn render-coordinate-seq [point-seq ctx project]
  (when (> (count point-seq) 1)
    (let [[coord & t] point-seq]
      (let [pt;; [x y]
            (project (.-y coord) (.-x coord))]
        (.moveTo ctx (.-x pt) (.-y pt)))
      (doseq [coord t]
        (let [pt;; [x y]
              (project (.-y coord) (.-x coord))]
          (.lineTo ctx (.-x pt) (.-y pt)))))))

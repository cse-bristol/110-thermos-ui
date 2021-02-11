(ns thermos-frontend.spatial
  (:require [clojure.set :as set]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.document :as document]
            [thermos-specs.view :as view]
            [thermos-frontend.operations :as operations]
            [cljsjs.rbush :as rbush]
            [cljsjs.jsts :as jsts]
            [cljts.core :as jts]
            [reagent.core :as reagent]
            [goog.object :as o]
            ))

(let [zoom-table
      (map-indexed vector
                   (into []
                         (map #(/ % 256)
                              [ 360 180 90 45 22.5 11.25 5.625 2.813 1.406 0.703
                               0.352 0.176 0.088 0.044 0.022 0.011 0.005 0.003 0.001 0.0005])))
      ;; how many degrees at is 1 pixel at each zoom, at the equator
      ]

  (defn- add-jsts-geometry
    "At the moment each candidate contains ::candidate/geometry, which is
  a geojson object, but we want a jsts thingy instead so we can do hit testing
  and find bounding boxes and so on.

  TODO we could convert the geojson to jsts in-place and then convert it back?
  "
    [candidate]
    (let [jsts-geom (jts/json->geom (::candidate/geometry candidate))
          
          envelope (.getEnvelopeInternal jsts-geom)

          min-x (.getMinX envelope)
          max-x (.getMaxX envelope)
          min-y (.getMinY envelope)
          max-y (.getMaxY envelope)
          
          bbox {:minX min-x
                :maxX max-x
                :minY min-y
                :maxY max-y}

          ;; how many degrees is the biggest extent
          ;; max-extent
          ;; (max (- max-x min-x)
          ;;                 (- max-y min-y))

          ]
      (assoc candidate
             ::jsts-geometry jsts-geom
;;             ::jsts-simple-geometry jsts-simple-geom
;;             ::minimum-zoom visible-zoom
             ::bbox bbox))))

(defn index-atom [document-atom]
  (reagent/track
   #(select-keys @document-atom [::spatial-index ::update-counter])))

(defn update-index
  "To perform spatial queries, we need to store some more stuff
  inside our document, and on its candidates.
  This function ensures that the extra spatial stuff is up-to-date.

  The arity-1 version checks a document against its own book-keeping
  data to make sure it's alright.

  The arity-3 version you can tell what needs updating, in case you
  know what you just added or deleted."
  ([document added removed]
   (let [document (document/map-candidates document add-jsts-geometry added)
         candidates-bboxes (::candidates-bboxes document)
         spatial-index (or (::spatial-index document) (rbush))
         indexed-candidates
         (-> (::indexed-candidates document)
             (set/union added)
             (set/difference removed)
             (set))

         candidates (::document/candidates document)

         ;; more horrible performance hackery
         added-candidates-bboxes
         (reduce
          (fn [a id]
            (let [bb (clj->js (::bbox (candidates id)))]
              (o/set bb "id" id)
              (assoc a id bb)))
          {}
          added)
         ]

     ;; put new items into the index based on their bounding boxes
     ;; they go in as JS objects, so we clj->js them
     (.load spatial-index (clj->js (vals added-candidates-bboxes)))

     ;; deindex things we can no longer see
     ;; these are JS objects rather than immutable maps, because
     ;; we converted them in a prior go above, so we need to use
     ;; .-id to acquire the ID property. Presume the order of arguments
     ;; is OK here, so we are passing a string to remove, and equality testing
     ;; that string against the ID property of each entry.
     (doseq [id removed]
       (.remove spatial-index (candidates-bboxes id)))

     ;; we have updated all our spatial book-keeping, tell the world.
     (assoc document
            ::update-counter (+ (::update-counter document)
                                (if (and (empty? added) (empty? removed)) 0 1))
            ::spatial-index spatial-index
            ::candidates-bboxes (merge added-candidates-bboxes
                                       (reduce dissoc candidates-bboxes removed))
            ::indexed-candidates indexed-candidates)))

  ([document]
   (let [all-candidates (set (keys (::document/candidates document)))
         indexed-candidates (::indexed-candidates document)]
     (if (= all-candidates indexed-candidates)
       document
       (update-index document
                     (set/difference all-candidates indexed-candidates)
                     (set/difference indexed-candidates all-candidates))))))

(defn find-candidates-ids-in-bbox
  [doc bbox]
  (let [spatial-index (::spatial-index doc)]
    (if (nil? spatial-index) []
        ;; this is a bit hacky to make some javascript interact faster.
        (js->clj
         (.map (.search spatial-index (clj->js bbox)) #(o/get % "id" nil))))))

(defn find-intersecting-candidates
  "Given `doc`, a document, and `shape`, a shape, and possibly having
a spatial index `index` (recommended), find the candidate IDs of candidates
that intersect the shape"
  [doc shape]

  (->> (::document/candidates doc)
       (vals)
       (filter #(let [geom (::jsts-geometry %)]
                  (case (.getGeometryType geom)
                    "Point"
                    (.intersects shape (.buffer geom 0.000025 6))
                    
                    (.intersects shape geom))
                  ))))

(defn find-intersecting-candidates-ids
  "Given `doc`, a document, and `shape`, a shape, and possibly having
a spatial index `index` (recommended), find the candidate IDs of candidates
that intersect the shape"
  [doc shape]

  (->> (find-intersecting-candidates doc shape)
       (map ::candidate/id)))

(defn select-intersecting-candidates
  "Select the candidates in `doc` that intersect `shape` using
the selection `mode` (passed on to operations/select) and maybe the index"
  [doc shape mode]
  (let [candidates-in-shape (find-intersecting-candidates-ids doc shape)]
    (operations/select-candidates doc candidates-in-shape mode)))


;; TODO: when we move the map, we need to load & unload candidates
;; that have become visible or gone offscreen, but not candidates that
;; we care about.

;; (we care about candidates which are ::selected or ::inclusion /= :forbidden)

;; so

;; we need a function like

(defn merge-new-candidates
  "Given `new-candidates` which have appeared because we moved, put them into the document
  unless they are already there"
  [doc new-candidates]

  )

(defn forget-invisible-candidates
  "Forget about any candidates in `shape` which we aren't interested in due to
  not being in the problem or selected."
  [doc shape]
  )

(defn zoom-to-selection [doc]
  (let [selected-candidates (operations/selected-candidates doc)
        selected-candidates (if (empty? selected-candidates)
                              (operations/constrained-candidates doc)
                              selected-candidates)]
    (if (empty? selected-candidates)
      doc
      (let [minX (apply min (map #(:minX (::bbox %)) selected-candidates))
            maxX (apply max (map #(:maxX (::bbox %)) selected-candidates))
            minY (apply min (map #(:minY (::bbox %)) selected-candidates))
            maxY (apply max (map #(:maxY (::bbox %)) selected-candidates))
            new-bbox {:north maxY
                      :south minY
                      :east maxX
                      :west minX}
            ]
        (operations/move-map doc new-bbox)
        ))))




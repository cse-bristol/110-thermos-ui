(ns thermos-ui.frontend.spatial-operations
  (:require [clojure.set :as set]
            [thermos-ui.frontend.operations :as operations]
            [rbush :as rbush]
            ["jsts/dist/jsts" :as jsts]
            ))

(let [geometry-factory (jsts/geom.GeometryFactory.)
      geometry-reader (jsts/io.GeoJSONReader. geometry-factory)]

  (defn- add-jsts-geometry
    "At the moment each candidate contains ::candidate/geometry, which is
  a geojson object, but we want a jsts thingy instead so we can do hit testing
  and find bounding boxes and so on.

  TODO we could convert the geojson to jsts in-place and then convert it back?
  "
    [candidate]

    (let [jsts-geom (.read geometry-reader (::candidate/geometry candidate))
          envelope (.getEnvelopeInternal jsts-geom)
          bbox {:minX (.getMinX bounding-box)
                :maxX (.getMaxX bounding-box)
                :minY (.getMinY bounding-box)
                :maxY (.getMaxY bounding-box)}
          ]
      (assoc candidate
             ::jsts-geometry jsts-geom
             ::bbox bbox))))

(defn update-index
  "To perform spatial queries, we need to store some more stuff
  inside our document, and on its candidates.
  This function ensures that the extra spatial stuff is up-to-date.

  The arity-1 version checks a document against its own book-keeping
  data to make sure it's alright.

  The arity-3 version you can tell what needs updating, in case you
  know what you just added or deleted."
  ([document added removed]
   (let [document (operations/map-candidates add-jsts-geometry added)
         spatial-index (::spatial-index document)
         indexed-candidates
         (-> (::indexed-candidates document)
             (set/union added)
             (set/difference removed)
             (set))
         ]

     ;; put new items into the index based on their bounding boxes
     ;; they go in as JS objects, so we clj->js them
     (.load spatial-index
            (clj->js
             (for [{id ::candidate/id bbox ::bbox}
                   (map (::document/candidates document) added)]
               (assoc bbox :id id))))

     ;; deindex things we can no longer see
     ;; these are JS objects rather than immutable maps, because
     ;; we converted them in a prior go above, so we need to use
     ;; .-id to acquire the ID property. Presume the order of arguments
     ;; is OK here, so we are passing a string to remove, and equality testing
     ;; that string against the ID property of each entry.
     (doseq [id removed]
       (.remove spatial-index
                id
                (fn [a b] (= a (.-id b)))))

     ;; we have updated all our spatial book-keeping, tell the world.
     (assoc document
            ::spatial-index spatial-index
            ::indexed-candidates indexed-candidates)))

  ([document]
   (let [all-candidates (set (keys ::document/candidates document))
         indexed-candidates (::indexed-candidates document)]
     (if (= all-candidates indexed-candidates)
       document
       (update-index document
                     (set/difference all-candidates indexed-candidates)
                     (set/difference indexed-candidates all-candidates))))))

(defn find-intersecting-candidates-ids
  "Given `doc`, a document, and `shape`, a shape, and possibly having
a spatial index `index` (recommended), find the candidate IDs of candidates
that intersect the shape"
  [doc shape]

  (->> (::document/candidates doc)
       (vals)
       (filter #(.intersects shape (::jsts-geometry %)))
       (map ::candidate/id)))

(defn select-intersecting-candidates
  "Select the candidates in `doc` that intersect `shape` using
the selection `mode` (passed on to operations/select) and maybe the index"
  [doc shape mode]
  (let [candidates-in-shape (find-intersecting-candidates-ids doc shape)]
    (operations/select-candidates doc candidates-in-shape mode)))

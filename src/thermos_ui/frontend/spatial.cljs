(ns thermos-ui.frontend.spatial
  (:require [clojure.set :as set]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.specs.document :as document]
            [thermos-ui.frontend.operations :as operations]
            [cljsjs.rbush :as rbush]
            [cljsjs.jsts :as jsts]
            [reagent.core :as reagent]
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
    (let [json-geom (::candidate/geometry candidate)
          jsts-geom (.read geometry-reader json-geom)
          envelope (.getEnvelopeInternal jsts-geom)
          bbox {:minX (.getMinX envelope)
                :maxX (.getMaxX envelope)
                :minY (.getMinY envelope)
                :maxY (.getMaxY envelope)}
          ]
      (assoc candidate
             ::jsts-geometry jsts-geom
             ::bbox bbox))))

(defn index-atom [document-atom]
  (reagent/track
   #(select-keys @document-atom [::spatial-index])))

(defn update-index
  "To perform spatial queries, we need to store some more stuff
  inside our document, and on its candidates.
  This function ensures that the extra spatial stuff is up-to-date.

  The arity-1 version checks a document against its own book-keeping
  data to make sure it's alright.

  The arity-3 version you can tell what needs updating, in case you
  know what you just added or deleted."
  ([document added removed]
   (let [document (operations/map-candidates document add-jsts-geometry added)
         spatial-index (or (::spatial-index document) (rbush))
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
   (let [all-candidates (set (keys (::document/candidates document)))
         indexed-candidates (::indexed-candidates document)]
     (if (= all-candidates indexed-candidates)
       document
       (update-index document
                     (set/difference all-candidates indexed-candidates)
                     (set/difference indexed-candidates all-candidates))))))

(defn find-candidates-ids-in-bbox
  [doc bbox]
  (let [spatial-index (::spatial-index doc)
        matches (if (nil? spatial-index) [] (js->clj (.search spatial-index (clj->js bbox)) :keywordize-keys true))]
    (map :id matches)))

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

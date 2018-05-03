(ns thermos-ui.frontend.map
  (:require [clojure.set :as set]

            [reagent.core :as reagent]
            [cljsjs.leaflet]
            [cljsjs.leaflet-draw] ;; this modifies js/L.Control in-place
            [cljsjs.jsts :as jsts]

            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.spatial :as spatial]
            [thermos-ui.frontend.editor-state :as state]
            [thermos-ui.frontend.tile :as tile]
            [thermos-ui.frontend.popover :as popover]
            [thermos-ui.frontend.popover-menu :as popover-menu]
            [thermos-ui.frontend.search-box :as search-box]
            [thermos-ui.frontend.hide-forbidden-control :as hide-forbidden-control]
            [thermos-ui.specs.document :as document]
            [thermos-ui.specs.view :as view]
            [thermos-ui.specs.candidate :as candidate]

            [goog.object :as o]
            [goog.ui.SplitPane :refer [Component]]
            [goog.events :refer [listen]]
            [goog.functions :refer [debounce]]
            ))

;; the map component

(declare mount
         unmount
         candidates-layer
         layers-control
         render-tile
         draw-control
         layer->jsts-shape
         latlng->jsts-shape
         on-right-click-on-map
         create-leaflet-control
         unload-candidates)

(defn component
  "Draw a cartographic map for the given `document`, which should be a reagent atom
  containing a document map"
  [document]
  (let [watches (atom nil)
        map-node (atom nil)
        ]
    (reagent/create-class
     {:reagent-render (fn [document]
                        [:div.map {:ref (partial reset! map-node)}])
      :component-did-mount    (partial mount document watches map-node)
      :component-will-unmount (partial unmount watches)
      })))

(def esri-sat-imagery
  (js/L.tileLayer
      "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
    (clj->js {})))

(defn mount
  "Make a leaflet control when this component is being put on screen"
  [document watches map-node component]
  (let [map-node @map-node

        edit!  (fn [f & a] (apply state/edit! document f a))
        track! (fn [f & a] (swap! watches conj (apply reagent/track! f a)))

        map (js/L.map map-node (clj->js {:preferCanvas true
                                         :fadeAnimation true
                                         :zoom 15
                                         :center [51.553356 -0.109271]
                                         }))
        candidates-layer (candidates-layer document)

        ;; The tilesize of 256 is related to the x, y values when talking
        ;; to the server for tile data, so if the tilesize changes, effectively
        ;; the meaning of the zoom, or equivalently the x and y changes too.
        ;; Halve this => increase zoom by 1 in queries below.
        candidates-layer (candidates-layer. (clj->js {:tileSize 256}))

        draw-control (draw-control {:position :topleft
                                    :draw {:polyline false
                                           :polygon true
                                           :marker false
                                           :circlemarker false
                                           :circle false
                                           }})

        layers-control (layers-control
                        {"Satellite" esri-sat-imagery
                         "None" (js/L.tileLayer "")
                         }
                        {"Candidates" candidates-layer})

        search-control (create-leaflet-control search-box/component)

        hide-forbidden-control (create-leaflet-control hide-forbidden-control/component)

        follow-map!
        #(let [bounds (.getBounds map)]
           (edit! operations/move-map
                  {:north (.getNorth bounds)
                   :south (.getSouth bounds)
                   :west (.getWest bounds)
                   :east (.getEast bounds)}))

        map-bounding-box (reagent/cursor document [::view/view-state ::view/bounding-box])

        show-bounding-box!
        #(let [{n :north s :south
                w :west e :east} @map-bounding-box]
           (when (and n s w e)
             (.fitBounds map (js/L.latLngBounds (clj->js [ [s w] [n e] ])))))

        repaint! #(.repaintInPlace candidates-layer)

        pixel-size
        (fn []
          (let [zoom (.getZoom map)
                zz (.unproject map (js/L.point 0 0) zoom)
                oo (.unproject map (js/L.point 1 1) zoom)]
            (Math/sqrt
             (+ (Math/pow (- (.-lat zz) (.-lat oo)) 2)
                (Math/pow (- (.-lng zz) (.-lng oo)) 2)))
            ))
        ]

    (.addLayer map esri-sat-imagery)
    (.addLayer map candidates-layer)
    (.addControl map (search-control. (clj->js {:position :topright})))
    (.addControl map layers-control)
    (.addControl map draw-control)
    (.addControl map (hide-forbidden-control. (clj->js {:position :topleft})))

    (.on map "moveend" follow-map!)
    (.on map "zoomend" follow-map!)


    ;; Event handling for selection

    (let [is-drawing (atom false)
          method (atom :replace)]
      (.on map (.. js/L.Draw -Event -DRAWSTART)
           #(reset! is-drawing true))

      (.on map (.. js/L.Draw -Event -DRAWSTOP)
           #(reset! is-drawing false))

      (.on map (.. js/L.Draw -Event -CREATED)
           (fn [e]
             (state/edit! document
                          spatial/select-intersecting-candidates
                          (layer->jsts-shape (o/get e "layer"))
                          @method)
             (reset! method :replace)))

      (.on map "click"
           (fn [e]
             (let [oe (o/get e "originalEvent")]
               (reset! method

                       (cond
                         (o/get oe "ctrlKey" false) :xor
                         (o/get oe "shiftKey" false) :union
                         :otherwise :replace))
               (when-not @is-drawing
                 (state/edit! document
                              spatial/select-intersecting-candidates

                              (latlng->jsts-shape
                               (o/get e "latlng")
                               (* 3 (pixel-size)))

                              @method))))
           )

      (.on map "contextmenu" (fn [e] (on-right-click-on-map e document pixel-size)))
      )

    ;; When zooming or moving, unload the candidates which fall out of the current view
    (let [;; We'll use this timeout to wait a bit before doing the unloading.
          ;; But if you move, then move again before the candidates get unloaded,
          ;; the timeout will get reset and the candidates won't get unloaded unnecessarily.
          ;; @TODO Decide what the timeout should be - for now I've settled on 400ms but this is fairly arbitrary
          timeout (atom nil)]
      (.on map "movestart" (fn [] (if @timeout (js/clearTimeout @timeout))))
      (.on map "zoomstart" (fn [] (if @timeout (js/clearTimeout @timeout))))
      (.on map "moveend" (fn []
                           (if @timeout (js/clearTimeout @timeout))
                           (reset! timeout
                                   (js/setTimeout
                                    (fn [] (unload-candidates))
                                    400))))
      (.on map "zoomend" (fn []
                           (if @timeout (js/clearTimeout @timeout))
                           (reset! timeout
                                   (js/setTimeout
                                    (fn [] (unload-candidates))
                                    400))))
      )

    (track! show-bounding-box!)

    ;; When you move the splitpane, re-centre the map.
    ;; This also ensures that when you increase the size of the map pane it renders any newly exposed tiles.
    (js/setTimeout
     (fn []
       (let [splitpane (get-in @document [::view/view-state ::view/splitpane])]
         (goog.events/listen
          splitpane
          goog.ui.Component.EventType.CHANGE
          (debounce (fn [] (.invalidateSize map)) 200)
          ))))

    )
  )

(defn unmount
  "Destroy a leaflet when its react component is going away"
  [watches component]
  (doseq [watch @watches] (reagent/dispose! watch)))

(defn candidates-layer
  "Create a leaflet layer class which renders the candidates from the document.

  There is a mismatch here between the OO style in leaflet and the functional
  style in react & clojure"
  [doc]
  (let [;; this is a set of all the tiles which are visible
        tiles (atom #{})
        tile-id (atom 0)

        ;; tiles are just canvas DOM elements
        make-tile
        (fn [coords layer]
          (let [canvas (js/document.createElement "canvas")]
            (swap! tiles conj canvas)
            (let [
                  zoom (.-z coords)

                  tile-id (str "T" (swap! tile-id inc) "N")

                  map-control (o/get layer "_map")
                  size (.getTileSize layer)

                  north-west (.unproject map-control (.scaleBy coords size) zoom)
                  south-east (.unproject map-control (.scaleBy (.add coords (js/L.point 1 1)) size) zoom)

                  bbox {:minY (min (.-lat north-west) (.-lat south-east))
                        :maxY (max (.-lat north-west) (.-lat south-east))
                        :minX (min (.-lng north-west) (.-lng south-east))
                        :maxX (max (.-lng north-west) (.-lng south-east))
                        }

                  ;; Queries the spatial index in the document
                  ;; to update the candidates ids. This uses a cursor
                  ;; into the document to get the spatial index, so that
                  ;; it only gets triggered when the spatial index is changed.
                  just-index (spatial/index-atom doc)

                  ;; contains just the IDs of candidates in this box
                  tile-candidates-ids
                  (reagent/track
                   #(spatial/find-candidates-ids-in-bbox @just-index bbox))

                  ;; this is a cursor into the document, used so that we only trigger
                  ;; update-tile-contents on changes to the candidate set
                  just-candidates
                  (reagent/cursor doc [::document/candidates])

                  ;; If we are not showing forbidden candidates, filter them out
                  filter-function (if (not= (operations/showing-forbidden? @doc) false)
                                    identity
                                    #(and % (not= (::candidate/inclusion %) :forbidden)))

                  tile-contents
                  (reagent/track
                   #(filter filter-function (map @just-candidates @tile-candidates-ids)))
                  ]

              ;; If we are not showing forbidden candidates,
              ;; we don't want to bother rendering the tile if there is nothing in it
              (when (or (not= (operations/showing-forbidden? @doc) false)
                        (not-empty @tile-contents))

                (set! (.. canvas -coords) coords)
                (set! (.. canvas -tile-id) tile-id)
                (o/set canvas "tracks"
                       (list (reagent/track!
                              (fn []
                                (tile/render-tile @tile-contents canvas layer)
                                ;; identify tiles with big red text
                                ;; (let [ctx (.getContext canvas "2d")]
                                ;;   (set! (.. ctx -font) "40px Sans")
                                ;;   (set! (.. ctx -fillStyle) "#ff0000")
                                ;;   (.fillText ctx (str tile-id) 0 40)
                                ;;   )

                                ))))
                ;; also request load of the tile into the document
                (state/load-tile! doc (.-x coords) (.-y coords) (.-z coords))))
            canvas))

        create-tile (fn [coords] (this-as layer (make-tile coords layer)))

        destroy-tile
        (fn [e]
          (swap! tiles disj (.. e -tile))
          (doseq [t (-> e (o/get "tile")
                        (o/get "tracks"))]

            (reagent/dispose! t)))

        initialize
        (fn [options]
          (this-as this
            (.call (.. js/L.GridLayer -prototype -initialize) this options)
            (.on this "tileunload" destroy-tile)))

        ;; when the map is redrawn, we re-render each tile
        ;; that is on-screen
        repaint
        (fn []
          (this-as layer
                   (let [doc @doc]
                     (doseq [visible-tile (filter identity @tiles)]
                       (make-tile (.-coords visible-tile) layer)
                       )))
          )

        ]
    ;; create a leaflet class with these functions
    (->>
     {:initialize initialize
      :createTile create-tile
      :repaintInPlace repaint
      :internalId "candidates-layer"}
     (clj->js)
     (.extend js/L.GridLayer))))

(defn- layers-control [choices extras]
  "Create a leaflet control to choose which layers are displayed on the map.
  `choices` is a list of layers of which only one may be selected (radios).
  `extras` is a list of layers of which any number may be selected (checkboxes)."
  (js/L.control.layers
   (clj->js choices)
   (clj->js extras)
   #js{:collapsed false}))

(defn- draw-control [args]
  (js/L.Control.Draw. (clj->js args)))

(let [geometry-factory (jsts/geom.GeometryFactory.)
      jsts-reader (jsts/io.GeoJSONReader. geometry-factory)
      ]
  (defn latlng->jsts-shape [ll rad]
    (let [c (jsts/geom.Coordinate. (.-lng ll) (.-lat ll))
          p (.createPoint geometry-factory c)]
      (.buffer p (* 3 rad))))

  (defn layer->jsts-shape [layer]
    (-> (.read jsts-reader (.toGeoJSON layer))
        (o/get "geometry"))))

(defn on-right-click-on-map
  "Callback for when you right-click on the map.
  If you are clicking on a selected candidate, open up a popover menu
  allowing you to edit the selected candidates in situ."
  [e document pixel-size]
  (let [oe (o/get e "originalEvent")
        click-range (latlng->jsts-shape
                     (o/get e "latlng")
                     (* 3 (pixel-size)))
        intersecting-candidates-ids (set (spatial/find-intersecting-candidates-ids @document click-range))
        selected-candidates-ids (operations/selected-candidates-ids @document)
        intersecting-selected-candidates-ids (set/intersection intersecting-candidates-ids selected-candidates-ids)
        ]
    ;; If there are any selected candidates in the range of your click then show a
    ;; menu allowing you to edit the selected candidates in situ.
    (if (> (count intersecting-selected-candidates-ids) 0)
      (let [selected-candidates (operations/selected-candidates @document)
            selected-paths (filter
                            (fn [candidate] (= (::candidate/type candidate) :path))
                            selected-candidates)
            selected-paths-ids (map ::candidate/id selected-paths)
            selected-buildings (filter
                                (fn [candidate] (or (= (::candidate/type candidate) :demand)
                                                    (= (::candidate/type candidate) :supply)))
                                selected-candidates)
            selected-buildings-ids (map ::candidate/id selected-buildings)
            set-inclusion (fn [candidates-ids inclusion-value] (state/edit! document
                                                                            operations/set-candidates-inclusion
                                                                            candidates-ids
                                                                            inclusion-value))
            popover-menu-content [{:value [:div.centre "Edit candidates"]
                                   :key "title"}]
            popover-menu-content (if (not-empty selected-paths)
                                   (conj popover-menu-content
                                         {:value [:b (str (count selected-paths) " roads selected")]
                                          :key "selected-roads-header"}
                                         {:value "Set inclusion"
                                          :key "inclusion-roads"
                                          :sub-menu [{:value "Required"
                                                      :key "required"
                                                      :on-select (fn [e] (set-inclusion selected-paths-ids :required)
                                                                   (state/edit! document operations/close-popover))}
                                                     {:value "Optional"
                                                      :key "optional"
                                                      :on-select (fn [e] (set-inclusion selected-paths-ids :optional)
                                                                   (state/edit! document operations/close-popover))}
                                                     {:value "Forbidden"
                                                      :key "forbidden"
                                                      :on-select (fn [e] (set-inclusion selected-paths-ids :forbidden)
                                                                   (state/edit! document operations/close-popover))}]}
                                         {:value "Set road type [TODO]"
                                          :key "road-type"
                                          :sub-menu [{:value "Cheap"
                                                      :key "cheap"}
                                                     {:value "Expensive"
                                                      :key "expensive"}]})
                                   popover-menu-content)
            popover-menu-content (if (and (not-empty selected-paths) (not-empty selected-buildings))
                                   (conj popover-menu-content {:value [:div.popover-menu__divider]
                                                               :key "divider"})
                                   popover-menu-content)
            popover-menu-content (if (not-empty selected-buildings)
                                   (conj popover-menu-content
                                         {:value [:b (str (count selected-buildings) " buildings selected")]
                                          :key "selected-buildings-header"}
                                         {:value "Set inclusion"
                                          :key "inclusion-buildings"
                                          :sub-menu [{:value "Required"
                                                      :key "required"
                                                      :on-select (fn [e] (set-inclusion selected-buildings-ids :required)
                                                                   (state/edit! document operations/close-popover))}
                                                     {:value "Optional"
                                                      :key "optional"
                                                      :on-select (fn [e] (set-inclusion selected-buildings-ids :optional)
                                                                   (state/edit! document operations/close-popover))}
                                                     {:value "Forbidden"
                                                      :key "forbidden"
                                                      :on-select (fn [e] (set-inclusion selected-buildings-ids :forbidden)
                                                                   (state/edit! document operations/close-popover))}]}
                                         {:value "Set type [TODO]"
                                          :key "type"
                                          :sub-menu [{:value "Demand"
                                                      :key "demand"}
                                                     {:value "Supply"
                                                      :key "supply"}]})
                                   popover-menu-content)]
        (state/edit!
         document
         operations/set-popover-content
         [popover-menu/component popover-menu-content])
        (state/edit! document operations/set-popover-source-coords
                     [(o/get oe "clientX") (o/get oe "clientY")])
        (state/edit! document operations/show-popover)))
    ))

(defn create-leaflet-control
  [component & args]
  (->> {:onAdd (fn [map]
                 (let [box (.create js/L.DomUtil "div")]
                   (.disableClickPropagation js/L.DomEvent box)
                   (reagent/render (into [component map] args) box)
                   box))
        }
       clj->js
       (.extend js/L.Control)))

(defn unload-candidates
  []
  (let [;; Get all the candidates in the bbox
        bbox (get-in @state/state [::view/view-state ::view/bounding-box])
        ;; Use the height and width of the box to naively add a buffer to the bbox
        bbox-height (- (:north bbox) (:south bbox))
        bbox-width (- (:east bbox) (:west bbox))
        bbox {:minY (- (:south bbox) (/ bbox-height 2))
              :maxY (+ (:north bbox) (/ bbox-height 2))
              :minX (- (:west bbox) (/ bbox-width 2))
              :maxX (+ (:east bbox) (/ bbox-width 2))}
        candidates-in-bbox-ids (spatial/find-candidates-ids-in-bbox @state/state bbox)
        ;; Get all the candidates that are either selected or constrained
        selected-candidates-ids (operations/selected-candidates-ids @state/state)
        constrained-candidates-ids (operations/constrained-candidates-ids @state/state)
        candidates-to-keep-ids (clojure.core/set (concat candidates-in-bbox-ids
                                          selected-candidates-ids
                                          constrained-candidates-ids))
        candidates-to-keep (select-keys (::document/candidates @state/state)
                                        candidates-to-keep-ids)
        ]
    ;; Remove all the candidates that we don't want to keep
    (state/edit-geometry! state/state assoc ::document/candidates candidates-to-keep)
    ))

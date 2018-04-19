(ns thermos-ui.frontend.map
  (:require [reagent.core :as reagent]
            [leaflet :as leaflet]
            [clojure.set :as set]
            [cljsjs.leaflet-draw] ;; this modifies leaflet/Control in-place
            [cljsjs.jsts :as jsts]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.spatial :as spatial]
            [thermos-ui.frontend.editor-state :as state]
            [thermos-ui.frontend.tile :as tile]
            [thermos-ui.frontend.popover :as popover]
            [thermos-ui.frontend.popover-menu :as popover-menu]
            [thermos-ui.frontend.search-box :as search-box]
            [thermos-ui.specs.document :as document]
            [thermos-ui.specs.view :as view]
            [thermos-ui.specs.candidate :as candidate]
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
         create-leaflet-control)


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
  (leaflet/tileLayer
      "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
    (clj->js {})))

(defn mount
  "Make a leaflet control when this component is being put on screen"
  [document watches map-node component]
  (let [map-node @map-node

        edit!  (fn [f & a] (apply state/edit! document f a))
        track! (fn [f & a] (swap! watches conj (apply reagent/track! f a)))

        map (leaflet/map map-node (clj->js {:preferCanvas true
                                            :fadeAnimation true
                                            :zoom 13
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
                                           :polygon false
                                           :marker false
                                           :circlemarker false
                                           :circle false
                                           }})

        layers-control (layers-control
                        {"Satellite" esri-sat-imagery
                         "None" (leaflet/tileLayer "")
                         }
                        {"Candidates" candidates-layer})

        search-control (create-leaflet-control search-box/component)

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
             (.fitBounds map (leaflet/latLngBounds (clj->js [ [s w] [n e] ])))))

        repaint! #(.repaintInPlace candidates-layer)

        pixel-size
        (fn []
          (let [zoom (.getZoom map)
                zz (.unproject map (leaflet/point 0 0) zoom)
                oo (.unproject map (leaflet/point 1 1) zoom)]
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

    (.on map "moveend" follow-map!)
    (.on map "zoomend" follow-map!)


    ;; Event handling for selection

    (let [shape (atom nil) ;; holds the shape to select with, when drawing
          ]

      (.on map (.. leaflet/Draw -Event -CREATED)
           (fn [e]
             (let [s (layer->jsts-shape (.. e -layer))
                   type (.. e -layerType)]
               (reset! shape
                       (if (= "circle" type)
                         ;; radius needs projecting, which is morally wrong.
                         (.buffer s (.. e -layer getRadius))
                         s)))))

      (.on map "click"
           (fn [e]
             (let [oe (.-originalEvent e)
                   method
                   (cond
                     (.-ctrlKey oe) :xor
                     (.-shiftKey oe) :union
                     :otherwise :replace)
                   ]

               (state/edit! document
                            spatial/select-intersecting-candidates

                            (or @shape
                                (latlng->jsts-shape
                                 (.-latlng e)
                                 (* 3 (pixel-size))))

                            method))

             (reset! shape nil)))

      (.on map "contextmenu" (fn [e] (on-right-click-on-map e document pixel-size)))
      )

    (track! show-bounding-box!)))

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

        ;; when the map is redrawn, we re-render each tile
        ;; that is on-screen
        repaint
        (fn []
          ;; (this-as the-map
          ;;   (let [doc @doc]
          ;;     (doseq [visible-tile @tiles]
          ;;       (tile/render-tile doc visible-tile the-map))))
          )

        ;; tiles are just canvas DOM elements
        create-tile
        (fn [coords]
          (let [canvas (js/document.createElement "canvas")]
            (swap! tiles conj canvas)
            (this-as this
              (let [zoom (.-z coords)

                    tile-id (str "T" (swap! tile-id inc) "N")

                    map-control (.-_map this)
                    size (.getTileSize this)
                    north-west (.unproject map-control (.scaleBy coords size) zoom)
                    south-east (.unproject map-control (.scaleBy (.add coords (leaflet/point 1 1)) size) zoom)

                    bbox {:minY (min (.-lat north-west) (.-lat south-east))
                          :maxY (max (.-lat north-west) (.-lat south-east))
                          :minX (min (.-lng north-west) (.-lng south-east))
                          :maxX (max (.-lng north-west) (.-lng south-east))
                          }

                    ;; contains just the IDs of candidates in this box
                    tile-candidates-ids
                    (reagent/atom ())

                    ;; Queries the spatial index in the document
                    ;; to update the candidates ids. This uses a cursor
                    ;; into the document to get the spatial index, so that
                    ;; it only gets triggered when the spatial index is changed.
                    just-index (spatial/index-atom doc)

                    tile-candidates-ids
                    (reagent/track
                     #(spatial/find-candidates-ids-in-bbox @just-index bbox))

                    ;; this is a cursor into the document, used so that we only trigger
                    ;; update-tile-contents on changes to the candidate set
                    just-candidates
                    (reagent/cursor doc [::document/candidates])

                    tile-contents
                    (reagent/track
                     #(map @just-candidates @tile-candidates-ids))
                    ]

                (set! (.. canvas -coords) coords)
                (set! (.. canvas -tile-id) tile-id)
                (set! (.. canvas -tracks)
                      (list (reagent/track!
                             (fn []
                               (when (.. canvas -destroyed)
                                 (println "Tile" tile-id "not destroyed properly"))

                               (tile/render-tile @tile-contents canvas this)
                               ;; identify tiles with big red text
                               ;; (let [ctx (.getContext canvas "2d")]
                               ;;   (set! (.. ctx -font) "40px Sans")
                               ;;   (set! (.. ctx -fillStyle) "#ff0000")
                               ;;   (.fillText ctx (str tile-id) 0 40)
                               ;;   )

                               ))))
                ))
            ;; also request load of the tile into the document
            (state/load-tile! doc (.-x coords) (.-y coords) (.-z coords))
            canvas))

        destroy-tile
        (fn [e]
          (set! (.. e -tile -destroyed) true)
          (swap! tiles disj (.. e -tile))
          (doseq [t (.. e -tile -tracks)]
            (reagent/dispose! t)))

        initialize
        (fn [options]
          (this-as this
            (.call (.. leaflet/GridLayer -prototype -initialize) this options)
            (.on this "tileunload" destroy-tile)))
        ]
    ;; create a leaflet class with these functions
    (->>
     {:initialize initialize
      :createTile create-tile
      :repaintInPlace repaint}
     (clj->js)
     (.extend leaflet/GridLayer))))

(defn- layers-control [choices extras]
  "Create a leaflet control to choose which layers are displayed on the map.
  `choices` is a list of layers of which only one may be selected (radios).
  `extras` is a list of layers of which any number may be selected (checkboxes)."
  ((.. leaflet -control -layers)
   (clj->js choices)
   (clj->js extras)
   #js{:collapsed false}))

(defn- draw-control [args]
  (leaflet/Control.Draw. (clj->js args)))

(let [geometry-factory (jsts/geom.GeometryFactory.)
      jsts-reader (jsts/io.GeoJSONReader. geometry-factory)
      ]
  (defn latlng->jsts-shape [ll rad]
    (let [c (jsts/geom.Coordinate. (.-lng ll) (.-lat ll))
          p (.createPoint geometry-factory c)]
      (.buffer p (* 3 rad))))

  (defn layer->jsts-shape [layer]
    (.-geometry (.read jsts-reader (.toGeoJSON layer) ))))

(defn on-right-click-on-map
  "Callback for when you right-click on the map.
  If you are clicking on a selected candidate, open up a popover menu
  allowing you to edit the selected candidates in situ."
  [e document pixel-size]
  (let [oe (.-originalEvent e)
        click-range (latlng->jsts-shape
                     (.-latlng e)
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
                                                                 inclusion-value))]
        (state/edit!
         document
         operations/set-popover-content
         [popover-menu/component [{:value [:div.centre "EDIT CANDIDATES"]
                                   :key "title"}
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
                                               :key "expensive"}]}
                                  {:value [:div.popover-menu__divider]
                                   :key "divider"}
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
                                               :key "supply"}]}
                                  ]])
        (state/edit! document operations/set-popover-source-coords [oe.clientX oe.clientY])
        (state/edit! document operations/show-popover)))
    ))

(defn create-leaflet-control
  [component]
  (->> {:onAdd (fn [map]
                 (let [box (.create leaflet/DomUtil "div")]
                   (.disableClickPropagation leaflet/DomEvent box)
                   (reagent/render [component map] box)
                   box))
        }
       clj->js
       (.extend leaflet/Control)))

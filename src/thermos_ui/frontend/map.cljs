(ns thermos-ui.frontend.map
  (:require [clojure.set :as set]

            [reagent.core :as reagent]
            [cljsjs.leaflet]
            [cljsjs.leaflet-draw] ;; this modifies js/L.Control in-place
            [cljsjs.jsts :as jsts]

            [thermos-ui.frontend.io :as io]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.spatial :as spatial]
            [thermos-ui.frontend.editor-state :as state]
            [thermos-ui.frontend.tile :as tile]
            [thermos-ui.frontend.popover :as popover]
            [thermos-ui.frontend.popover-menu :as popover-menu]
            [thermos-ui.frontend.search-box :as search-box]
            [thermos-ui.frontend.hide-forbidden-control :as hide-forbidden-control]
            [thermos-ui.frontend.zoom-to-selection-control :as zoom-to-selection-control]
            [thermos-ui.frontend.supply-parameters :as supply-parameters]
            [thermos-ui.frontend.candidate-editor :as candidate-editor]
            
            [thermos-ui.specs.document :as document]
            [thermos-ui.specs.solution :as solution]
            
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
  (let [watches (atom [])
        map-node (atom nil)
        ]
    (reagent/create-class
     {:reagent-render (fn [document]
                        [:div.map {:ref (partial reset! map-node)}])
      :component-did-mount    (partial mount document watches map-node)
      :component-will-unmount (partial unmount watches)
      })))

(def basemaps
  {:none
   (js/L.tileLayer "")

   :stamen-toner-lite
   (js/L.tileLayer
    "https:///stamen-tiles-{s}.a.ssl.fastly.net/toner-background/{z}/{x}/{y}.png"
    (clj->js {:subdomains "abcd"
              :minZoom 0
              :maxZoom 20}))
   
   :satellite
   (js/L.tileLayer
    "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
    (clj->js {}))})

(def basemap-names
  {:none "None"
   :stamen-toner-lite "Maps"
   :stamen-labels "Road names"
   :satellite "Satellite"})

(def heat-density-layer
  (js/L.tileLayer
   "/heat-map-tiles/{z}/{x}/{y}.png"
   (clj->js {:opacity 0.5
             :minZoom 14
             :maxZoom 20})))

(def labels-layer
  (js/L.tileLayer
   "https://cartodb-basemaps-{s}.global.ssl.fastly.net/rastertiles/voyager_only_labels/{z}/{x}/{y}{r}.png"
   (clj->js {:subdomains "abcd"
             :minZoom 14
             :maxZoom 20})
   )
  )

(defn mount
  "Make a leaflet control when this component is being put on screen"
  [document watches map-node component]
  (let [map-node @map-node

        edit!  (fn [f & a] (apply state/edit! document f a))
        track! (fn [f & a]
                 (let [watch (apply reagent/track! f a)]
                   (swap! watches conj #(reagent/dispose! watch))))

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
        candidates-layer (candidates-layer. (clj->js {:tileSize 256
                                                      :minZoom 15
                                                      :maxZoom 20
                                                      }))

        draw-control (draw-control {:position :topleft
                                    :draw {:polyline false
                                           :polygon true
                                           :marker false
                                           :circlemarker false
                                           :circle false
                                           }})

        normal-layers {::view/candidates-layer candidates-layer
                       ::view/heat-density-layer heat-density-layer
                       ::view/labels-layer labels-layer
                       }
        
        layers-control (layers-control
                        (into {}
                              (for [[k v] basemaps]
                                [(or (basemap-names k)
                                     (name k)) v]))
                        
                        {"Candidates" candidates-layer
                         "Heatmap" heat-density-layer
                         "Labels" labels-layer
                         })

        search-control (create-leaflet-control search-box/component)

        hide-forbidden-control (create-leaflet-control hide-forbidden-control/component)

        zoom-to-selection-control (create-leaflet-control zoom-to-selection-control/component)

        test-control-group (create-leaflet-control (fn [leaflet-map]
                                                     [:div.leaflet-bar.leaflet-control-group
                                                      [hide-forbidden-control/component leaflet-map]
                                                      [zoom-to-selection-control/component leaflet-map]
                                                      ]))

        follow-map!
        #(let [bounds (.getBounds map)]
           (edit! operations/move-map
                  {:north (.getNorth bounds)
                   :south (.getSouth bounds)
                   :west (.getWest bounds)
                   :east (.getEast bounds)}))

        map-layers (reagent/cursor document [::view/view-state ::view/map-layers])

        show-map-layers!
        #(let [target-state @map-layers

               target-state
               (merge
                (into {} (for [[k _] normal-layers] [k false]))
                target-state)
               
               visible-state
               (merge
                {::view/basemap-layer
                 (first (keep
                         (fn [[k v]] (when (.hasLayer map v) k))
                         basemaps))}
                (into {} (for [[k v] normal-layers] [k (.hasLayer map v)])))
               ]
           (println "setting layer visibilty from state"
                    (::view/basemap-layer target-state)
                    (::view/basemap-layer visible-state))
           
           (when (not= target-state visible-state)
             (doseq [l (concat
                        (vals basemaps)
                        (vals normal-layers))]

               (when (.hasLayer map l)
                 (.removeLayer map l)))

             (when-let [basemap (basemaps (::view/basemap-layer target-state))]
               (.addLayer map basemap))

             (doseq [[k v] normal-layers]
               (when (get target-state k)
                 (.addLayer map v)))))
        
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

    (track! show-bounding-box!)
    (track! show-map-layers!)
    
    (.addControl map (search-control. (clj->js {:position :topright})))
    (.addControl map layers-control)
    (.addControl map draw-control)
    (.addControl map (test-control-group. (clj->js {:position :topleft})))

    (.on map "moveend" follow-map!)
    (.on map "zoomend" follow-map!)


    ;; this is a bit untidy: when drawing a polygon, the click events
    ;; on the map fire before the created event, so we can ignore
    ;; them.  However, the click event for a rectangle fires after we
    ;; have finished, so we need to know if that's happened.

    ;; this still loses a click in some silly circumstance

    
    (let [draw-state (atom nil)
          method (atom :replace)]

      (.on map (.. js/L.Draw -Event -DRAWSTART)
           #(reset! draw-state
                    (case (o/get % "layerType")
                      "polygon" :drawing-polygon
                      "rectangle" :drawing-rectangle)))

      (.on map (.. js/L.Draw -Event -DRAWSTOP)
           #(reset! draw-state
                    (case (o/get % "layerType")
                      "polygon" nil
                      "rectangle" :stop-rectangle)))

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

               (case @draw-state
                 :stop-rectangle (reset! draw-state nil)
                 nil (state/edit! document
                                  spatial/select-intersecting-candidates

                                  (latlng->jsts-shape
                                   (o/get e "latlng")
                                   (pixel-size))

                                  @method)

                 nil)
               ))
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

    ;; When you start to zoom, abort any pending requests at the zoom level you are leaving
    (.on map "zoomstart" (fn [e]
                           (let [start-zoom (.getZoom e.target)
                                 requests-to-remove-ids (filter
                                                         #(= (apply str (take-last 2 %)) (str start-zoom))
                                                         (io/get-outstanding-request-ids))]
                             (doseq [id requests-to-remove-ids] (io/abort-request id)))
                           ))
    
    (let [watch-layers (fn [e]
                         (let [layer (o/get e "layer" nil)
                               layer-visible (.hasLayer map layer)
                               basemap-key ((set/map-invert basemaps) layer)
                               normal-key ((set/map-invert normal-layers) layer)
                               ]
                           (cond
                             (and layer-visible basemap-key)
                             (swap! map-layers assoc ::view/basemap-layer basemap-key)

                             normal-key
                             (swap! map-layers assoc normal-key layer-visible)))
                         (println "updating layers due to watch")
                         )
          ]
      
      (.on map "overlayadd" watch-layers)
      (.on map "overlayremove" watch-layers)
      (.on map "baselayerchange" watch-layers))

    (swap! watches conj #(do (.off map)
                             (.remove map)))))

(defn unmount
  "Destroy a leaflet when its react component is going away"
  [watches component]

  (doseq [watch @watches] (watch))
  (reset! watches nil))

(defn candidates-layer
  "Create a leaflet layer class which renders the candidates from the document.

  There is a mismatch here between the OO style in leaflet and the functional
  style in react & clojure"
  [doc]
  (let [;; this is a set of all the tiles which are visible
        tiles (atom #{})
        tile-id (atom 0)

        just-candidates
        (reagent/cursor doc [::document/candidates])

        solution
        (reagent/cursor doc [::solution/summary])
        
        filtered-candidates-ids
        (reagent/track #(set (map ::candidate/id (operations/get-filtered-candidates @doc))))

        ;; since we are going to react to this, we should make it its own atom
        showing-forbidden?
        (reagent/track #(operations/showing-forbidden? @doc))

        any-filters?
        (reagent/track #(not (empty? (operations/get-all-table-filters @doc))))


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

                  tile-contents
                  (reagent/track
                   (fn []
                     (let [just-candidates     @just-candidates
                           tile-candidates-ids @tile-candidates-ids
                           showing-forbidden?  @showing-forbidden?
                           any-filters?        @any-filters?
                           tile-candidates (if showing-forbidden?
                                             (keep just-candidates tile-candidates-ids)
                                             (keep #(let [c (just-candidates %)]
                                                      (when (candidate/is-included? c) c))
                                                   tile-candidates-ids))
                           ]
                       (if any-filters?
                         (let [filtered-candidates-ids @filtered-candidates-ids]
                           (map #(assoc % :filtered (not (filtered-candidates-ids (::candidate/id %)))) tile-candidates))
                         tile-candidates))))

                  tile-last-rendered-ids (atom {:normal #{} :special #{}})
                  render-count (atom 0)
                  ]

              ;; If we are not showing forbidden candidates,
              ;; we don't want to bother rendering the tile if there is nothing in it

              (set! (.. canvas -coords) coords)
              (set! (.. canvas -tile-id) tile-id)
              (o/set canvas "tracks"
                     (list (reagent/track!
                            (fn []
                              (when @showing-forbidden?
                                (state/load-tile! doc (.-x coords) (.-y coords) (.-z coords)))
                              )
                            )

                           (reagent/track!
                            (fn [] (tile/render-tile @solution @tile-contents canvas layer)


                              ;; (let [ctx (.getContext canvas "2d")]
                              ;;   (set! (.. ctx -font) "40px Sans")
                              ;;   (set! (.. ctx -fillStyle) "#ff0000")
                              ;;   (.fillText ctx (str tile-id "-" (swap! render-count inc)) 40 40)
                              ;;   )


                              )


                            ))))
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

(defn popover-content [document selected-candidates]
  (let [{paths :path
         buildings :building}
        (group-by ::candidate/type selected-candidates)

        supplies (filter candidate/has-supply? selected-candidates)
        
        paths (map ::candidate/id paths)
        buildings (map ::candidate/id buildings)
        supplies (map ::candidate/id supplies)
        
        set-inclusion!
        (fn [candidates-ids inclusion-value]
          (state/edit!
           document
           #(-> %
                (operations/set-candidates-inclusion candidates-ids inclusion-value)
                (operations/close-popover))))


        allow-supply!
        (fn [candidate-ids]
          ;; (state/edit! document #(-> (operations/allow-supply % candidate-id)
          ;;                            (operations/close-popover)))
          )
        
        
        forbid-supply!
        (fn [candidate-ids]
          ;; (state/edit! document #(-> (operations/forbid-supply % candidate-id)
          ;;                            (operations/close-popover)))
          )
        ]
    [popover-menu/component
     (remove
      nil?
      `[{:value [:div.centre "Edit candidates"] :key "title"}

        ~@(when (not-empty paths)
            (list
             {:value [:b (str (count paths) " roads selected")]
              :key "selected-roads-header"}
             {:value "Set inclusion"
              :key "inclusion-roads"
              :sub-menu [{:value "Required"
                          :key "required"
                          :on-select #(set-inclusion! paths :required)}
                         
                         {:value "Optional"
                          :key "optional"
                          :on-select #(set-inclusion! paths :optional)}
                         
                         {:value "Forbidden"
                          :key "forbidden"
                          :on-select #(set-inclusion! paths :forbidden)}]}
             ))
        

        ~(when (and (not-empty paths) (not-empty buildings))
           {:value [:div.popover-menu__divider] :key "divider"})

        ~@(when (not-empty buildings)
            (list
             {:value [:b (str (count buildings) " buildings selected")]
              :key "selected-buildings-header"}
             {:value "Set inclusion (c)"
              :key "inclusion-buildings"
              :sub-menu [{:value "Required"
                          :key "required"
                          :on-select #(set-inclusion! buildings :required)
                          }
                         {:value "Optional"
                          :key "optional"
                          :on-select #(set-inclusion! buildings :optional)}
                         {:value "Forbidden"
                          :key "forbidden"
                          :on-select #(set-inclusion! buildings :forbidden)}]}
             {:value "Edit demands"
              :key "edit-demands"
              :on-select #(candidate-editor/show-editor! document buildings)}
             ))

        ~@(list
           (when (seq supplies)
             {:value "Disallow as supply point"
              :key "forbid-supplies"
              :on-select #(forbid-supply! supplies)})
           (when (seq buildings)
             {:value (if (seq supplies)
                       "Edit supply parameters"
                       "Make supply point")
              :key "allow-supplies"
              :on-select #(supply-parameters/show-editor! document buildings)}))
        ])]))

(defn on-right-click-on-map
  "Callback for when you right-click on the map.
  If you are clicking on a selected candidate, open up a popover menu
  allowing you to edit the selected candidates in situ."
  [e document pixel-size]

  (let [current-selection (operations/selected-candidates-ids @document)

        oe (o/get e "originalEvent")
        click-range (latlng->jsts-shape
                     (o/get e "latlng")
                     (pixel-size))
        
        clicked-candidates (spatial/find-intersecting-candidates-ids @document click-range)

        update-selection (if (empty? (set/intersection (set current-selection)
                                                       (set clicked-candidates)))
                           #(operations/select-candidates % clicked-candidates :replace)
                           identity)
        ]
    (state/edit! document update-selection)
    (let [selection (operations/selected-candidates @document)]
      (if (empty? selection)
        (popover/close! document)
        (popover/open! document
                       [popover-content document selection]
                       [(o/get oe "clientX") (o/get oe "clientY")])))))

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

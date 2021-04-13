;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.map
  (:require [clojure.set :as set]

            [reagent.core :as reagent]
            [cljsjs.leaflet]
            [cljsjs.leaflet-draw] ;; this modifies js/L.Control in-place
            [cljsjs.jsts :as jsts]
            [cljts.core :as jts]
            [com.rpl.specter :as S]

            [thermos-frontend.io :as io]
            [thermos-frontend.operations :as operations]
            [thermos-frontend.spatial :as spatial]
            [thermos-frontend.editor-state :as state]
            [thermos-frontend.tile :as tile]
            [thermos-frontend.popover :as popover]
            [thermos-frontend.popover-menu :as popover-menu]
            [thermos-frontend.search-box :as search-box]
            [thermos-frontend.hide-forbidden-control :as hide-forbidden-control]
            [thermos-frontend.zoom-to-selection-control :as zoom-to-selection-control]
            [thermos-frontend.supply-parameters :as supply-parameters]
            [thermos-frontend.candidate-editor :as candidate-editor]
            [thermos-frontend.connector-tool :as connector]
            [thermos-frontend.demand-tool :as demand-tool]
            [thermos-frontend.reactive-layer :as reactive-layer]

            [thermos-specs.document :as document]
            [thermos-specs.solution :as soln]

            [thermos-specs.view :as view]
            [thermos-specs.candidate :as candidate]

            [goog.object :as o]
            [goog.ui.SplitPane :refer [Component]]
            [goog.events :refer [listen]]
            [goog.functions :refer [debounce]]

            [thermos-frontend.flow :as flow]

            [thermos-specs.demand :as demand]))

;; the map component

(declare mount
         unmount
         candidates-layer
         connector-layer
         demand-tool-layer
         layers-control
         render-tile
         draw-control
         layer->jsts-shape
         latlng->jsts-shape
         latlng->jsts-point
         on-right-click-on-map
         create-leaflet-control
         unload-candidates)

(defn component
  "Draw a cartographic map for the given `document`, which should be a reagent atom
  containing a document map"
  [document flow]
  (let [watches (atom [])
        map-node (atom nil)
        ]
    (reagent/create-class
     {:reagent-render (fn [document]
                        [:div.map {:ref (partial reset! map-node)}])
      :component-did-mount    (partial mount document flow watches map-node)
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
              :maxZoom 20
              :attribution
              "Map tiles by <a target=_blank href=\"http://stamen.com/\">Stamen Design</a>, under <a target=_blank href=\"http://creativecommons.org/licenses/by/3.0\">CC BY 3.0</a>. Data by <a target=_blank href=\"http://openstreetmap.org/\">OpenStreetMap</a>, under <a target=_blank href=\"http://creativecommons.org/licenses/by-sa/3.0\">CC BY SA</a>."
              }))

   :satellite
   (js/L.tileLayer
    "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
    (clj->js {:minZoom 0 :maxZoom 21
              :attribution
              "Imagery by <a target=_blank href=\"https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer\">Esri, Maxar, Earthstar Geographics, CNES/Airbus DS, USDA FSA, USGS, Aerogrid, IGN, IGP, and the GIS User Community</a>"
              }))})

(def basemap-names
  {:none "None"
   :stamen-toner-lite "Maps"
   :stamen-labels "Road names"
   :satellite "Satellite"})

(def heat-density-layer
  (js/L.tileLayer
   "../d/{z}/{x}/{y}.png"
   (clj->js {:opacity 0.6
             :minZoom 12
             :maxZoom 21})))

(def cold-density-layer
  (js/L.tileLayer
   "../cd/{z}/{x}/{y}.png"
   (clj->js {:opacity 0.6
             :minZoom 12
             :maxZoom 21})))

(def labels-layer
  (js/L.tileLayer
   "https://cartodb-basemaps-{s}.global.ssl.fastly.net/rastertiles/voyager_only_labels/{z}/{x}/{y}{r}.png"
   (clj->js {:subdomains "abcd"
             :minZoom 14
             :maxZoom 20})))

(defn mount
  "Make a leaflet control when this component is being put on screen"
  [document flow watches map-node component]
  (let [map-node @map-node
        
        edit!  (fn [f & a] (apply state/edit! document f a))
        track! (fn [f & a]
                 (let [watch (apply reagent/track! f a)]
                   (swap! watches conj #(reagent/dispose! watch))))

        map-bounding-box (reagent/cursor document [::view/view-state ::view/bounding-box])

        map (js/L.map map-node (clj->js {:preferCanvas true
                                         :fadeAnimation true
                                         :zoom 15
                                         :center
                                         (let [{n :north s :south
                                                w :west e :east}
                                               @map-bounding-box]
                                           [(* 0.5 (+ w e)) (* 0.5 (+ n s))])}))

        ;; The tilesize of 256 is related to the x, y values when talking
        ;; to the server for tile data, so if the tilesize changes, effectively
        ;; the meaning of the zoom, or equivalently the x and y changes too.
        ;; Halve this => increase zoom by 1 in queries below.
        candidates-layer (candidates-layer flow document
                                           {:tileSize 256
                                            :minZoom 13
                                            :maxZoom 21})

        connector-layer (connector-layer {:tileSize 256
                                          :minZoom 15
                                          :maxZoom 21})
        
        demand-tool-layer (demand-tool-layer {:tileSize 256
                                              :minZoom 15
                                              :maxZoom 21})

        draw-control (draw-control {:position :topleft
                                    :draw {:polyline false
                                           :polygon true
                                           :marker false
                                           :circlemarker false
                                           :circle false
                                           }})

        candidates-layer
        (js/L.layerGroup #js [candidates-layer connector-layer demand-tool-layer])

        density-layer
        (if (document/is-cooling? @document)
          cold-density-layer heat-density-layer)
        
        normal-layers {::view/candidates-layer candidates-layer
                       ::view/heat-density-layer density-layer
                       ::view/labels-layer labels-layer
                       }


        layers-control (layers-control
                        (into {}
                              (for [[k v] basemaps]
                                [(or (basemap-names k)
                                     (name k)) v]))

                        {"Candidates" candidates-layer
                         (if (document/is-cooling? @document)
                           "Coldmap" "Heatmap")
                         
                         density-layer
                         "Labels" labels-layer
                         })

        search-control (create-leaflet-control search-box/component)

        map-controls
        (create-leaflet-control
         (fn [leaflet-map]
           [:div.leaflet-bar.leaflet-control-group
            [hide-forbidden-control/component leaflet-map]
            [zoom-to-selection-control/component leaflet-map]
            ]))

        connector-control (create-leaflet-control connector/create-control)
        building-control (create-leaflet-control demand-tool/create-control)

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



        show-bounding-box!
        #(let [{n :north s :south
                w :west e :east} @map-bounding-box]
           (when (and n s w e)
             (.fitBounds map (js/L.latLngBounds (clj->js [ [s w] [n e] ])))))

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

    (-> (js/ResizeObserver.
         (fn [evt]
           (.invalidateSize map)))
        (.observe map-node))
    
    (track! show-bounding-box!)
    (track! show-map-layers!)

    (.addControl map (search-control.
                      (clj->js {:position :topright})))
    (.addControl map layers-control)
    (.addControl map draw-control)

    (.addControl map (map-controls.
                      (clj->js {:position :topleft})))
    (.addControl map (connector-control.
                      (clj->js {:position :topleft})))
    (.addControl map (building-control.
                      (clj->js {:position :topleft})))

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

      (.on map "mousemove"
           (fn [e]
             (cond
               (connector/is-drawing?)
               (let [latln  (o/get e "latlng")
                     hitbox (latlng->jsts-shape latln (pixel-size))

                     hover-candidate (first (spatial/find-intersecting-candidates @document hitbox))]
                 (if hover-candidate
                   (connector/mouse-moved-to-candidate! hover-candidate)
                   (connector/mouse-moved-to-point! (latlng->jsts-point latln))))

               (demand-tool/is-drawing?)
               (let [latln  (o/get e "latlng")
                     hitbox (latlng->jsts-shape latln (pixel-size))
                     hover-candidate (first (spatial/find-intersecting-candidates @document hitbox))]
                 
                 (if hover-candidate
                   (demand-tool/draw-at! nil)
                   (demand-tool/draw-at! (latlng->jsts-point latln))))
               )))

      (.on map "click"
           (fn [e]
             (let [oe (o/get e "originalEvent")]
               (reset! method

                       (cond
                         (o/get oe "ctrlKey" false) :xor
                         (o/get oe "shiftKey" false) :union
                         :otherwise :replace))

               (cond
                 (connector/is-drawing?)
                 (connector/mouse-clicked!)

                 (demand-tool/is-drawing?)
                 (demand-tool/mouse-clicked!)
                 
                 (= @draw-state :stop-rectangle)
                 (reset! draw-state nil)

                 :else
                 (state/edit! document
                              spatial/select-intersecting-candidates

                              (latlng->jsts-shape
                               (o/get e "latlng")
                               (pixel-size))

                              @method)))))

      (.on map "contextmenu" (fn [e] (on-right-click-on-map e document pixel-size))))

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
                             (swap! map-layers assoc normal-key layer-visible))))]

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

;; so the rubbish thin ghere is no way to use a subscription within
;; another one - can I improve that somehow
(defn- filtered-candidates-ids [doc]
  (set (map ::candidate/id @(flow/view* doc operations/get-filtered-candidates))))

(defn- any-filters? [flow]
  (not (empty? @(flow/view* flow operations/get-all-table-filters))))

(defn- tile-candidates-ids [flow bbox]
  (-> (flow/view*
       flow select-keys
       [::spatial/spatial-index ::spatial/update-counter])

      ;; the projection above will make this fire a bit less often
      
      (deref)
      (spatial/find-candidates-ids-in-bbox bbox)))

(defn- selection-adjacent-by-group
  "Given the document, return a set of all IDs which are in the same
  group as any selected candidates"
  [f]
  (let [candidates @(flow/view* f ::document/candidates)

        ;; a map from group-id to group member ids
        groups     (->> (for [[id {g ::demand/group}] candidates
                              :when g] [g id])
                        (group-by first)
                        (S/multi-transform
                         [S/MAP-VALS (S/multi-path
                                      [S/ALL (S/terminal second)]
                                      (S/terminal set))]))

        ]
    (set
     (for [[id {g ::demand/group
                s ::candidate/selected}]
           candidates
           :when (and s g)
           adj (groups g)]
       adj))))

(defn candidates-layer [flow doc args]
  (let [just-candidates
        (flow/view* flow ::document/candidates)

        adjacent-ids
        (flow/view flow selection-adjacent-by-group)

        solution
        (flow/view* flow document/has-solution?)

        filtered-candidates-ids
        (flow/view flow filtered-candidates-ids)

        selected-candidates-ids
        (flow/view* flow operations/selected-candidates-ids)

        showing-forbidden?
        (flow/view* flow operations/showing-forbidden?)

        any-filters?
        (flow/view flow any-filters?)

        map-view        (flow/view* flow get-in [::view/view-state ::view/map-view])
        show-diameters? (flow/view* flow get-in [::view/view-state ::view/show-pipe-diameters])

        min-max-diameters
        (reagent/track
         #(when @solution
            (reduce
             (fn [[l u] d]
               [(or (and l (min d l)) d)
                (or (and u (max d u)) d)])
             []
             (keep ::soln/diameter-mm (vals @just-candidates)))))
        ]
    

    (reactive-layer/create
     :internal-id "candidates-layer"
     :constructor-args args
     :paint-tile
     (fn [canvas coords layer bbox]
       (let [;; contains just the IDs of candidates in this box
             tile-candidates-ids
             (flow/view flow tile-candidates-ids bbox)

             tile-contents
             (reagent/track
              (fn []
                (let [adjacent-ids        @adjacent-ids
                      just-candidates     @just-candidates
                      tile-candidates-ids @tile-candidates-ids

                      showing-forbidden? @showing-forbidden?
                      any-filters?       @any-filters?
                      tile-candidates    (if showing-forbidden?
                                           (keep just-candidates tile-candidates-ids)
                                           (keep #(let [c (just-candidates %)]
                                                    (when (candidate/is-included? c) c))
                                                 tile-candidates-ids))

                      any-adjacent? (not-empty adjacent-ids)
                      ]

                  (cond-> tile-candidates
                    any-filters?
                    (->> (map (let [f @filtered-candidates-ids]
                                #(assoc % :filtered (not (f (::candidate/id %)))))))

                    any-adjacent?
                    (->> (map (fn [c]
                                (cond-> c
                                  (contains? adjacent-ids (::candidate/id c))
                                  (assoc :in-selected-group true))
                                ))))
                  )))
             ]
         ;; we return our tracks for later disposal
         (list (reagent/track!
                (fn []
                  (when @showing-forbidden?
                    (state/load-tile! doc (.-x coords) (.-y coords) (.-z coords)))))

               (reagent/track!
                (fn []
                  (tile/render-tile
                   @solution
                   @tile-contents
                   canvas
                   layer
                   (or @map-view ::view/constraints)
                   (and @show-diameters? @min-max-diameters))
                  )))
         )))))

 (defn connector-layer [doc args]
  (let [map-view (reagent/track #(-> @doc ::view/view-state ::view/map-view))]
    (reactive-layer/create
     :constructor-args args
     :internal-id "connectors"
     :paint-tile
     (fn [canvas coords layer bbox]
       ;; because there is not a lot to render in the connector layer,
       ;; we don't need to do optimisations for it
       (list
        (reagent/track!
         (fn []
           (connector/render-tile! canvas coords layer))))))))

(defn demand-tool-layer [doc args]
  (reactive-layer/create
   :constructor-args args
   :internal-id "new-buildings"
   :paint-tile
   (fn [canvas coords layer bbox]
     ;; because there is not a lot to render in the connector layer,
     ;; we don't need to do optimisations for it
     (list
      (reagent/track!
       (fn []
         (demand-tool/render-tile! canvas coords layer)))))))

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

(defn latlng->jsts-point [ll]
  (jts/create-point (.-lng ll) (.-lat ll)))

(defn latlng->jsts-shape [ll rad]
  (.buffer (latlng->jsts-point ll) (* 3 rad)))

(defn layer->jsts-shape [layer]
  (-> (.toGeoJSON layer)
      (jts/json->geom)
      (o/get "geometry")))

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
           #(operations/set-candidates-inclusion % candidates-ids inclusion-value))
          (popover/close!))

        forbid-supply!
        (fn [candidate-ids]
          (state/edit! document
                       #(document/map-candidates %
                         candidate/forbid-supply!
                         candidate-ids))
          (popover/close!))

        delete-selection!
        (fn [candidate-ids]
          (state/edit-geometry!
           document
           document/remove-candidates candidate-ids)
          (popover/close!))
        ]
    [popover-menu/component
     (remove
      nil?
      `[{:value [:div.centre "Edit candidates"] :key "title"}

        ~@(when (not-empty paths)
            (list
             {:value [:b (str (count paths) " roads")]
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
             {:value "Edit cost (e)"
              :key "road-cost"
              :on-select #(candidate-editor/show-editor! document paths)}))

        ~(when (and (not-empty paths) (not-empty buildings))
           {:value [:div.popover-menu__divider] :key "divider"})

        ~@(when (not-empty buildings)
            (list
             {:value [:b (str (count buildings) " buildings")]
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
             {:value "Edit buildings (e)"
              :key "edit-demands"
              :on-select #(candidate-editor/show-editor! document buildings)}
             {:value "Edit groups"
              :key "grouping"
              :sub-menu
              [{:value "Group buildings (G)"
                :key "group-buildings"
                :on-select #(state/fire-event! [:group-selection])}
               {:value "Ungroup buildings (U)"
                :key "ungroup-buildings"
                :on-select #(state/fire-event! [:ungroup-selection])}
               {:value "Select same group (g)"
                :key "select-group"
                :on-select #(state/fire-event! [:group-select-members])}]}
             
             
             {:value [:div.popover-menu__divider] :key "divider-2"}))
        

        ~@(list
           (when (seq buildings)
             {:key "supplies-header"
              :value [:b (str (count supplies) " supply points")]})
           (when (seq supplies)
             {:value "Disallow as supply point"
              :key "forbid-supplies"
              :on-select #(forbid-supply! supplies)})
           (when (seq buildings)
             {:value (if (seq supplies)
                       "Edit supply parameters (s)"
                       "Make supply point (s)")
              :key "allow-supplies"
              :on-select #(supply-parameters/show-editor! document buildings)}))

        ~@(when (or (not-empty paths) (not-empty buildings))
            (list {:value [:div.popover-menu__divider] :key "divider-3"}
                  {:value "Delete"
                   :key "delete-items"
                   :on-select #(delete-selection! (map ::candidate/id selected-candidates))
                   }))
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
        (popover/close!)
        (popover/open! [popover-content document selection]
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

        candidates-in-bbox-ids (set (spatial/find-candidates-ids-in-bbox @state/state bbox))
        
        keep-constraints #{:required :optional}
        loaded-candidates (::document/candidates @state/state)
        
        
        ]
    ;; Remove all the candidates that we don't want to keep
    (when (< (count candidates-in-bbox-ids)
             (count loaded-candidates))
      (let [candidates-to-keep
            (persistent!
             (reduce-kv (fn [out k v]
                          (cond-> out
                            (or (::candidate/selected v)
                                (::candidate/modified v)
                                (keep-constraints (::candidate/inclusion v))
                                (contains? candidates-in-bbox-ids k))
                            (assoc! k v)))
                        (transient {})
                        loaded-candidates))]
        (state/edit-geometry! state/state assoc ::document/candidates candidates-to-keep)))))

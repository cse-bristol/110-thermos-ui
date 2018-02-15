(ns thermos-ui.frontend.map
  (:require [reagent.core :as reagent]
            [leaflet :as leaflet]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.editor-state :as state]
            [thermos-ui.frontend.tile :as tile]
            ))

;; the map component

(declare mount unmount candidates-layer layers-control render-tile)

(defn component
  "Draw a cartographic map for the given `document`, which should be a reagent atom
  containing a document map"
  [document]
  (let [watches (atom nil)
        map-node (atom nil)
        ]
    (reagent/create-class
     {:reagent-render         (fn [document]
                                [:div.map {:style {:height "500px"}
                                           :ref (partial reset! map-node)}])
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

        map (leaflet/map map-node (clj->js {:preferCanvas true :fadeAnimation true
                                            :zoom 13
                                            :center [51.454514 -2.587910]
                                            }))
        candidates-layer (candidates-layer document)
        candidates-layer (candidates-layer. (clj->js {:tileSize 128}))

        layers-control (layers-control
                        #js {}
                        #js {"Candidates" candidates-layer})

        follow-map!
        #(let [bounds (.getBounds map)]
           (edit! operations/move-map
                  {:north (.getNorth bounds)
                   :south (.getSouth bounds)
                   :west (.getWest bounds)
                   :east (.getEast bounds)}))

        repaint! #(.repaintInPlace candidates-layer)

        ;; this is probably the tricky bit
        ;; we want to read the bounds out of the document
        ;; (which will get set when we move map)
        ;; and then request loading of the visible area
        ;; something should diff out the unloaded area to help.
        demand-tiles! #()
        ]

    (.addLayer map esri-sat-imagery)
    (.addLayer map candidates-layer)
    (.addControl map layers-control)

    (.on map "moveend" follow-map!)
    (.on map "zoomend" follow-map!)

    (track! repaint!) ;; repaint when the candidate dataset changes
    (track! demand-tiles!) ;; ask for more tiles when we move the viewport
    ;; TODO we need the inverse of follow-map! here
    ))


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
        ;; when the map is redrawn, we re-render each tile
        ;; that is on-screen
        repaint
        (fn []
          (this-as the-map
            (let [doc @doc]
              (doseq [visible-tile @tiles]
                (tile/render-tile doc visible-tile the-map)))))

        ;; tiles are just canvas DOM elements
        create-tile
        (fn [coords]
          (let [canvas (js/document.createElement "canvas")]
            (swap! tiles conj canvas)
            (this-as this
              (set! (.. canvas -coords) coords)
              (tile/render-tile doc canvas this))
            canvas))

        destroy-tile
        (fn [e] (swap! tiles disj (.. e -tile)))

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

(defn layers-control [choices extras]
  "Create a leaflet control to choose which layers are displayed on the map.
  `choices` is a list of layers of which only one may be selected (radios).
  `extras` is a list of layers of which any number may be selected (checkboxes)."
  ((.. leaflet -control -layers)
   choices
   extras
   #js{:collapsed false}))

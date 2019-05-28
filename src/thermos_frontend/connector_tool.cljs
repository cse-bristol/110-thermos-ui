(ns thermos-frontend.connector-tool
  "The code supporting drawing new connectors on the map.

  A user clicks the connector tool button.
  Then they can click the thing they want to connect, intermediate points, and the end thing.

  As they move the mouse, the map should display what would happen if they clicked.
  The start and end segments should be placed for minimum length and should move around as the cursor moves.

  Only one of these can exist in a given js environment at once because of the
  global state, but that would be easy to change."
  (:require [reagent.core :as reagent]
            [cljsjs.jsts :as jsts]

            [cljts.core :as jts]
            
            [thermos-frontend.tile :as tile]
            [thermos-frontend.spatial :as spatial]
            
            [thermos-frontend.editor-state :as editor-state]

            [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.path :as path]))

(def start-state
  {:drawing false
   :start nil
   :vertices []
   :end nil
   :next-vertex nil
   :next-candidate nil
   })

(defonce state
  (reagent/atom start-state))

(defn create-control [leaflet-map]
  [:div.leaflet-control-group.leaflet-bar
   [:a.leaflet-control-button
    {:title "Draw a new connector (j)"
     :style {:font-size "28px"
             :color
             (if (:drawing @state) "#00bfff")
             :border
             (if (:drawing @state) "#00bfff")}
     :on-click #(swap! state (fn [s]
                              (if (:drawing s)
                                start-state
                                (assoc start-state :drawing true))))}
    "⟜"]])

(defn is-drawing? []
  (:drawing @state))

(defn mouse-moved-to-point!
  "Use this function when the user moves the mouse to an empty space.

  This will change the render state to show that if the user clicks
  here, they will add a vertex to the in-progress connector."
  [point]
  (when (:start @state)
    (swap! state assoc
           :next-candidate nil
           :next-vertex point)))

(defn mouse-moved-to-candidate!
  "Use this function when the user moves the mouse to a connector.

  This will change the render state to show that if the user clicks
  here, they will either start or complete their connector."
  [candidate]

  (swap! state assoc
         :next-candidate candidate
         :next-vertex nil))

(def spline
  (reagent/track
   (fn []
     (let [{drawing :drawing
            start :start
            next-vertex :next-vertex
            next-candidate :next-candidate
            vertices :vertices}
           @state

           vertices (filter identity
                            (conj vertices next-vertex))

           first-vertex (first vertices)
           ]

       (cond
         (and start first-vertex next-candidate)
         (let [op1 (jsts/operation.distance.DistanceOp.
                    (::spatial/jsts-geometry start)
                    first-vertex)

               op2 (jsts/operation.distance.DistanceOp.
                    (last vertices)
                    (::spatial/jsts-geometry next-candidate))
               
               coord-on-start (aget (.nearestPoints op1) 0)
               coord-on-end   (aget (.nearestPoints op2) 1)
               ]
           (concat [coord-on-start]
                   (map #(.getCoordinate %) vertices)
                   [coord-on-end]))
         
         (and start first-vertex)
         (let [op (jsts/operation.distance.DistanceOp.
                   (::spatial/jsts-geometry start)
                   first-vertex)
               coord-on-start (aget (.nearestPoints op) 0)]
           (concat [coord-on-start] (map #(.getCoordinate %) vertices)))

         (and start next-candidate)
         (let [op (jsts/operation.distance.DistanceOp.
                   (::spatial/jsts-geometry start)
                   (::spatial/jsts-geometry next-candidate))]
           (seq (.nearestPoints op))))))))

(defn- connect-path [candidates path-id vertex-coord vertex-id]
  (let [path       (get candidates path-id)
        path-geom  (::spatial/jsts-geometry path)
        path-start (first (jts/coordinates path-geom))
        path-end   (last (jts/coordinates path-geom))
        ]

    (if (or (= path-start vertex-coord)
            (= path-end vertex-coord))
      ;; if we're connect to the start or end of an existing line
      ;; all we need to do is insert the geometry we have created
      ;; and all will be well
      candidates

      ;; in this case we are cutting the line, which means making two
      ;; new lines and deleting the one we're cutting.
      ;; the new lines are similar to the old ones in some way.
      (let [[h t] (jts/cut-line-string path-geom vertex-coord)]
        (if t
          (let [vtx-id (jts/ghash (jts/create-point vertex-coord))

                ;; TODO we need to update the lengths of these paths
                
                h (assoc path
                         ::spatial/jsts-geometry h
                         ::candidate/id (jts/ghash h)
                         ::candidate/geometry (jts/geom->json h)
                         ::path/end vtx-id)
                
                t (assoc path
                         ::spatial/jsts-geometry t
                         ::candidate/id (jts/ghash t)
                         ::candidate/geometry (jts/geom->json t)
                         ::path/start vtx-id)
                ]
            (-> candidates
                (dissoc path-id)
                (assoc (::candidate/id h) h
                       (::candidate/id t) t)))
          
          ;; if t is nil then the cut only made one piece for some reason
          candidates)
        
        ))))


(defn mouse-clicked!
  "Use this function when the user clicks - since the state should
  contain information about what we are pointing at you don't need to
  say much.

  In the event that the click completes a connector we will need to
  modify document to know as much."
  []

  (let [{next-vertex :next-vertex
         next-candidate :next-candidate
         start-candidate :start} @state]
    (cond
      next-vertex ;; add point to list of points
      (swap! state update :vertices conj next-vertex)

      (and next-candidate
           (not start-candidate))
      (swap! state assoc :start next-candidate)

      next-candidate
      (let [spline           @spline
            end-candidate    next-candidate

            spline-geometry  (jts/create-line-string
                              (clj->js spline))

            spline-geojson   (jts/geom->json spline-geometry)
            
            spline-id        (jts/ghash spline-geometry)
            start-id         (jts/ghash
                              (jts/create-point (first spline)))
            
            end-id           (jts/ghash
                              (jts/create-point (last spline)))
            
            
            length           (jts/geodesic-length spline-geometry)
            
            spline-candidate
            {::candidate/type    :path
             ::candidate/subtype "Connector"
             ::candidate/id       spline-id
             ::candidate/geometry spline-geojson

             ::spatial/jsts-geometry spline-geometry
             
             ::path/start  start-id
             ::path/end    end-id

             ::path/length length}
            
            ]

        (println "new line length" length)
        ;; if our start-candidate and end-candidate are paths we may
        ;; also need to split those

        (editor-state/edit-geometry!
         editor-state/state
         (fn [doc]
           (-> doc
               (update ::document/candidates
                       assoc spline-id spline-candidate)
               (cond->
                 ;; connect buildings:
                 (= :building (::candidate/type start-candidate))
                 (update-in [::document/candidates
                             (::candidate/id start-candidate)
                             ::candidate/connections]
                            conj start-id)

                 (= :building (::candidate/type end-candidate))
                 (update-in [::document/candidates
                             (::candidate/id end-candidate)
                             ::candidate/connections]
                            conj end-id)

                 ;; split path
                 (= :path (::candidate/type start-candidate))
                 (update ::document/candidates
                         connect-path
                         (::candidate/id start-candidate)
                         (first spline)
                         start-id)
                 
                 (= :path (::candidate/type end-candidate))
                 (update ::document/candidates
                         connect-path
                         (::candidate/id end-candidate)
                         (last spline)
                         end-id)
                 ))))
        (reset! state start-state))
      )))

(defn render-tile!
  "Draw what is going on into the given tile."

  ;; TODO avoid drawing in wrong tiles
  [canvas coords layer]
  (let [state @state
        ctx (.getContext canvas "2d")
        size (.getTileSize layer)
        width  (.-x size)
        height (.-y size)]
    
    (if (:drawing state)
      (let [project (tile/projection canvas layer)

            {start    :start
             vertices :vertices
             end      :end

             next-vertex    :next-vertex
             next-candidate :next-candidate} state

            spline @spline
            ]

        (tile/fix-size canvas layer)

        (set! (.. ctx -lineWidth) 6)
        (set! (.. ctx -strokeStyle) "#00bfff")
        (.setLineDash ctx #js [])
        
        (when start
          
          (tile/render-geometry
           (::spatial/jsts-geometry start)
           ctx project
           false false))
        
        (when next-candidate
          (tile/render-geometry
           (::spatial/jsts-geometry next-candidate)
           ctx project
           false false))

        (.setLineDash ctx #js [5 3])
        (tile/render-coordinate-seq spline ctx project)
        (.stroke ctx))

      ;; if we are not drawing, erase the canvas in case we were
      (.clearRect ctx 0 0 width height)
      )))

(ns thermos-ui.frontend.editor
  (:require [reagent.core :as r :refer [atom]]
            [thermos-ui.frontend.map :as map]
            [thermos-ui.urls :as urls]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.specs.view :as view]
            [thermos-ui.specs.document :as document]
            [thermos-ui.frontend.editor-state :as state]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.main-nav :as main-nav]
            [thermos-ui.frontend.network-candidates-panel :as network-candidates-panel]
            [thermos-ui.frontend.selection-info-panel :as selection-info-panel]
            [thermos-ui.frontend.popover :as popover]
            [thermos-ui.frontend.toaster :as toaster]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as s]
            [goog.ui.SplitPane :refer [SplitPane Component]]
            [goog.events :refer [listen]]
            [goog.math :refer [Size]]
            [goog.functions :refer [debounce]]
            ))

(enable-console-print!)

(defn- url-decode [str]
  (js/decodeURIComponent (s/replace str "+" "%20")))

(let [[org-name proj-name version]
      (->>
       (-> js/window.location
           (.-pathname)
           (s/split "/"))
       (remove empty?))

      proj-name (url-decode proj-name)
      ]

  (defn do-save [name]
    (state/save-document!
     org-name name
     (fn [new-id]
       (js/window.history.pushState
        nil
        name
        (urls/editor org-name name new-id))
       ;; Show "toaster" message notifying successful save
       (state/edit! state/state operations/set-toaster-content "Project successfully saved.")
       (state/edit! state/state operations/set-toaster-class "toaster--success")
       (state/edit! state/state operations/show-toaster)
       (js/setTimeout
        (fn [] (state/edit! state/state operations/hide-toaster))
        4000)
       )))

  (defn- rotate-inclusion! []
    (let [selected (operations/selected-candidates @state/state)]
      (when-not (empty? selected)
        (let [inclusion (::candidate/inclusion (first selected))]
          (state/edit! state/state
                       operations/set-candidates-inclusion
                       (cljs.core/map ::candidate/id selected)
                       (case inclusion
                         :forbidden :optional
                         :optional :required
                         :required :forbidden

                         :optional))))))

  (defonce unsaved? (r/atom false))
  (defonce interesting-state (fn [] (document/keep-interesting @state/state)))
  (defonce changed (reagent.ratom/run! (reset! unsaved? true) (r/track! interesting-state)))

  (defn map-page []
    (let [close-popover (fn [e]
                          (let [popover-menu-node (js/document.querySelector ".popover-menu")
                                click-is-outside-popover (and (some? popover-menu-node)
                                                              (not (.contains popover-menu-node e.target)))
                                popover-is-populated (some?
                                                      (->> @state/state
                                                          ::view/view-state
                                                          ::view/popover
                                                          ::view/popover-content))]
                            (if (and click-is-outside-popover popover-is-populated)
                                (state/edit! state/state operations/close-popover))))
          close-table-filter (fn [e] (state/edit! state/state operations/close-table-filter))
          ]
      [:div.editor__container
       {:on-key-press
        (fn [e]
          (case (.-key e)
            "c" (rotate-inclusion!)
            :default)
          )
        :on-click (fn [e] (do (close-popover e) (close-table-filter e))) ;; Close the popover menu if it is open
        :on-context-menu close-popover
        }
       [main-nav/component
        {:on-save do-save
         :name (if (and (= proj-name "new") (not version)) "" proj-name)
         :unsaved? unsaved?}]

       [:div.layout__container.goog-splitpane {:id "splitpane"}
        [:div.goog-splitpane-first-container.lhs-pane
         [map/component state/state]
         ]
        [:div.goog-splitpane-second-container.rhs-pane
         [:div.goog-splitpane {:id "splitpane2" :style {:height "100%"}}
          [:div.goog-splitpane-first-container.rhs-top-pane
           [network-candidates-panel/component state/state]]
          [:div.goog-splitpane-second-container.rhs-bottom-pane
           [selection-info-panel/component state/state]]
          [:div.goog-splitpane-handle.goog-splitpane-handle--vertical]
          ]
         ]
        [:div.goog-splitpane-handle.goog-splitpane-handle--horizontal]
        ]
       [popover/component state/state]
       [toaster/component state/state]]))

  (defn on-js-reload [])

  (defn mount-root []
    (r/render [map-page] (.getElementById js/document "app"))

    ;; Warn the user they have unsaved changes if they try to leave the page.
    (.addEventListener js/window "beforeunload"
                       (fn [e]
                         (when @unsaved?
                           (let [msg "You have unsaved changes. Are you sure you want to leave the page?"]
                             (set! e.returnValue msg)
                             msg))))

    ;; Do the split pane thing
    (let [;; 1st splitpane
          lhs (goog.ui.Component.)
          rhs (goog.ui.Component.)
          splitpane (goog.ui.SplitPane. lhs rhs goog.ui.SplitPane/Orientation.HORIZONTAL)
          initial-left-pane-size (- (.-clientWidth (js/document.querySelector "body")) 550)
          ;; 2nd splitpane (the nested one)
          top (goog.ui.Component.)
          bottom (goog.ui.Component.)
          splitpane2 (goog.ui.SplitPane. top bottom goog.ui.SplitPane/Orientation.VERTICAL)]

      (.setInitialSize splitpane initial-left-pane-size)
      (.setHandleSize splitpane 3)
      (.decorate splitpane (js/document.getElementById "splitpane"))

      ;; Set a reference to the splitpane in the view state so we can access it elsewhere
      (state/edit! state/state
                   assoc
                   ::splitpane
                   splitpane)

      ;; Listen for when the window resizes and make the splitpane resize to match it
      ;; (Although you'd hope the user won't be resizing their browser very frequently)
      (js/window.addEventListener
       "resize"
       (fn [e]
         (debounce
          (.setSize splitpane (Size. js/window.innerWidth (- js/window.innerHeight 50)))
          200)))

      ;; Nest the second pane in the RHS pane
      (.setInitialSize splitpane2 "50%")
      (.setHandleSize splitpane2 3)
      (.decorate splitpane2 (js/document.getElementById "splitpane2"))

      ;; Listen for when the 1st splitpane is resized and do the following:
      ;; Don't let the RHS pane get smaller than 400px
      ;; Resize the second (nested) splitpane to match the new width of the RHS pane.
      (let [second-pane-min-width 400]
        (goog.events/listen
         splitpane
         goog.ui.Component.EventType.CHANGE
         (fn [e] (let [second-pane-width (- js/window.innerWidth (.getFirstComponentSize e.target))]
                   (if (< second-pane-width second-pane-min-width)
                     ;; Coincidentally these actions are exclusive of each other
                     (.setFirstComponentSize splitpane (- js/window.innerWidth second-pane-min-width))
                     (.setSize splitpane2 (Size. (- second-pane-width 2) "100%")))
                   ))))

      ;; Listen for when the second splitpane is resized
      ;; and ensure that neither of the panes get smaller than 50px in height.
      (let [min-height 50]
        (goog.events/listen
         splitpane2
         goog.ui.Component.EventType.CHANGE
         (fn [e] (let [;; It's slightly easier to think of first component max height
                       ;; rather than second component min-height
                       first-component-max-height (apply - [js/window.innerHeight min-height 50])]
                   (if (< (.getFirstComponentSize splitpane2) (- min-height 3))
                     (.setFirstComponentSize splitpane2 (- min-height 3)))
                   (if (> (.getFirstComponentSize splitpane2) first-component-max-height)
                     (.setFirstComponentSize splitpane2 first-component-max-height))
                   ))))
      )
    )


  (if version
    ;; If this is an existing problem, load it, then create the map.
    ;; Otherwise create the map straight away.
    (state/load-document! org-name proj-name version mount-root)
    (mount-root)))

(ns thermos-ui.frontend.editor
  (:require [reagent.core :as r :refer [atom]]
            [thermos-ui.frontend.map :as map]
            [thermos-ui.urls :as urls]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.specs.solution :as solution]
            [thermos-ui.specs.view :as view]
            [thermos-ui.specs.document :as document]
            [thermos-ui.frontend.editor-state :as state]
            [thermos-ui.frontend.parameters :as parameters]
            [thermos-ui.frontend.solution :as solution-component]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.main-nav :as main-nav]
            [thermos-ui.frontend.network-candidates-panel :as network-candidates-panel]
            [thermos-ui.frontend.selection-info-panel :as selection-info-panel]
            [thermos-ui.frontend.popover :as popover]
            [thermos-ui.frontend.toaster :as toaster]
            [thermos-ui.frontend.editor-keys :as keys]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as s]
            [goog.ui.SplitPane :refer [SplitPane Component]]
            [goog.events :refer [listen]]
            [goog.math :refer [Size]]
            [goog.functions :refer [debounce]]

            [re-com.core :as rc]
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

  (defn do-save [run? name]
    (state/save-document!
     org-name name run?
     (fn [org-name name new-id]
       (js/window.history.pushState
        nil
        name
        (urls/editor org-name name new-id))
       ;; Show "toaster" message notifying successful save
       (toaster/show!
        [:div.toaster.toaster--success "Project saved"])

       )))

  (defonce unsaved? (r/atom false))
  (defonce interesting-state (fn [] (document/keep-interesting @state/state)))
  (defonce changed (reagent.ratom/run! (reset! unsaved? true) (r/track! interesting-state)))
  
  (defn map-page [document]
    (r/with-let [h-split-pos (r/cursor document
                                       [::view/view-state
                                        ::view/map-page-h-split]
                                       )
                 v-split-pos (r/cursor document
                                       [::view/view-state
                                        ::view/map-page-v-split]
                                       )
                 ]

      [rc/v-split
       :initial-split (or @h-split-pos 75)
       :on-split-change #(reset! h-split-pos %)
       :margin "0"
       :panel-1 [rc/h-split
                 :initial-split (or @h-split-pos 60)
                 :on-split-change #(reset! h-split-pos %)
                 :margin "0"
                 :panel-1 [map/component document]
                 :panel-2 [selection-info-panel/component document]]
       :panel-2 [:div
                 {:style {:width :100%}}
                 [network-candidates-panel/component document]]
       ])
    
    )
  (defn main-page []
    (r/with-let [selected-tab (r/cursor state/state [::view/view-state ::view/selected-tab])
                 solution (r/cursor state/state [::solution/solution])

                 last-run-state (r/atom nil)
                 ]
      (let [close-popover
            (fn [e]
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
         {:on-key-press keys/handle-keypress
          :on-click (fn [e] (close-popover e) (close-table-filter e)) ;; Close the popover menu if it is open
          :on-context-menu close-popover
          }
         [main-nav/component
          {:on-save (partial do-save false)
           :on-run (partial do-save true)
           :on-tab-switch #(reset! selected-tab %)
           :name (if (and (= proj-name "new") (not version)) "" proj-name)
           :unsaved? unsaved?
           :selected-tab (or @selected-tab :candidates)
           :tabs
           [
            {:key :candidates :label "Map"}
            {:key :parameters :label "Options"}
            (when @solution {:key :solution :label "Result"})
            {:key :help :label "Help"}
            ]
           }]

         (let [run-state (state/is-running?)
               last-state @last-run-state]
           (when (and (= :complete run-state)
                      (or (= :running last-state)
                          (= :queued last-state)))
             (let [[org-name proj-name version] (state/get-last-save)]
               (println "last save version " version)
               (state/load-document!
                org-name proj-name version
                (fn []
                  (println "select solution tab?")
                  (state/edit! state/state
                               assoc-in
                               [::view/view-state ::view/selected-tab]
                               :solution)))))
           
           (reset! last-run-state run-state)
           
           (when (#{:running :queued} run-state)
             [:div
              {:style {:position :absolute
                       :z-index 1000000
                       :width :100%
                       :height :100%
                       :display :flex
                       :background "rgba(0,0,0,0.75)"}}
              [:b {:style {:color :white
                           :margin :auto
                           :font-size :3em}}
               (if (= :queued run-state)
                 [:span "Number " (state/queue-position) " in queue"]
                 [:span "Running..."])]]))
         
         (case (or @selected-tab :candidates)
           :candidates
           [map-page state/state]

           :parameters
           [parameters/component state/state]

           :solution
           [solution-component/component state/state]
           
           :help
           [:iframe {:src "/help/index.html"
                     :style {:height :500px
                             :width :100%
                             :margin 0
                             :padding 0
                             :border :none}
                     }]
           
           )
         
         [popover/component state/state]
         [toaster/component]]
        )))

  (defn on-js-reload [])

  (defn mount-root []
    (r/render [main-page] (.getElementById js/document "app"))

    ;; Warn the user they have unsaved changes if they try to leave the page.
    (.addEventListener js/window "beforeunload"
                       (fn [e]
                         (when @unsaved?
                           (let [msg "You have unsaved changes. Are you sure you want to leave the page?"]
                             (set! e.returnValue msg)
                             msg))))

    )


  (if version
    ;; If this is an existing problem, load it, then create the map.
    ;; Otherwise create the map straight away.
    (state/load-document! org-name proj-name version mount-root)
    (mount-root)))

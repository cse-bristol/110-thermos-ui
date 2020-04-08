(ns thermos-frontend.editor
  (:require [reagent.core :as r :refer [atom]]
            [thermos-frontend.map :as map]
            [thermos-urls :as urls]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.solution :as solution]
            [thermos-specs.view :as view]
            [thermos-specs.document :as document]
            [thermos-frontend.editor-state :as state]
            [thermos-frontend.preload :as preload]

            [thermos-frontend.operations :as operations]
            [thermos-frontend.main-nav :as main-nav]
            [thermos-frontend.network-candidates-panel :as network-candidates-panel]
            [thermos-frontend.selection-info-panel :as selection-info-panel]
            [thermos-frontend.popover :as popover]
            [thermos-frontend.view-control :as view-control]
            [thermos-frontend.params.global :as global-parameters]
            [thermos-frontend.params.objective :as objective]
            [thermos-frontend.params.tariffs :as tariff-parameters]
            [thermos-frontend.params.pipes :as pipe-parameters]
            [thermos-frontend.params.insulation :as insulation]
            [thermos-frontend.params.alternatives :as alternatives]
            [thermos-frontend.params.profiles :as profiles]
            [thermos-frontend.params.supply-technologies :as supply-technologies]
            [thermos-frontend.params.supply-objective :as supply-objective]
            
            [thermos-frontend.solution-view :as solution-view]
            [thermos-frontend.toaster :as toaster]
            [thermos-frontend.editor-keys :as keys]
            [thermos-frontend.theme :as theme]

            [clojure.pprint :refer [pprint]]
            [clojure.string :as s]
            [goog.ui.SplitPane :refer [SplitPane Component]]
            [goog.events :refer [listen]]
            [goog.math :refer [Size]]
            [goog.functions :refer [debounce]]
            [thermos-frontend.util :refer [target-value]]

            [re-com.core :as rc]))

(enable-console-print!)

(defn do-save [run? name]
  (let [can-run (document/is-runnable? @state/state)
        invalid (and run? (not can-run))]
    (when invalid
      (toaster/show!
       [:div.toaster.toaster--error
        "Cannot optimise until you have added some demands and supplies!"]))

    (when-not (and invalid (not (state/needs-save?)))
      (state/save!
       name
       :run (and run? can-run)
       :callback
       #(when-not invalid
          (toaster/show! [:div.toaster.toaster--success "Project saved"]))))))


(defn map-page [doc]
  (r/with-let [h-split-pos (r/cursor doc
                                     [::view/view-state
                                      ::view/map-page-h-split]
                                     )
               v-split-pos (r/cursor doc
                                     [::view/view-state
                                      ::view/map-page-v-split]
                                     )
               v-splitter-element (atom nil)
               v-splitter-dblclick-listener (atom nil)]

    
    [(r/create-class
       {:reagent-render
        (fn []
          [rc/v-split
           :style {:height :100%}
           :initial-split (or @v-split-pos 75)
           :on-split-change #(reset! v-split-pos %)
           :margin "0"
           :panel-1 [rc/h-split
                     :initial-split (or @h-split-pos 60)
                     :on-split-change #(reset! h-split-pos %)
                     :margin "0"
                     :panel-1 [:div.map-container [map/component doc] [view-control/component doc]]
                     :panel-2 [selection-info-panel/component doc]

                     ]
           :panel-2 [:div
                     {:style {:width :100%}}
                     [network-candidates-panel/component doc]]
           
           ])
        :component-did-mount
        (fn [arg]
          (let [vsplitter (.querySelector js/document ".re-v-split-splitter")
                listener
                (.addEventListener
                 vsplitter "dblclick"
                 (fn [e]
                   (let [container (.querySelector js/document ".rc-v-split")
                         container-height (.-offsetHeight container)
                         new-bottom-pane-height 0 ;; having the headers at the bottom looks weird

                         ;; (/ 4000 container-height) ;; 40px as % of container
                         new-top-pane-height (- 100 new-bottom-pane-height)
                         bottom-pane (.querySelector js/document ".re-v-split-bottom")
                         top-pane (.querySelector js/document ".re-v-split-top")]
                     (set! (.. top-pane -style -flex) (str new-top-pane-height " 1 0px"))
                     (set! (.. bottom-pane -style -flex) (str new-bottom-pane-height " 1 0px"))
                     (reset! v-split-pos new-top-pane-height))))]
            ;; Set these atoms so we can grab them to remove the listener when the
            ;; component is unmounted
            (reset! v-splitter-element vsplitter)
            (reset! v-splitter-dblclick-listener listener)))

        :component-will-unmount
        (fn []
          ;; Remove the double click event listener
          (.removeEventListener @v-splitter-element "dblclick" @v-splitter-dblclick-listener))
        })]))

(defn main-page []
  (r/with-let [*selected-tab (r/cursor state/state [::view/view-state ::view/selected-tab])
               *show-menu (r/atom false)
               *is-cooling (r/track #(document/is-cooling? @state/state))
               has-solution? (r/track #(document/has-solution? @state/state))
               has-valid-solution? (r/track #(solution/valid-state?
                                             (keyword (::solution/state @state/state))))]

    (let [close-popover
          (fn [e]
            (let [popover-menu-node (js/document.querySelector ".popover-menu")
                  click-is-outside-popover (and popover-menu-node
                                                (not (.contains popover-menu-node e.target)))
                  ]
              (when click-is-outside-popover (popover/close!))))

          close-table-filter
          (fn [e]
            (when (operations/table-filter-open? @state/state)
              (state/edit! state/state operations/close-table-filter)))

          menu-timer (atom nil)

          goto #(reset! *selected-tab %)
          selected-tab (or @*selected-tab :candidates)

          switcher (fn [key label]
                     [:li [:button.button--link-style
                           {:on-click #(goto key)
                            :class (when (= @*selected-tab key) "selected")}
                           label]])

          ]

      [:div {:style {:height :100% :width :100%
                     :display :flex
                     :flex-direction :column}
             :on-click (fn [e] (close-popover e) (close-table-filter e))
             :on-context-menu close-popover
             :ref popover/set-focus-element!}
       (when @*show-menu
         [:div.main-menu.slide-left.shadow.right
          {:style {:position :absolute
                   :z-index 1000000
                   :width :20em
                   :left :-20em
                   :flex-grow 1
                   :height "100%"
                   :display :flex
                   :flex-direction :column
                   :background "rgba(255,255,255,0.95)"}

           :on-mouse-enter
           #(js/clearTimeout @menu-timer)
           :on-mouse-leave
           #(reset! menu-timer
                    (js/setTimeout
                     (fn [] (reset! *show-menu false))
                     300))

           }
          
          [:div.menu-block
           [:h1 "Network problem"]
           [:ul
            [switcher :candidates "Map view"]
            [switcher :parameters "Objective"]
            
            [switcher :tariffs "Tariffs"]
            [switcher :pipe-costs "Pipe costs"]
            [switcher :insulation "Insulation"]
            [switcher :alternatives "Individual systems"]
            ]]

          (when @has-solution?
            [:div.menu-block
             [:h1 "Network solution"]
             [:ul
              [switcher :solution "Solution summary"]
              [switcher :run-log "Run log"]
              ]])
          
          [:div.menu-block
           [:h1 "Supply problem"]
           [:ul
            [switcher :profiles "Profiles"]
            [switcher :supply-technologies "Technologies"]
            [switcher :supply-objective "Objective"]
            ]]

          [:div.menu-block
           [:h1 "Help"]
           [:input.text-input
            {:placeholder "Search help..."
             :type :search
             :on-key-press
             #(let [key (.-key %)]
                (when (= key "Enter")
                  (->> (target-value %)
                       (str "/help/search?q=")
                       (js/window.open))))}
            ]


           [:ul
            [:li [:a {:href "/help" :target "help"} "Help contents"]]
            [:li [:a {:href "/help/networks.html" :target "help"} "Network editor help"]]]

           ]
          [:div.menu-block
           [:h1 "Project"]
           [:ul
            [:li [:a {:href "../../../"} "Back to project"]]
            [:li [:a {:href "/"} "THERMOS home page"]]
            ]
           ]

          ])

       (when-not (preload/get-value :read-only)
         [main-nav/component
          {:on-save (partial do-save false)
           :on-run (partial do-save true)

           :hamburger
           [:button.hamburger
            {:on-click #(swap! *show-menu not)
             :class (when (state/is-running?) "spin-around")
             :style (merge
                     {:background :none
                      :border :none}

                     (when @*is-cooling
                       {:transform "scaleY(-1)"}))
             }
            theme/icon]

           :name (preload/get-value :name)
           :unsaved? (state/needs-save?)}])

       (cond
         (state/is-running?)
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
           (let [position (state/queue-position)]
             (if (zero? position)
               [:span "Running"]
               [:span "Number " position " in queue"]))
           ]])

       [:div {:style {:height 1 :flex-grow 1
                      :overflow :auto
                      :display :flex
                      :flex-direction :column}}

        (cond
          (= selected-tab :candidates)
          [map-page state/state]

          (= selected-tab :parameters)
          [objective/objective-parameters state/state]

          (= selected-tab :tariffs)
          [tariff-parameters/tariff-parameters state/state]

          (= selected-tab :pipe-costs)
          [pipe-parameters/pipe-parameters state/state]

          (= selected-tab :insulation)
          [insulation/insulation-parameters state/state]

          (= selected-tab :alternatives)
          [alternatives/alternatives-parameters state/state]

          (= selected-tab :profiles)
          [profiles/profiles-parameters state/state]

          (= selected-tab :supply-technologies)
          [supply-technologies/supply-tech-parameters state/state]

          (= selected-tab :supply-objective)
          [supply-objective/supply-objective-parameters state/state]
          
          (= selected-tab :solution)
          [solution-view/solution-summary state/state]

          (= selected-tab :run-log)
          [solution-view/run-log state/state]

          :else
          [:div "Unknown page!!! urgh!"])]

       [popover/component state/state]
       [toaster/component]]
      )))

(defn on-js-reload [])

(defn mount-root []
  (r/render [main-page] (.getElementById js/document "app"))

  ;; Warn the user they have unsaved changes if they try to leave the page.
  (.addEventListener js/window "beforeunload"
                     (fn [e]
                       (when (state/needs-save?)
                         (let [msg "You have unsaved changes. Are you sure you want to leave the page?"]
                           (set! e.returnValue msg)
                           msg))))
  ;; unfortunately react event propagation seems to be weird
  ;; if we put the listener on the editor container, it doesn't
  ;; get the key events it should.
  (js/document.addEventListener "keypress" keys/handle-keypress))


;; (state/load-document! @document-identity mount-root)
(mount-root)

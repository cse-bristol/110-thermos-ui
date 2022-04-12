;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

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
            [thermos-frontend.supply-solution-view :as supply-solution-view]
            [thermos-frontend.merge-upload :as merge-upload]
            
            [thermos-frontend.solution-view :as solution-view]
            [thermos-frontend.toaster :as toaster]
            [thermos-frontend.editor-keys :as keys]
            [thermos-frontend.theme :as theme]

            [clojure.pprint :refer [pprint]]
            [clojure.string :as s]

            [goog.events :refer [listen]]
            [goog.math :refer [Size]]
            [goog.functions :refer [debounce]]
            [thermos-frontend.util :refer [target-value]]

            [thermos-frontend.flow]
            [thermos-frontend.splitter :refer [splitter]]
            [thermos-backend.content-migrations.messages :as migration-messages]

            [ajax.core :refer [POST]]
            [ajax.protocols :refer [-body]]

            [thermos-pages.symbols :as symbols]
            [thermos-specs.magic-fields :as magic-fields]))

(enable-console-print!)

(defn do-save [name run?]
  (let [can-run (document/is-runnable? @state/state)
        invalid (and run? (not can-run))]
    (when invalid
      (toaster/show!
       [:div.toaster.toaster--error
        "Cannot optimise until you have added some demands and supplies!"]))
    
    (when-not (and invalid (not (state/needs-save?)))
      (state/save!
       name
       :run (when (and run? can-run) run?)
       :callback
       #(cond
          (= % 403)
          (toaster/show! [:div.toaster.toaster--error "You have exceeded your weekly job quota"])

          (not invalid)
          (toaster/show! [:div.toaster.toaster--success "Project saved"]))))))


(defn map-page [doc flow]
  (r/with-let [h-split-pos (r/cursor doc
                                     [::view/view-state
                                      ::view/map-page-h-split]
                                     )
               v-split-pos (r/cursor doc
                                     [::view/view-state
                                      ::view/map-page-v-split]
                                     )
               v-splitter-element (atom nil)
               v-splitter-dblclick-listener (atom nil)
               ]
    [:<>
     
     [splitter
      {:axis :h
       :split (or @v-split-pos 75)
       :on-split-change #(reset! v-split-pos %)
       
       :top
       [splitter
        {:axis :v
         :split (or @h-split-pos 60)
         :on-split-change #(reset! h-split-pos %)
         :left
         [:div.map-container
          [map/component doc flow]
          [view-control/component doc]]
         
         :right
         [selection-info-panel/component flow]
         }]

       :bottom
       [:div
        {:style {:width :100%}}
        [network-candidates-panel/component flow]
        ]
       }
      ]

     
     ]
    ))

(defn main-page []
  (r/with-let [*selected-tab (r/cursor state/state [::view/view-state ::view/selected-tab])
               *show-menu (r/atom false)
               *is-cooling (r/track #(document/is-cooling? @state/state))
               has-solution? (r/track #(document/has-solution? @state/state))
               has-supply-solution? (r/track #(document/has-supply-solution? @state/state))
               has-valid-solution? (r/track #(solution/exists? @state/state))]

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
                   :overflow-y :auto
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
            [switcher :pipe-costs "Pipe & connection costs"]
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

          (when @has-supply-solution?
            [:div.menu-block
             [:h1 "Supply solution"]
             [:ul
              [switcher :supply-solution "Solution summary"]
              ]])

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
            [:li [:a {:href "/help/networks.html" :target "help"} "Network editor help"]]
            [:li [:button.button--link-style {:on-click keys/show-help}
                  "Keyboard shortcuts"]]]
           ]

          [:div.menu-block
           [:h1 "Import / Export Data"]
           [:ul
            [:li [:button.button--link-style
                  {:on-click
                   #(let [state (magic-fields/join (document/keep-interesting @state/state))]
                      (POST "/convert/excel"
                          {:params {:state state}
                           :response-format
                           {:type :blob :read -body}
                           :handler
                           (fn [blob]
                             (let [a (js/document.createElement "a")]
                               (set! (.-href a) (js/window.URL.createObjectURL blob))
                               (set! (.-download a)
                                     (str (preload/get-value :name) ".xlsx"))
                               (.dispatchEvent a (js/MouseEvent. "click"))))}))
                   
                   }
                  symbols/download " Excel Spreadsheet"]]

            [:li [:button.button--link-style
                  {:on-click merge-upload/do-upload}
                  symbols/upload " Excel Spreadsheet"]]
            
            [:li [:button.button--link-style
                  {:on-click
                   #(let [state (document/keep-interesting @state/state)]
                      (POST "/convert/json"
                          {:params {:state state}
                           :response-format
                           {:type :blob :read -body}

                           :handler
                           (fn [blob]
                             (let [a (js/document.createElement "a")]
                               (set! (.-href a) (js/window.URL.createObjectURL blob))
                               (set! (.-download a)
                                     (str (preload/get-value :name) ".json"))
                               (.dispatchEvent a (js/MouseEvent. "click"))))}))
                   }
                  symbols/download " Geojson"]]]]
          
          [:div.menu-block
           [:h1 "Project"]
           [:ul
            [:li [:a {:href "../../../"} "Back to project"]]
            [:li [:a {:href "/"} "THERMOS home page"]]
            ]]])

       [main-nav/component
        {:read-only (preload/get-value :read-only)
         :on-save #(do-save % nil)
         :on-run  do-save

         :hamburger
         [:button.hamburger
          {:on-click #(swap! *show-menu not)
           :class (when (state/is-running-or-queued?) "spin-around")
           :style (merge
                   {:background :none
                    :border :none}

                   (when @*is-cooling
                     {:transform "scaleY(-1)"}))
           }
          theme/icon]

         :name (preload/get-value :name)
         :unsaved? (state/needs-save?)}]

       (cond
         (state/is-running-or-queued?)
         [:div
          {:style {:position :absolute
                   :z-index 1000000
                   :width :100%
                   :height :100%
                   :display :flex
                   :background "rgba(0,0,0,0.75)"}}
          (let [position (state/queue-position)
                run-state (state/run-state)]
            [:div.popover-menu
             {:style {:background :white
                      :padding :1em
                      :margin :auto :width :75vw}}
             
             (case run-state
               (:cancel :cancelling)  [:b "Cancelling"]
               :running
               [:<>
                [:b "Model is running"]
                [:pre
                 {:style {:font-size :0.75em :overflow :auto}}
                 
                 (state/run-message)]
                ]

               [:span "Number " position " in queue"])
             [:a {:href "../../../"} "Back to project"]])])

       [:div {:style {:height 1 :flex-grow 1
                      :overflow :auto
                      :display :flex
                      :flex-direction :column}}

        (cond
          (= selected-tab :candidates)
          [map-page state/state state/flow]

          (= selected-tab :parameters)
          [objective/objective-parameters state/state]

          (= selected-tab :tariffs)
          [tariff-parameters/tariff-parameters state/state]

          (= selected-tab :pipe-costs)
          [pipe-parameters/pipe-parameters state/state state/flow]

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

          (= selected-tab :supply-solution)
          [supply-solution-view/supply-solution state/state]

          (= selected-tab :run-log)
          [solution-view/run-log state/state]

          :else
          [:div "Unknown page!!! urgh!"])]
       (when-let [messages
                  (seq @(thermos-frontend.flow/view* state/flow
                                                     ::document/migration-messages))]
         [:div {:style {:position :fixed :top 0 :left :0 :width :100% :height :100%
                        :z-index 2000
                        :display :flex :flex-direction :column
                        :background "rgb(1,1,1,0.5)"
                        }}
          [:div.card {:style {:margin-top :auto
                              :margin-bottom :auto
                              :margin-left :auto
                              :margin-right :auto
                              :max-height :50%
                              :max-width :75%
                              :overflow :auto
                              }}
           [:h1.card-header "Changes have been made to this problem"]
           [:p "THERMOS has been updated since this problem was saved, and the following changes have been made:"]

           [:div {:style {:overflow :auto}}
            (for [msg messages]
              [:div {:key msg}
               (migration-messages/messages msg)])]

           [:div {:style {:display :flex}}
            [:button.button 
             {:style {:margin-left :auto}
              :on-click #(thermos-frontend.flow/fire! state/flow
                                                      [dissoc ::document/migration-messages])}
             
             "OK"]]
           ]
          
          ]
         )
       [popover/component state/state]
       [toaster/component]]
      )))

(defn on-js-reload [])

(defn mount-root []
  (r/render [main-page] (.getElementById js/document "app"))

  ;; Warn the user they have unsaved changes if they try to leave the page.
  (when-not (preload/get-value :read-only)
    (.addEventListener js/window "beforeunload"
                       (fn [e]
                         (when (state/needs-save?)
                           (let [msg "You have unsaved changes. Are you sure you want to leave the page?"]
                             (set! e.returnValue msg)
                             msg)))))
  ;; unfortunately react event propagation seems to be weird
  ;; if we put the listener on the editor container, it doesn't
  ;; get the key events it should.
  )

(defonce keyhandler
  (js/document.addEventListener
   "keypress"
   keys/handle-keypress))

;; (state/load-document! @document-identity mount-root)
(mount-root)

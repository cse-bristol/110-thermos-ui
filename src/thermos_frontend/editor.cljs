;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.editor
  (:require [reagent.core :as r :refer [atom]]
            [reagent.dom.client :as rdc]
            [thermos-frontend.map :as map]
            [thermos-urls :as urls]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.solution :as solution]
            [thermos-specs.view :as view]
            [thermos-specs.document :as document]
            [thermos-frontend.editor-state :as state]
            [thermos-frontend.subs]
            [thermos-frontend.push-offer :as push-offer]
            [thermos-frontend.db :as db]
            [thermos-frontend.doc :as doc]
            [thermos-frontend.fx :as fx]
            [re-frame.core :as rf]
            [thermos-frontend.preload :as preload]

            [thermos-frontend.operations :as operations]
            [thermos-frontend.main-nav :as main-nav]
            [thermos-frontend.page-nav :as page-nav]
            [thermos-frontend.network-candidates-panel :as network-candidates-panel]
            [thermos-frontend.selection-info-panel :as selection-info-panel]
            [thermos-frontend.comparison-view :as comparison-view]
            [thermos-frontend.popover :as popover]
            [thermos-frontend.view-control :as view-control]
            [thermos-frontend.params.objective :as objective]
            [thermos-frontend.params.tariffs :as tariff-parameters]
            [thermos-frontend.params.pipes :as pipe-parameters]
            [thermos-frontend.params.insulation :as insulation]
            [thermos-frontend.params.alternatives :as alternatives]
            [thermos-frontend.params.profiles :as profiles]
            [thermos-frontend.params.supply-technologies :as supply-technologies]
            [thermos-frontend.params.supply-objective :as supply-objective]
            [thermos-frontend.params.supply-types :as supply-types]
            [thermos-frontend.supply-solution-view :as supply-solution-view]
            [thermos-frontend.merge-upload :as merge-upload]
            [thermos-frontend.parameter-sets :as parameter-sets]
            
            [thermos-frontend.solution-view :as solution-view]
            [thermos-frontend.toaster :as toaster]
            [thermos-frontend.format :as format]
            [thermos-frontend.editor-keys :as keys]
            [thermos-frontend.theme :as theme]

            [thermos-util.converter :as converter]
            
            [clojure.pprint :refer [pprint]]
            [clojure.string :as s]

            [goog.events :refer [listen]]
            [goog.math :refer [Size]]
            [goog.functions :refer [debounce]]
            [thermos-frontend.util :refer [target-value]]

            [thermos-frontend.splitter :refer [splitter]]
            [thermos-backend.content-migrations.messages :as migration-messages]

            [ajax.core :refer [POST]]
            [ajax.protocols :refer [-body]]

            [thermos-pages.symbols :as symbols]))

(enable-console-print!)

(rf/reg-event-fx
 ::save
 ;; save the document as `name`, and run it if `run?` (:network,
 ;; :supply, ...) and it can be run
 (fn [{db :db} [_ name run?]]
   (let [can-run (document/is-runnable? db)
         invalid (and run? (not can-run))]
     {:fx
      [(when invalid
         [::fx/call [toaster/show!
                     [:div.toaster.toaster--error
                      "Cannot optimise until you have added some demands and supplies!"]]])

       (when-not (and invalid (not (db/needs-save? db)))
         [:dispatch
          [:server/save name (when (and run? can-run) run?)
           #(cond
              (= % 403)
              (toaster/show! [:div.toaster.toaster--error "You have exceeded your weekly job quota"])

              (not invalid)
              (toaster/show! [:div.toaster.toaster--success "Project saved"]))]])]})))

(defn do-save [name run?]
  (rf/dispatch [::save name run?]))

(defn- download!
  "Post `network` to `url`, which converts it into a file, and download
  the file, named for the network with `extension`"
  ;; not called `document`, which would hide js/document
  [network url extension]
  (POST url
      {:params {:state network :name (preload/get-value :name)}
       :response-format
       {:type :blob :read -body}
       :handler
       (fn [blob]
         (let [a (js/document.createElement "a")]
           (set! (.-href a) (js/window.URL.createObjectURL blob))
           (set! (.-download a)
                 (str (preload/get-value :name) extension))
           (.dispatchEvent a (js/MouseEvent. "click"))))}))

(rf/reg-event-fx
 ::export
 (fn [{db :db} [_ url extension]]
   {:fx [[::fx/call [download! (document/keep-interesting db) url extension]]]}))


(defn map-page []
  (r/with-let [h-split-path [::view/view-state ::view/map-page-h-split]
               v-split-path [::view/view-state ::view/map-page-v-split]
               v-splitter-element (atom nil)
               v-splitter-dblclick-listener (atom nil)
               ]
    [:<>
     
     [splitter
      {:axis :h
       :split (doc/value v-split-path 75)
       :on-split-change #(doc/set-value! v-split-path %)
       
       :top
       [splitter
        {:axis :v
         :split (doc/value h-split-path 60)
         :on-split-change #(doc/set-value! h-split-path %)
         :left
         [:div.map-container
          [map/component]
          [view-control/component]]
         
         :right
         (if @(rf/subscribe [:compare/comparing])
           [comparison-view/panel]
           [selection-info-panel/component])
         }]

       :bottom
       [:div
        {:style {:width :100%}}
        [network-candidates-panel/component]
        ]
       }
      ]

     
     ]
    ))

(defn- upload-geojson-2!
  "Ask for a geojson v2 file, and update the document from it"
  []
  (let [f (js/document.createElement "input")]
    (set! (.-type f) "file")
    (.addEventListener
     f "change"
     (fn [e]
       (let [r (js/FileReader.)]
         (set!
          (.-onload r)
          (fn [e]
            (->> e
                 (.-target)
                 (.-result)
                 (js/JSON.parse)
                 (js->clj)
                 (vector :doc/edit converter/update-from-geojson-2)
                 (rf/dispatch))))
         (.readAsText r (-> e (.-target) (.-files) (aget 0))))))
    (.dispatchEvent f (js/MouseEvent. "click"))))

(defn- file-menu [set-mode]
  [main-nav/dropdown {:label "File"}
   [:div.dropdown__heading "Export"]
   [:button.dropdown__item
    {:on-click #(rf/dispatch [::export "/convert/excel" ".xlsx"])}
    symbols/download " Excel spreadsheet"]
   (when-not set-mode
     [:<>
      [:button.dropdown__item
       {:on-click #(rf/dispatch [::export "/convert/gpkg" ".gpkg"])
        :title "The network and its results, as a GIS file for QGIS and the like"}
       symbols/download " GeoPackage"]
      [:button.dropdown__item
       {:on-click #(rf/dispatch [::export "/convert/json" ".json"])}
       symbols/download " Geojson"]
      [:button.dropdown__item
       {:on-click #(rf/dispatch [::export "/convert/json-2" ".json"])}
       symbols/download " Geojson v2"]])

   [:div.dropdown__heading "Import"]
   [:button.dropdown__item {:on-click merge-upload/do-upload}
    symbols/upload " Excel spreadsheet"]
   (when-not set-mode
     [:button.dropdown__item {:on-click upload-geojson-2!}
      symbols/upload " Geojson v2"])

   (when-not set-mode
     [:<>
      [:div.dropdown__heading "Parameter sets"]
      [:button.dropdown__item {:on-click parameter-sets/show-apply-dialog!}
       "Apply a parameter set..."]
      (when-not (preload/get-value :read-only)
        [:button.dropdown__item {:on-click parameter-sets/show-save-dialog!}
         "Save parameters as a set..."])
      [:a.dropdown__item {:href "/parameters" :target "_blank"} "All parameter sets"]

      [:div.dropdown__heading "Compare"]
      [:button.dropdown__item {:on-click comparison-view/show-chooser!}
       "Compare with another network..."]
      (when @(rf/subscribe [:compare/comparing])
        [:button.dropdown__item {:on-click #(rf/dispatch [:compare/stop])}
         "Stop comparing"])])

   (when-not (or set-mode (preload/get-value :read-only))
     [:<>
      [:div.dropdown__heading "Tools"]
      [:button.dropdown__item
       {:on-click #(main-nav/with-name (fn [name] (do-save name :tidy)))
        :title "Save, and mark the parts of the network that cannot be reached"}
       "Tidy"]
      [:button.dropdown__item
       {:on-click #(main-nav/with-name (fn [name] (do-save name :tree)))
        :title "Save, and join up the buildings with a quick approximate tree, without optimising"}
       "Tree"]])

   [:div.dropdown__heading "Go to"]
   (if set-mode
     [:a.dropdown__item {:href "/parameters"} "Parameter sets"]
     [:a.dropdown__item {:href "../../../"} "Project"])
   [:a.dropdown__item {:href "/"} "THERMOS home page"]])

(defn- help-menu []
  [main-nav/dropdown {:label "Help"}
   [:input.text-input
    {:placeholder "Search help..."
     :type :search
     :style {:margin "0.25em 1em" :width "calc(100% - 2em)"}
     :on-key-press
     #(when (= "Enter" (.-key %))
        (->> (target-value %)
             (str "/help/search?q=")
             (js/window.open)))}]
   [:a.dropdown__item {:href "/help" :target "help"} "Help contents"]
   [:a.dropdown__item {:href "/help/networks.html" :target "help"} "Network editor help"]
   [:button.dropdown__item {:on-click keys/show-help} "Keyboard shortcuts"]])

(defn navbar [*is-cooling]
  (let [set-mode (some? (db/parameter-set))]
    [main-nav/component
     {:key :main-nav
      :read-only (preload/get-value :read-only)
      :parameter-set set-mode
      :on-save #(do-save % nil)
      :on-run  do-save

      :home
      [:a.main-nav__home
       {:href (if set-mode "/parameters" "../../../")
        :title (if set-mode "Back to parameter sets" "Back to project")
        :class (when @(rf/subscribe [:server/running-or-queued]) "spin-around")
        :style (when (= :cooling @*is-cooling)
                 {:transform "scaleY(-1)"})}
       theme/icon]

      :menus [[file-menu set-mode] [help-menu]]

      :name (preload/get-value :name)
      :unsaved? @(rf/subscribe [:server/needs-save])}]))

(defn- rough-duration
  "A duration in seconds, to the nearest minute or so"
  [s]
  (let [m (js/Math.round (/ s 60))]
    (cond
      (< s 60)  "less than a minute"
      (< m 60)  (str m " min")
      :else     (let [h (js/Math.floor (/ m 60))
                      m (mod m 60)]
                  (str h " h" (when (pos? m) (str " " m " min")))))))

(defn- clock-time
  "The time `s` seconds from now, with the day if it is not today"
  [s]
  (let [then (js/Date. (+ (js/Date.now) (* 1000 s)))]
    (if (= (.toDateString then) (.toDateString (js/Date.)))
      (.toLocaleTimeString then js/undefined #js {:hour "2-digit" :minute "2-digit"})
      (.toLocaleString then js/undefined #js {:weekday "short" :hour "2-digit" :minute "2-digit"}))))

(defn- last-line
  "The last line of `message` with something on it"
  [message]
  (->> (s/split-lines (str message))
       (remove s/blank?)
       (last)))

(defn- run-log
  "The model's log, as a summary showing its latest line, which opens to
  the whole log in a box which scrolls, following new lines unless the
  user has scrolled up to look at something"
  [_]
  (let [*element (clojure.core/atom nil)
        *follow  (clojure.core/atom true)
        follow!  (fn [] (when-let [e @*element]
                          (when @*follow (set! (.-scrollTop e) (.-scrollHeight e)))))]
    (r/create-class
     {:component-did-mount follow!
      :component-did-update follow!
      :reagent-render
      (fn [message]
        [:details {:style {:margin "0.5em 0" :min-height 0}
                   ;; the log can't be scrolled while it is closed, so
                   ;; catch up when it is opened
                   :on-toggle follow!}
         [:summary {:style {:cursor :pointer
                            :white-space :nowrap
                            :overflow :hidden
                            :text-overflow :ellipsis
                            :font-family :monospace
                            :font-size :0.85em}}
          (or (last-line message) "Starting...")]
         [:pre
          {:ref #(reset! *element %)
           :on-scroll (fn [e]
                        (let [e (.-target e)]
                          (reset! *follow (< (- (.-scrollHeight e)
                                                (.-scrollTop e)
                                                (.-clientHeight e))
                                             20))))
           :style {:font-size :0.75em
                   :overflow :auto
                   :max-height :50vh
                   :margin "0.5em 0 0 0"
                   :padding :0.5em
                   :background "#f4f4f4"}}
          message]])})))

(defn- run-progress [{:keys [elapsed expected-runtime finishes-in]}]
  (when (and elapsed expected-runtime (pos? expected-runtime))
    [:div {:style {:margin "0.5em 0"}}
     [:progress {:style {:width :100%}
                 :max expected-runtime
                 :value (min elapsed expected-runtime)}]
     [:div {:style {:display :flex :justify-content :space-between :font-size :0.9em}}
      [:span (rough-duration elapsed) " of up to " (rough-duration expected-runtime)]
      (if (pos? finishes-in)
        [:span "Should finish by " (clock-time finishes-in)]
        [:span "Time limit reached, finishing up"])]]))

(defn- objective-at
  "The objective of the best solution as it was `t` seconds into the run,
  from `objectives`, [[seconds objective] ...] in time order"
  [objectives t]
  (second (last (take-while #(<= (first %) t) objectives))))

(defn- objective-chart
  "How the objective of the best solution so far has improved, as a step
  chart going on to `elapsed` seconds, so the user can see whether it is
  still getting better, and so whether to stop the run"
  [objectives elapsed]
  (r/with-let [*hover (r/atom nil)] ;; the time pointed at, in seconds
    (let [width   600
          height  100
          pad     4
          [t-last o-last] (peek objectives)
          t-max   (max 1 elapsed t-last)
          ys      (map second objectives)
          y-min   (reduce min ys)
          y-max   (reduce max ys)
          ;; a flat line goes across the middle
          span    (if (> y-max y-min) (- y-max y-min) (max 1 (js/Math.abs y-max)))
          y-low   (if (> y-max y-min) y-min (- y-min (/ span 2)))
          sx      #(* width (/ % t-max))
          sy      #(- height pad (* (- height pad pad) (/ (- % y-low) span)))
          [[t0 o0] & more] objectives
          path    (str "M" (sx t0) " " (sy o0)
                       (apply str (for [[t o] more] (str " H" (sx t) " V" (sy o))))
                       " H" width)
          hover   @*hover
          hover-o (when hover (objective-at objectives hover))
          label   {:font-size :0.75em :color theme/dark-grey}]
      [:div {:style {:margin "0.5em 0"}}
       [:div {:style {:display :flex :justify-content :space-between :font-size :0.9em}}
        [:span "Objective of the best solution so far"]
        (if hover-o
          [:span (format/si-number hover-o) " after " (format/seconds hover)]
          [:span (format/si-number o-last) ", found after " (format/seconds t-last)])]
       [:div {:style {:display :flex :align-items :stretch :margin-top :0.25em}}
        [:div {:style (merge label {:width :4em :padding-right :0.5em :text-align :right
                                    :display :flex :flex-direction :column
                                    :justify-content :space-between})}
         [:span (format/si-number (if (> y-max y-min) y-max (+ y-low span)))]
         [:span (format/si-number y-low)]]
        [:svg {:view-box (str "0 0 " width " " height)
               :preserve-aspect-ratio "none"
               :role :img
               :aria-label (str "Objective over time, now " (format/si-number o-last))
               :style {:flex 1 :height :6em :display :block :cursor :crosshair}
               :on-mouse-move (fn [e]
                                (let [r (.getBoundingClientRect (.-currentTarget e))]
                                  (reset! *hover (* t-max (/ (- (.-clientX e) (.-left r))
                                                             (.-width r))))))
               :on-mouse-leave #(reset! *hover nil)}
         [:path {:d (str "M0 0 V" height " H" width)
                 :fill :none :stroke theme/light-grey :stroke-width 1
                 :vector-effect "non-scaling-stroke"}]
         (when hover
           [:path {:d (str "M" (sx hover) " 0 V" height)
                   :stroke theme/dark-grey :stroke-width 1 :stroke-dasharray "3 3"
                   :vector-effect "non-scaling-stroke"}])
         [:path {:d path :fill :none
                 :stroke theme/in-solution-orange :stroke-width 2
                 :stroke-linejoin :round
                 :vector-effect "non-scaling-stroke"}]]]
       [:div {:style (merge label {:display :flex :justify-content :space-between
                                   :margin-left :4.5em})}
        [:span "0"]
        [:span (rough-duration t-max)]]])))

(defn- run-buttons
  "What the user can do while the network is queued or running: stop it,
  or leave it to run and edit a copy"
  [run-state]
  (let [stopping (#{:cancel :cancelling} run-state)]
    [:div {:style {:display :flex :gap :1em :align-items :center :margin "0.5em 0"}}
     (when-not stopping
       [:button.button.button--danger
        {:on-click #(rf/dispatch [:server/stop])
         :title (if (= :running run-state)
                  "Stop the optimiser now, and use the best solution it has found so far"
                  "Take this off the queue without running it")}
        (if (= :running run-state) "Stop" "Cancel")])
     [:button.button
      {:on-click #(rf/dispatch [:server/continue-editing])
       :title "Leave this to finish by itself, and carry on editing a copy of it"}
      "Continue editing"]
     [:a {:href "../../../" :style {:margin-left :auto}} "Back to project"]]))

(defn- run-estimate [position {:keys [starts-in finishes-in]}]
  [:div
   [:b "Number " position " in queue"]
   (when (and starts-in finishes-in)
     [:p (if (< starts-in 60)
           "Should start shortly"
           (str "Should start within " (rough-duration starts-in)))
      ", and finish by " (clock-time finishes-in) "."
      [:br]
      [:small "This assumes every job ahead uses its whole time limit, so it may well be sooner."]])])

(defn main-page []
  (r/with-let [selected-tab-path [::view/view-state ::view/selected-tab]
               *is-cooling (rf/subscribe [:doc/mode])]

    (let [close-popover
          (fn [e]
            (let [popover-menu-node (js/document.querySelector ".popover-menu")
                  click-is-outside-popover (and popover-menu-node
                                                (not (.contains popover-menu-node e.target)))
                  ]
              (when click-is-outside-popover (popover/close!))))

          close-table-filter
          (fn [e]
            (rf/dispatch [:table/close-filter]))

          set-mode (some? (db/parameter-set))

          selected-tab (doc/value selected-tab-path :candidates)

          ;; a parameter set has no map or solution
          selected-tab (if (and set-mode (#{:candidates :solution :run-log :supply-solution} selected-tab))
                         :parameters
                         selected-tab)
          ]

      [:div {:style {:height :100% :width :100%
                     :display :flex
                     :flex-direction :column}
             :on-click (fn [e] (close-popover e) (close-table-filter e))
             :on-context-menu close-popover
             :ref popover/set-focus-element!}
       ^{:key :navbar} [navbar *is-cooling]

       ^{:key :page-nav} [page-nav/component selected-tab]

       (cond
         @(rf/subscribe [:server/running-or-queued])
         [:div {:key :run-state-overlay
                :style {:position :absolute
                        :z-index 1000000
                        :width :100%
                        :height :100%
                        :display :flex
                        :background "rgba(0,0,0,0.75)"}}
          (let [{position :queue-position
                 timing :run-timing
                 objectives :run-objectives
                 :keys [run-state run-message]} @(rf/subscribe [:server/run])]
            [:div.popover-menu
             {:style {:background :white
                      :padding :1em
                      :margin :auto :width :75vw
                      :max-height :85vh
                      :display :flex
                      :flex-direction :column}}

             (case run-state
               (:cancel :cancelling)
               [:<>
                [:b "Stopping"]
                [:p "The optimiser is finishing up with the best solution it has found so far."]
                (when (seq objectives)
                  [objective-chart objectives (:elapsed timing 0)])
                [run-log run-message]]

               :running
               [:<>
                [:b "Model is running"]
                [run-progress timing]
                (when (seq objectives)
                  [objective-chart objectives (:elapsed timing 0)])
                [run-log run-message]]

               [run-estimate (:position timing position) timing])
             (when-not (#{:cancel :cancelling} run-state)
               [push-offer/component])
             [run-buttons run-state]])])

       (if set-mode
         ^{:key :set-editor-bar} [parameter-sets/set-editor-bar]
         ^{:key :changed-banner} [parameter-sets/changed-banner])

       ^{:key :comparison-banner} [comparison-view/banner]

       [:div {:style {:key :main-body
                      :height 1 :flex-grow 1
                      :overflow :auto
                      :display :flex
                      :flex-direction :column}}

        (cond
          (= selected-tab :candidates)
          [map-page]

          (= selected-tab :parameters)
          [objective/objective-parameters]

          (= selected-tab :tariffs)
          [tariff-parameters/tariff-parameters]

          (= selected-tab :pipe-costs)
          [pipe-parameters/pipe-parameters]

          (= selected-tab :insulation)
          [insulation/insulation-parameters]

          (= selected-tab :alternatives)
          [alternatives/alternatives-parameters]

          (= selected-tab :supply-types)
          [supply-types/supply-types-parameters]

          (= selected-tab :profiles)
          [profiles/profiles-parameters]

          (= selected-tab :supply-technologies)
          [supply-technologies/supply-tech-parameters]

          (= selected-tab :supply-objective)
          [supply-objective/supply-objective-parameters]
          
          (= selected-tab :solution)
          [solution-view/solution-summary]

          (= selected-tab :supply-solution)
          [supply-solution-view/supply-solution]

          (= selected-tab :run-log)
          [solution-view/run-log]

          :else
          [:div "Unknown page!!! urgh!"])]
       (when-let [messages
                  (seq @(rf/subscribe [:doc/migration-messages]))]
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
              :on-click #(rf/dispatch [:doc/dismiss-migration-messages])}
             
             "OK"]]
           ]
          
          ]
         )
       [popover/component]
       [toaster/component]]
      )))

(defonce ^:private root
  (delay (rdc/create-root (.getElementById js/document "app"))))

(defn ^:dev/after-load mount-root []
  (rdc/render @root [main-page]))

(defn init
  "The entry point, called once when the page loads"
  []
  (mount-root)

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
  ;; (the indirection lets hot reloads of editor-keys take effect)
  (js/document.addEventListener "keypress" #(keys/handle-keypress %))
  (js/document.addEventListener "keydown" #(keys/handle-keydown %)))

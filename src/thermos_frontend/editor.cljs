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
            [thermos-frontend.model-parameters :as model-parameters]
            [thermos-frontend.solution-view :as solution-view]
            [thermos-frontend.toaster :as toaster]
            [thermos-frontend.editor-keys :as keys]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as s]
            [goog.ui.SplitPane :refer [SplitPane Component]]
            [goog.events :refer [listen]]
            [goog.math :refer [Size]]
            [goog.functions :refer [debounce]]

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
     :style {:height :100%}
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
               has-solution? (r/track #(document/has-solution? @state/state))]
    
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
          ]
      [:div.editor__container
       {:on-click (fn [e] (close-popover e) (close-table-filter e))
        :on-context-menu close-popover
        :ref popover/set-focus-element!}

       [main-nav/component
        {:on-save (partial do-save false)
         :on-run (partial do-save true)
         :on-tab-switch #(reset! selected-tab %)
         :name (preload/get-value :name)
         :unsaved? (state/needs-save?)
         :selected-tab (or @selected-tab :candidates)
         :tabs
         [
          {:key :candidates :label "Map"}
          {:key :parameters :label "Options"}
          (when @has-solution? {:key :solution :label "Result"})
          {:key :help :label "Help" :href "/help"}
          ]
         }]

       (when (state/is-running?)
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

       [:div {:style {:overflow-y :auto
                      :flex 1}}
        (case (or @selected-tab :candidates)
          :candidates
          [map-page state/state]

          :parameters
          [model-parameters/parameter-editor state/state]

          :solution
          [solution-view/component state/state])]
       
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

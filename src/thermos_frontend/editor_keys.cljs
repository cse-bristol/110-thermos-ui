(ns thermos-frontend.editor-keys
  (:require [thermos-frontend.editor-state :as state]
            [thermos-frontend.operations :as operations]
            [thermos-frontend.popover :as popover]
            
            [thermos-frontend.spatial :as spatial]
            [thermos-specs.candidate :as candidate]

            [clojure.pprint :as pprint]
            
            [thermos-frontend.supply-parameters :as supply-parameters]
            [thermos-frontend.candidate-editor :as candidate-editor]
            
            [goog.object :as o]))

(defn- rotate-inclusion! []
  (state/edit!
   state/state
   operations/rotate-candidates-inclusion
   (operations/selected-candidates-ids @state/state)))

(defn- edit-supply! []
  (->> @state/state
       (operations/selected-candidates)
       (filter candidate/is-building?)
       (map ::candidate/id)
       (supply-parameters/show-editor! state/state)))

(defn edit-demand-or-path! []
  (->> @state/state
       (operations/selected-candidates)
       (map ::candidate/id)
       (candidate-editor/show-editor! state/state)))

(defn- zoom-to-fit! []
  (state/edit!
   state/state
   spatial/zoom-to-selection))

(defn- select-all! []
  (state/edit! state/state operations/select-candidates
               (map ::candidate/id
                    (operations/get-filtered-candidates @state/state))
               :replace))

(defn- show-pprint-thing []
  (popover/open! state/state
                 [:div {:style {:background "white" :width :50% :height :50% :display :block
                                :min-width :500px
                                }}
                  [:textarea {:style {:width :100% :height :300px}}
                   (with-out-str
                     (pprint/pprint
                      (operations/selected-candidates @state/state)))]
                  [:button
                   {:on-click #(popover/close! state/state)}
                   "OK"]
                  ]
                 :middle))

(defn handle-keypress [e]
  (case (.-key e)
    "c" (rotate-inclusion!)
    "s" (edit-supply!)
    "z" (zoom-to-fit!)
    "a" (select-all!)
    "e" (edit-demand-or-path!)
    "i" (show-pprint-thing)
    
    :default))

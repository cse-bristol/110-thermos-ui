(ns thermos-ui.frontend.editor-keys
  (:require [thermos-ui.frontend.editor-state :as state]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.spatial :as spatial]
            [thermos-ui.specs.candidate :as candidate]

            [thermos-ui.frontend.supply-parameters :as supply-parameters]
            [thermos-ui.frontend.candidate-editor :as candidate-editor]
            
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

(defn handle-keypress [e]
  (case (.-key e)
    "c" (rotate-inclusion!)
    "s" (edit-supply!)
    "z" (zoom-to-fit!)
    "a" (select-all!)
    "e" (edit-demand-or-path!)
    
    :default))

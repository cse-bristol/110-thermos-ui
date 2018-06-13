(ns thermos-ui.frontend.editor-keys
  (:require [thermos-ui.frontend.editor-state :as state]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.specs.candidate :as candidate]))

(defn- rotate-inclusion! []
  (state/edit!
   state/state
   operations/rotate-candidates-inclusion
   (operations/selected-candidates-ids @state/state)))

(defn- rotate-type! []
  (let [selection (operations/selected-candidates @state/state)]
    (when (= 1 (count selection))
      (let [selection (first selection)
            type (::candidate/type selection)]
        (when (#{:demand :supply} type)
          (state/edit!
           state/state
           operations/set-candidate-type
           (::candidate/id selection)
           (if (= :demand type) :supply :demand))
          )))))

(defn- zoom-to-fit! [])

(defn handle-keypress [e]
  (case (.-key e)
    "c" (rotate-inclusion!)
    "t" (rotate-type!)
    "z" (zoom-to-fit!)
    :default))

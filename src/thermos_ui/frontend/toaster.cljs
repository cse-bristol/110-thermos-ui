(ns thermos-ui.frontend.toaster
  (:require [reagent.core :as r]
            [thermos-ui.specs.view :as view]))

(defn component
  "Component for displaying flash messages that disappear after a short delay,
  e.g. for notifying the user of a successful save."
  [doc]
  (let [showing (->> @doc ::view/view-state ::view/toaster ::view/toaster-showing)
        content (->> @doc ::view/view-state ::view/toaster ::view/toaster-content)
        class   (->> @doc ::view/view-state ::view/toaster ::view/toaster-class)]
    [:div.toaster__container {:class (if showing "toaster__container--showing")}
     [:div.toaster {:class class}
      "Save successful" content]]))

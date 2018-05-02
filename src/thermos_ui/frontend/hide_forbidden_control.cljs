(ns thermos-ui.frontend.hide-forbidden-control
    (:require
      [reagent.core :as reagent]
      [thermos-ui.frontend.editor-state :as state]
      [thermos-ui.specs.document :as document]
      [thermos-ui.specs.view :as view]
      [thermos-ui.specs.candidate :as candidate]
      [thermos-ui.frontend.operations :as operations]))

(defn component
  [leaflet-map]
  (let [show-forbidden? (->> @state/state ::view/view-state ::view/show-forbidden)]
    (if (nil? show-forbidden?)
      (state/edit! state/state assoc-in [::view/view-state ::view/show-forbidden] true))
    [:div.leaflet-touch.leaflet-bar
     [:input {:type "checkbox"
              :id "hide-forbidden-candidates"
              :style {:display "none"}
              :checked (not show-forbidden?)
              :on-change (fn [e] (state/edit! state/state
                                              assoc-in
                                              [::view/view-state ::view/show-forbidden]
                                              (not show-forbidden?))
                           (if show-forbidden? ;; I.e. we are changing from show forbidden -> don't show forbidden
                             ;; Remove the forbidden candidates from candidates
                             (let [constrained-candidates-map
                                   (into {} (map (fn [cand] [(::candidate/id cand) cand])
                                                 (operations/constrained-candidates @state/state)))]
                               (state/edit-geometry! state/state assoc ::document/candidates constrained-candidates-map))
                             ;; Add the candidates back in when showing forbidden
                             (.eachLayer leaflet-map
                                         (fn [layer]
                                           (when (= (.-internalId layer) "candidates-layer")
                                             (.repaintInPlace layer)
                                             )))
                             ))
              }]
     [:label.hide-forbidden-control {:for "hide-forbidden-candidates"
                                     :title (if show-forbidden?
                                              "Hide unconstrained candidates"
                                              "Show unconstrained candidates")}
      [:span.hide-forbidden-icon]]]))

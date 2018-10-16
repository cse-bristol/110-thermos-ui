(ns thermos-ui.frontend.solution-view
  (:require [thermos-ui.specs.solution :as solution]
            [thermos-ui.specs.document :as document]
            [thermos-ui.specs.technology :as technology]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.frontend.virtual-table :as table]
            [thermos-ui.frontend.format :refer [si-number]]
            [reagent.core :as reagent]

            )
  )

(defn- render-error [s]
  [:div
   [:h1 "The optimisation model has encountered an error"]
   [:p "This should not happen."]
   (for [log (::solution/log s)]
     [:pre log])])

(defn- render-infeasible [s]
  [:div
   [:h1 "The optimisation model was unable to find a solution"]
   [:p "Producing an explanation for this is a bit difficult, but for now:"]
   [:ul
    [:li "Is every required demand location reachable from a supply location?"]
    [:li "Are there any emissions limits? They may be too severe."]
    [:li "Is there enough supply capacity allowed to meet all required demands?"]]])

(defn- tiles
  "Inefficient and rubbish implementation of a quantiling algorithm.
  TODO this ought to be weighted n-tile"
  [vals n]
  (let [vals (sort vals)
        psize (max 1 (int (/ (count vals) n)))
        parts (partition psize vals)
        ]
    (map #(vector (first %) (last %)) parts)))

(defn- bin
  ([caps]
   (fn [x] (bin x caps)))
  ([val caps]
   (first
    (filter #(<= (first %) val (second %)) caps))))

(defn- render-solution [document]
  (reagent/with-let [summary (reagent/cursor document [::solution/summary])
                     candidates (reagent/cursor document [::document/candidates])
                     candidates (reagent/track #(filter candidate/is-in-solution? (vals @candidates)))
                     ]
    (let [summary @summary
          {paths :path
           buildings :building} (group-by ::candidate/type @candidates)

          bands (tiles (map ::solution/path-capacity paths) 10)
          band (bin bands)
          
          paths-by-capacity (group-by
                             (fn [path]
                               (band (::solution/path-capacity path)))
                             paths)

          ]
      ;; (println bands)
      [:div.results_view
       [:h1 "The optimisation model has found a solution"]

       [:div
        [:h2 "Summary"]
        [:span "NPV: " (::solution/objective-value summary)]
        ]

       [:div
        [:h2 "Supply"]
        [:table
         [:thead
          [:tr
           [:th "Name"]
           [:th "Plant"]
           [:th "Count"]
           [:th "Capacity"]
           
           [:th "Heat output"]
           [:th "Fuel input"]
           [:th "Power output"]

           [:th "Capital cost"]
           [:th "Fuel cost"]
           [:th "Power revenue"]]]
         [:tbody
          (for [building buildings
                plant (::solution/plant building)]
            [:tr
             [:td (::candidate/name building)]
             [:td (::technology/id plant)]
             [:td (::solution/count plant)]
             [:td (::solution/capacity plant)]
             [:td (si-number (* 1000 (::solution/heat-output plant)))]
             [:td (si-number (* 1000 (::solution/fuel-input plant)))]
             [:td (si-number (* 1000 (::solution/power-output plant)))]
             [:td (si-number (::solution/capital-cost plant))]
             [:td (si-number (::solution/fuel-cost plant))]
             ])]]]

       [:div
        [:h2 "Demand"]
        [:table
         [:thead
          [:tr
           [:th "Name"]
           [:th "Heat demand (Wh/yr)"]
           [:th "Revenue (¤)"]]]
         [:tbody
          (for [b buildings]
            [:tr
             [:td (::candidate/name b)]
             [:td (si-number (* 1000 (::candidate/demand b)))]
             [:td (si-number (::solution/heat-revenue b))]])]]
        ]

       [:div
        [:h2  "Network"]
        [:table
         [:thead
          [:tr [:th "Capacity (W)"] [:th "Length (m)"] [:th "Cost (¤)"] [:th "Losses (Wh/year)"]]]
         [:tbody
          (for [[[l u] ps] paths-by-capacity]
            [:tr {:key (str l u)}
             [:td
              (if (= l u)
                (si-number (* 1000 l) )
                (str (si-number (* 1000 l)) " - " (* 1000 u)))]
             [:td (si-number (apply + (map ::candidate/length ps)))]
             [:td (si-number (apply + (map ::solution/capital-cost ps)))]])
          ]]]])))

(defn component [document]
  (reagent/with-let [summary (reagent/cursor document [::solution/summary])]
    (let [summary @summary]
      (case (::solution/state summary)
        :error (render-error summary)
        :infeasible (render-infeasible summary)
        
        (render-solution document)))))

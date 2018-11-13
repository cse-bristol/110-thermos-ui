(ns thermos-frontend.solution-view
  (:require [thermos-specs.solution :as solution]
            [thermos-specs.demand :as demand]
            [thermos-specs.path :as path]
            [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [reagent.core :as reagent]
            [thermos-frontend.format :as format]))

(defn- solution-not-found []
  [:div
   [:p [:b "No solution found"] " for this problem"]
   [:p "A solution may exist, but one could not be found with the resources available."]
   [:p "Reduce the problem size, or increase the maximum running time or allowable distance from the best possible answer."]])

(defn- solution-infeasible []
  [:div
   [:p "The problem is " [:b "infeasible"] "."]
   [:p "This means that no solution exists for this problem."]
   [:p "Possible explanations for this are:"]
   [:ol
    [:li
     [:p "There are required demands which are not reachable from any supply location."]]
    
    [:li
     [:p "The total peak demand in some part of the problem exceeds the maximum supply capacity connectable to it."]]

    [:li
     [:p "The emissions limits cannot be achieved."]]]])

(defn- solution-exists [document]
  (reagent/with-let [candidates (reagent/cursor document [::document/candidates])
                     solution-members (reagent/track #(filter candidate/in-solution? (vals @candidates)))]
    (let [solution-members @solution-members
          {buildings :building paths :path} (group-by ::candidate/type solution-members)

          path-groups
          (group-by #(int (/ (::solution/capacity-kw %) 100)) paths)
          ]
      [:div
       [:h1 (::solution/state @document) " solution found in " (format/seconds (::solution/runtime @document))]

       [:p "The solution has objective value " (format/si-number (::solution/objective @document))]

       [:h2 "Demands"]
       [:table
        [:thead
         [:tr
          [:th "Name"]
          [:th "Demand (Wh/yr)"]
          [:th "Connection size (W)"]
          [:th "Revenue (NPV)"]]]
        
        [:tbody
         (for [building buildings
               :when (candidate/has-demand? building)]
           [:tr {:key (::candidate/id building)}
            [:td (::candidate/name building)]
            [:td (format/si-number (* 1000 (::demand/kwh building)))]
            [:td (format/si-number (* 1000 (::demand/kwp building)))]
            [:td (format/si-number (::solution/demand-npv building))]])]]

       [:h2 "Supplies"]
       [:table
        [:thead
         [:tr
          [:th "Name"]
          [:th "Capacity (W)"]
          [:th "Output (Wh/yr)"]
          [:th "Capital cost (NPV)"]
          [:th "Operating cost (NPV)"]
          [:th "Heat cost (NPV)"]
          ]]
        [:tbody
         (for [building buildings
               :when (candidate/has-supply? building)]
           [:tr {:key (::candidate/id building)}
            [:td (::candidate/name building)]
            [:td (format/si-number (* 1000 (::demand/kwh btuilding)))]
            [:td (format/si-number (* 1000 (::demand/kwp building)))]
            [:td (format/si-number (::solution/demand-npv building))]
            [:td "TODO"]])]]
       [:h2 "Network"]
       [:table
        [:thead
         [:tr
          [:th "Capacity (W)"]
          [:th "Length (m)"]
          [:th "Capital cost (NPV)"]]]
        [:tbody
         ;; want to group by capacity, perhaps rounded
         (for [[cap ps] path-groups]
           [:tr {:key cap}
            [:td (* 100 cap) "-" (* 100 (+ cap 1))]
            [:td (format/si-number (reduce + (map ::path/length ps)))]
            [:td (format/si-number (reduce + (map ::solution/capex-npv ps)))]
            ]
           )
         ]
        ]
       
       ])))

(defn- unknown-state []
  [:div
   [:p "The optimisation has produced an unexpected outcome."]
   [:p "This is probably a bug in the application."]])

(defn component [document]
  [:div.solution-component
   (case (keyword (::solution/state @document))
     :infeasible [solution-infeasible]
     ::noSolution [solution-not-found]
     (:valid :feasible :optimal :globallyOptimal :locallyOptimal :maxIterations :maxTimeLimit)
     [solution-exists document]

     [unknown-state])])


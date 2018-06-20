(ns thermos-ui.frontend.solution
  (:require [thermos-ui.specs.solution :as solution]
            [thermos-ui.specs.document :as document]
            [thermos-ui.specs.technology :as technology]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.frontend.virtual-table :as table]
            [thermos-ui.frontend.format :refer [si-number]]
            [reagent.core :as reagent]

            )
  )

(defn component [document]
  (reagent/with-let [solution (reagent/cursor document [::solution/solution])
                     candidates (reagent/cursor document [::document/candidates])
                     included-candidates (reagent/track
                                          #(filter
                                           (comp ::solution/included ::solution/candidate)
                                           (vals @candidates)))

                     technologies (reagent/cursor document [::document/technologies])

                     box (fn [& content]
                           [:span {:style
                                   {:background :#555
                                    :margin :0.5em
                                    :padding :0.5em
                                    :color :#fff
                                    :border-radius :0.5em
                                    :display :inline-block
                                    }}
                            content]
                           )
                     ]

    [:div.results_view
     (let [{status ::solution/status
            value ::solution/objective-value
            runtime ::solution/runtime
            metrics ::solution/metrics} @solution

           {supplies :supply
            demands :demand
            paths :path} (group-by ::candidate/type @included-candidates)

           plant (group-by
                  :process
                  (mapcat (comp ::solution/technologies
                                ::solution/candidate)
                          supplies))


           tidy-class (fn [class]
                        (if (or (= "" class)
                                (nil? class))
                          "Unclassified"
                          class))

           add-up (fn [vals & [scale]]
                    (si-number
                     (* (or scale 1) (reduce + 0 vals))))

           ]
       (case status
         (:feasible :optimal)
         [:div
          [:h1 "Objective value " (si-number (* 1000 value)) ", solution " (name status)
           " (" (/ runtime 1000) "s)"
           ]
          
          [:h1 "Metrics"]

          (let [rows (group-by :category metrics)
                metric-types [["capex" 1000] ["opex" 1000] ["ghg" 1]]
                ]
            [:table
             [:thead
              [:tr
               [:th ""]
               (for [[k _] metric-types] [:th {:key k} k])]]
             [:tbody
              (for [[row-name cells] rows]
                [:tr {:key row-name}
                 [:td [:b row-name]]

                 (let [cells (group-by :metric cells)]
                   (for [[m scale] metric-types]
                     [:td {:key m} 
                      (si-number (* scale (get-in cells [m 0 :value])))
                      ]
                     ))])]])

          [:h1 "Network"]

          [:h2 "Supplies"]
          [:table
           [:thead
            [:tr
             [:th "Technology"]
             [:th "Capacity"]
             [:th "Capital cost"]
             [:th "Heat output"]
             [:th "Power output"]
             [:th "Fuel input"]
             ]]
           [:tbody
            (let [technologies @technologies]
              (for [[technology places] plant]
                (let [tech (->> technologies
                                (filter (comp (partial = technology)
                                              ::technology/id))
                                (first))

                      ;; so in here, it looks like the output flow from a
                      ;; technology is restricted by the resource balance
                      ;; so that even if we would make money by selling
                      ;; more power we can't unless we need the heat.

                      ;; TODO capacity rule is probably backwards
                      
                      count (reduce + 0 (map (comp js/parseInt :count) places))
                      rate (reduce + 0 (map (comp js/parseFloat :production) places))

                      heat-led
                      (or (nil? (::technology/power-efficiency tech))
                          (= 0 (::technology/power-efficiency tech)))

                      fuel-input
                      (* 1000000 ;; megawatts
                         8766 ;; hours per year
                         (/ rate (if heat-led
                                   (::technology/heat-efficiency tech)
                                   (::technology/power-efficiency tech))))
                      
                      ;; in a heat led system, the heat output is 1 * the rate
                      ;; otherwise it is fuel input * heat efficiency
                      heat-output
                      (* fuel-input (::technology/heat-efficiency tech))

                      power-output
                      (* fuel-input (::technology/power-efficiency tech))

                      _ (println power-output
                                 fuel-input
                                 (::technology/power-efficiency tech))
                      ]
                  [:tr {:key technology}
                   [:td technology]
                   
                   [:td (si-number (* 1000000 count (::technology/capacity tech)))]
                   [:td (si-number (* count (::technology/capital-cost tech)))]
                   [:td (si-number heat-output) "Wh/yr"]
                   [:td (si-number power-output) "Wh/yr"]
                   [:td (si-number fuel-input) "Wh/yr"]
                   ])
                ))
            ]
           ;; need these data
           ]
          [:h2 "Pipework"]

          [:table
           [:thead
            [:tr
             [:th "Classification"]
             [:th "Length"]
             [:th "Capital cost"]
             ]]

           [:tbody
            (for [{class :subtype length :length cost :cost}
                  (->> paths
                       (group-by (comp tidy-class ::candidate/subtype))
                       (map (fn [[k ps]] {:subtype k
                                          :length (reduce + 0 (map ::candidate/length ps))
                                          :cost (reduce + 0 (map ::candidate/path-cost ps))}))
                       (sort-by :cost)
                       (reverse))]
              
              [:tr {:key class}
               [:td class]
               [:td (si-number length)]
               [:td (si-number cost)]])]]
          
          [:h2 "Demands"]
          [:table
           [:thead
            [:tr
             [:th "Classification"]
             [:th "Count"]
             [:th "Heat demand"]
             ;; [:th "Revenue"]
             ;; need this one
             ]
            ]
           [:tbody
            (for [{class :subtype c :count d :demand}
                  (->> demands
                       (group-by (comp tidy-class ::candidate/subtype))
                       (map (fn [[k ds]] {:subtype k
                                          :count (count ds)
                                          :demand (* 1000 (reduce + 0 (map ::candidate/demand demands)))}))
                       (sort-by :demand)
                       (reverse))
                  ]
              [:tr {:key class}
               [:td class]
               [:td c]
               [:td (si-number d ) "Wh/yr"]])]]]

         [:div
          [:h1 "No solution found"]
          [:pre (str @solution)]]
         
         )

       )
     ]
    ))

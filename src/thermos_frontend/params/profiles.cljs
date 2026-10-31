;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.params.profiles
  (:require [reagent.core :as reagent]
            [re-frame.core :as rf]
            [thermos-frontend.doc :as doc]
            [clojure.string :as string]
            [thermos-pages.symbols :as syms]
            [thermos-frontend.inputs :as inputs]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.supply :as supply]
            [goog.object :as o]
            [clojure.pprint]
            [thermos-util :refer-macros [for-map]]
            [clojure.string :as s]

            [reagent.core :as r]))

(defn- add-tab-button [{:keys [placeholder on-add-tab]
                        :or {placeholder "Add"}}]
  (reagent/with-let [active (reagent/atom false)]
    [:li.tabs__tab
     {:on-click #(reset! active true)}
     (if @active
       [:input.input
        {:placeholder placeholder
         :ref #(and % (.focus %))
         :on-blur #(reset! active false)
         :on-key-up
         #(when (= (.-key %) "Enter")
            (and on-add-tab (on-add-tab (-> % .-target .-value)))
            (reset! active false))
         }]
       "+")]))

(defn- put-vec
  "Put x into vector v at index i, extending v if needs be and padding with zeroes."
  [v i x]
  (let [v (vec v)]
    (assoc
     (if (<= i (count v))
       v
       (vec (take i (concat v (repeat 0)))))
     i x)))

(defn- assoc-in-v [m ks v]
  (let [lk (last ks)
        ks (drop-last ks)]
    (if (empty? ks)
      (put-vec m lk v)
      (update-in m ks
                 put-vec lk v))))

(defn- day-type-tabs
  "A tab strip which lets you choose and create day types"
  [{:keys [on-select on-create on-delete on-rename day-types selected]}]
  (reagent/with-let [editing-name (reagent/atom false)]
    [:ul.tabs__tabs.tabs__tabs--pills
     ;; the list of tabs for days
     (let [show-edit @editing-name]
       (for [[day-type {day-name :name}] (sort-by (comp - :frequency second) day-types)]
         (let [is-selected (= selected day-type)]
           [:li.tabs__tab {:key day-type}
            (if (and is-selected show-edit)
              [:input.input
               {:placeholder "Day type name"
                :default-value day-name
                :ref #(and % (doto % (.focus) (.select)))
                :on-blur #(reset! editing-name false)
                :on-key-up
                #(when (= (.-key %) "Enter")
                   (and on-rename (on-rename selected (-> % .-target .-value)))
                   (reset! editing-name false))
                }]
              [:span.tabs__tab
               {:on-click (if is-selected
                            #(reset! editing-name true)
                            #(on-select day-type))
                :class (when is-selected "tabs__tab--active")}
               day-name])
            (when is-selected
              [:button.button--link-style
               {:on-click (and on-delete #(on-delete selected))}
               syms/delete])
            
            ])))
     
     ;; the add button
     [add-tab-button {:placeholder "New day type"
                      :on-add-tab on-create}]]))

(defn- numtext [atts]
  
  (let [inner-value (reagent/atom (:value atts))]
    (reagent/create-class
     {:display-name "numtext"
      :component-did-update
      (fn [this old-argv]
        (let [old-value (:value (second old-argv))
              new-value (:value (second (reagent/argv this)))]
          (println (reagent/props this))
          (when (not= old-value new-value)
            (reset! inner-value new-value))))
      
      :reagent-render
      (fn [atts]
        [:input {:type        :text
                 :value       @inner-value
                 :on-blur     #(reset! inner-value (:value atts))
                 :on-change   #(do
                                   (reset! inner-value (-> % .-target .-value))
                                   (let [val (-> % .-target .-value
                                                 (js/parseFloat))]
                                     (when (and (js/isFinite val)
                                                (:on-change atts))
                                       ((:on-change atts) val))))
                 :placeholder (:placeholder atts)
                 :style       (:style atts)}
         ])})))

(defn- day-type-parameters [day-type]
  (let [day-types       (doc/value [::supply/day-types])
        divisions       (get-in day-types [day-type :divisions])
        frequency       (get-in day-types [day-type :frequency])
        total-frequency (reduce + 0 (map :frequency (vals day-types)))]
    [:div
     [:label "Relative frequency: "
      [inputs/number {:path [::supply/day-types day-type :frequency]
                      :key day-type ;; this is on here to make it
                      ;; re-render. Otherwise reagent sees it as same element
                      :max 365
                      :min 0
                      :step 1}]
      " of " total-frequency " (or ~"
      (int (* 365 (/ (double frequency)
                     (double total-frequency))))
      " days per year)"]
     
     
     [:label {:style {:margin-left :1em}}
      "Time precision: "
      [inputs/parsed
       {:value divisions
        :type :number
        :class :number-input
        :min 1
        :max 48
        :step 1
        :parse (fn [t]
                 (let [t (js/parseInt t)]
                   (when (and (js/isFinite t) (> t 0)) t)))
        :on-blur (fn [e v] (rf/dispatch [:doc/edit supply/change-divisions day-type v]))
        :on-key-down
        (fn [e]
          (when (= e.key "Enter")
            (rf/dispatch [:doc/edit supply/change-divisions day-type (.. e -target -parsedValue)])))
        }
       ]
      ]]))


(defn- tabular-block [{:keys [columns
                              row-label
                              cell-values
                              data
                              scale
                              on-change
                              on-add-row
                              key
                              ]
                       :or {scale 1}}]
  [:<>
   [:div {:style {:display               :grid
                  :grid-template-columns (str "max-content " (s/join " " (repeat columns "2.5em")))}}
    (doall
     (for [[row-id data] data
           :let          [cell-values (cell-values data)]]
       (list
        [:div {:key row-id :style {:width :10em}}
         (row-label row-id data)]
        (for [i (range columns)]
          [inputs/parsed {:type        :text
                          :key i
                          :style       {:width :100% :border :none :border-radius 0}
                          :parse       (fn [val]
                                         (let [val (js/parseFloat val)]
                                           (when (js/isFinite val) val)))
                          :on-blur     (fn [e val] (on-change
                                                    row-id i
                                                    (/ val scale)))
                          :on-key-down (fn [e val]
                                         (when (= (.-key e) "=")
                                           (dotimes [i columns]
                                             (on-change row-id i (/ val scale)))
                                           (.preventDefault e)))

                          :placeholder (* scale (get cell-values i 0))
                          :value       (* scale (get cell-values i 0))}]))))]
   (when on-add-row
     [add-tab-button {:placeholder "Name"
                      :on-add-tab  on-add-row}])])

(defn- zeroes
  "A block of zeroes suitable for the day types in `document`"
  [document]
  (for-map [[day-type {d :divisions}] (::supply/day-types document)]
           day-type (vec (repeat d 0.0))))

(defn- shown-day-type
  "The day type to show, given the `selected` one, which is nil before one
  has been chosen, or :latest to show the latest, as after adding one"
  [day-types selected]
  (cond
    (contains? day-types selected) selected
    (empty? day-types)             nil
    (nil? selected)                (first (keys day-types))
    :else                          (apply max (keys day-types))))

(defn profiles-parameters []
  (reagent/with-let [selected-day-type (reagent/atom nil)

                     fuel-block
                     (fn [{:keys [scale today key on-add-row]}]
                       [tabular-block
                        {:key     [key today]
                         :scale   scale
                         :columns (:divisions (get (doc/value [::supply/day-types]) today) 1)
                         :row-label
                         (fn [fuel-id x]
                           [:div.flex-cols {:style {:max-width :100%}}
                            [:button.button--link-style
                             {:style    {:margin-left :auto}
                              :on-click #(rf/dispatch [:doc/edit supply/remove-fuel fuel-id])}
                             syms/delete]
                            
                            [:input {:value     (:name x)
                                     :style     {:flex-shrink 1 :flex-grow 1 :width 1
                                                 :border-radius 0 :border :none}
                                     :on-change #(doc/update! [::supply/fuels] assoc-in [fuel-id :name] (.. % -target -value))}]]
                           )
                         :cell-values (fn [x] (get (key x) today))
                         :data        (doc/value [::supply/fuels])
                         :on-change   (fn [fuel division val]
                                        (doc/update! [::supply/fuels] assoc-in-v [fuel key today division] val))
                         :on-add-row  on-add-row
                         }])

                     add-fuel-row
                     (fn [name]
                       (rf/dispatch
                        [:doc/edit
                         #(supply/add-fuel % {:name  name
                                              :price (zeroes %)
                                              :pm25  (zeroes %)
                                              :co2   (zeroes %)
                                              :nox   (zeroes %)})]))

                     substation-load-block
                     (fn [{:keys [today]}]
                       [tabular-block
                        {:columns (:divisions (get (doc/value [::supply/day-types]) today) 1)
                         :scale (/ 1000.0)
                         :data (doc/value [::supply/substations])
                         :row-label
                         (fn [sub-id sub-data]
                           (:name sub-data (str "Substation " sub-id)))
                         :cell-values
                         (fn [sub-data]
                           (get (:load-kw sub-data) today))
                         :on-change
                         (fn [sub-id hh val]
                           (doc/update! [::supply/substations] assoc-in-v [sub-id :load-kw today hh] val))
                         }
                        
                        ]
                       )
                     
                     heat-profile-block
                     (fn [{:keys [today]}]
                       [tabular-block
                        {:key     [:heat today] ;; this causes react to redraw content on day switch
                         :columns (:divisions (get (doc/value [::supply/day-types]) today) 1)
                         :row-label
                         (fn [profile-id x]
                           [:div.flex-cols {:style {:max-width :100%}}
                            [:button.button--link-style
                             {:style    {:margin-left :auto}
                              :on-click #(rf/dispatch [:doc/edit supply/remove-profile profile-id])}
                             syms/delete]
                            
                            [:input {:value     (:name x)
                                     :style     {:flex-shrink 1 :flex-grow 1 :width 1
                                                 :border-radius 0 :border :none}
                                     
                                     :on-change #(doc/update! [::supply/heat-profiles] assoc-in [profile-id :name] (.. % -target -value))}]]
                           )
                         
                         :cell-values (fn [x] (get (:demand x) today))
                         :data        (doc/value [::supply/heat-profiles])
                         :on-change   (fn [profile division val]
                                        (doc/update! [::supply/heat-profiles] assoc-in-v [profile :demand today division] val))
                         :on-add-row
                         (fn [name]
                           (rf/dispatch
                            [:doc/edit
                             #(supply/add-profile % {:name   name
                                                     :demand (zeroes %)})]))}])
                     ]
   (let [day-types (doc/value [::supply/day-types])
         today     (shown-day-type day-types @selected-day-type)]
    [:div.card.flex-grow.parameters-component
     [:h1.card-header "Default profile"]
     [:div "This profile will be used for buildings where you have not set a profile: "
      [inputs/select {:values
                      (for [[profile-id {name :name}] (doc/value [::supply/heat-profiles])]
                        [profile-id name])
                      :value (or (doc/value [::supply/default-profile]) (min (keys (doc/value [::supply/heat-profiles]))))
                      :on-change #(doc/set-value! [::supply/default-profile] %)
                      }]
      ]
     
     [:h1.card-header "Day types"]
     [day-type-tabs
      {:day-types day-types :selected today
       :on-select #(reset! selected-day-type %)
       :on-create #(do
                     (rf/dispatch [:doc/edit supply/add-day-type {:name % :frequency 1 :divisions 24}])
                     (reset! selected-day-type :latest))
       :on-delete (fn [id]
                    (rf/dispatch [:doc/edit supply/remove-day-type id])
                    (reset! selected-day-type :latest))
       :on-rename (fn [day-type new-name]
                    (doc/set-value! [::supply/day-types day-type :name] new-name))
       }]

     [day-type-parameters today]

     [:div {:style {:padding-top :1em
                    :padding-bottom :1em}}
      "Press the " [:b "="] " key to set all values in a row to the focused cell."]
     
     (let [columns-today (:divisions (get day-types today) 1)]

       [:div {:style {:overflow-x :auto}}
        [:h1.card-header "Heat profiles"]
        
        [heat-profile-block {:today today}]
        
        [:h1.card-header "Fuels"]

        [:div {:style {:margin-top :1em :margin-bottom :0.5em :font-size :1.25em}}
         [:b  "Prices"]
         [:span " — c/kWh"]]
        [tabular-block
         {:key         [:elec today]
          :columns     columns-today
          :scale       100.0
          :row-label   (constantly "Grid offer")
          :cell-values (fn [x] (get x today))
          :data        [[0 (doc/value [::supply/grid-offer])]]
          :on-change   (fn [_ division val]
                         (doc/update! [::supply/grid-offer] assoc-in-v [today division] val))}]
        
        [fuel-block {:key        :price
                     :scale      100.0
                     :today      today
                     :columns    columns-today
                     :on-add-row add-fuel-row}]

        [:div {:style {:margin-bottom :0.5em :font-size :1.25em}}
         [:b  "CO₂ Emissions"]
         [:span " — " (candidate/emissions-factor-units :co2)]]
        
        [fuel-block {:key :co2 :today today
                     :scale (candidate/emissions-factor-scales :co2)}]
        
        [:div {:style {:margin-top :1em :margin-bottom :0.5em :font-size :1.25em}}
         [:b "NOₓ Emissions"]
         [:span " — " (candidate/emissions-factor-units :nox)]]
        
        [fuel-block {:key :nox :today today
                     :scale (candidate/emissions-factor-scales :nox)}]

        [:div {:style {:margin-top :1em :margin-bottom :0.5em :font-size :1.25em}}
         [:b "PM" [:sub "2.5"] " Emissions "]
         [:span " — " (candidate/emissions-factor-units :pm25)]]
        
        [fuel-block {:key :pm25 :today today
                     :scale (candidate/emissions-factor-scales :pm25)
                     }]


        (when (seq (doc/value [::supply/substations]))
          [:<>
           [:h1.card-header {:style {:margin-top :1em}} "Substation load — MW"]
           [substation-load-block {:today today}]])
        
        ])
     ])))

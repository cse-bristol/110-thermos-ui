(ns thermos-ui.frontend.network-candidates-panel
  (:require [reagent.core :as reagent]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.virtual-table :as virtual-table]))

(declare component get-row-function)

(defn component
  "DOCSTRING"
  [document]
  (let [items @(reagent/track (fn [] (operations/selected-candidates @document)))]
    [:div {:style {:height "100%"}}
    ; [:div {:style {:height "50%" :border-bottom "1px solid red"}}
    ;
    ;   [:> js/ReactVirtualized.AutoSizer
    ;    (fn [dimensions]
    ;      (let [height (.-height dimensions)
    ;            width (.-width dimensions)]
    ;        (reagent/as-element
    ;         (into
    ;          [:> js/ReactVirtualized.Table
    ;           {:headerHeight 50
    ;            :height height
    ;            :rowCount (count items)
    ;            :rowGetter (fn [arg]
    ;                         (let [index (.-index arg)
    ;                               item (nth items index)]
    ;                           (clj->js item)
    ;                           ; {(name ::candidate/id) (::candidate/id item)
    ;                           ;  (name ::candidate/postcode) (::candidate/postcode item)}
    ;                           ))
    ;            :rowHeight 50
    ;            :sort (fn [arg] (println arg))
    ;            :width width}
    ;           ]
    ;          [[:> js/ReactVirtualized.Column {:dataKey (name ::candidate/id)
    ;                                           :key (name ::candidate/id)
    ;                                           :flexGrow 1
    ;                                           :width 5
    ;                                           :label "Id"}]
    ;           [:> js/ReactVirtualized.Column {:dataKey (name ::candidate/postcode)
    ;                                           :key (name ::candidate/postcode)
    ;                                           :flexGrow 1
    ;                                           :width 5
    ;                                           :label "Postcode"}]]
    ;          ))))]]

     ;; Attempt at virtual-table component
     [virtual-table/component
      {:columns [{:key ::candidate/id
                  :label "ID"
                  :sortable true}
                 {:key ::candidate/postcode
                  :label "Postcode"
                  :sortable true}]
       :items items
       :props {}}]

     ]))


; (defn ui []
;   (let [!st (r/atom nil)]
;     (fn [{:keys [ks on-row-click show-fn]} items]
;       (let [{:keys [sortBy sortDirection]} @!st
;             sorted-items @(r/track sort-by-col
;                                    items
;                                    (some-> sortBy keyword)
;                                    (= "DESC" sortDirection))]
;         [:> js/ReactVirtualized.AutoSizer
;          (fn [m]
;            (r/as-element (into
;                           [:> js/ReactVirtualized.FlexTable
;                            (cond-> {:height 800
;                                     :className "inspector"
;                                     :width (aget m "width")
;                                     :headerHeight 70
;                                     :rowHeight 30
;                                     :rowCount (count sorted-items)
;                                     :rowClassName (fn [m]
;                                                     (when (odd? (aget m "index"))
;                                                       "table-odd"))
;                                     :rowGetter (fn [m]
;                                                  (get sorted-items (aget m "index")))
;                                     :sort #(reset! !st (js->clj % :keywordize-keys true))
;                                     :sortBy (:sortBy @!st)
;                                     :sortDirection (:sortDirection @!st)}
;                                    on-row-click
;                                    (assoc :on-row-click (comp on-row-click sorted-items :index mapify)))]
;                           (map (fn [k]
;                                  [:> js/ReactVirtualized.FlexColumn
;                                   {:key (name k)
;                                    :label (name k)
;                                    :dataKey (kw->str k)
;                                    :cellDataGetter (fn [m]
;                                                      (fmt-value (get (aget m "rowData") (keyword (aget m "dataKey")))))
;                                    :width 200}])
;                                (filter (or show-fn (constantly true)) ks)))))]))))

(ns thermos-ui.frontend.virtual-table
  (:require [reagent.core :as reagent :refer-macros [with-let]]
            [cljsjs.react]

            [cljsjs.react-virtualized]))

(declare component table generate-column sort-table)

(defn component
  "A virtual table wrapper. Takes any of the virtualised table
  properties in PROPS, as well as :items, which is a vector whose
  entries hold the data for each row. COLUMNS is then a series of
  maps, which should have at least :label and :key.

  The value of the column for a given row will be found by looking up
  the column's :key in that row's datum. Other properies in a column
  will be passed onto the virtualised column.

  Stores the sort order as a bit of internal state.
  "
  [{items :items :as props} & columns]
  ;; reagent/with-let allows us to define an atom whose lifecycle
  ;; follows that of the component, so it's not recreated when we re-render
  (reagent/with-let [sort-order (reagent/atom [nil nil])
                     ;; we can use the sort-order atom now to define
                     ;; our sort function.
                     sort! (fn [items]
                             (let [[sort-col sort-dir] @sort-order
                                   sort-col (cljs.reader/read-string sort-col)]
                               (if sort-col
                                 ((if (= "ASC" sort-dir) identity reverse)
                                  (sort-by sort-col items))
                                 items)))
                     ]
    ;; and now the actual component
    (let [[sort-col sort-dir] @sort-order
          sorted-items (sort! items)]
      [:> js/ReactVirtualized.AutoSizer
       (fn [dims]
         (reagent/as-element
          [:> js/ReactVirtualized.Table
           (merge
            {:rowCount (count sorted-items)
             :rowGetter #(nth sorted-items (.-index %))
             :rowHeight 50
             :headerHeight 50
             :sortBy sort-col
             :sortDirection sort-dir
             :sort #(reset! sort-order [(.-sortBy %) (.-sortDirection %)])
             }
            props
            (js->clj dims :keywordize-keys true))
           (for [{key :key :as col} columns]
             ^{:key key} ;; this is to make reagent shut up about :key
                         ;; props, I am not sure how it works but it
                         ;; does.
             [:> js/ReactVirtualized.Column
              (merge
               {:key (str key) ;; this is needed to so that the sort
                               ;; column has the fully qualified name
                :disableSort false
                :width 100
                :dataKey (str key)
                :flexGrow 1
                :cellDataGetter #(get (.-rowData %) key)
                }
               col)])
           ]))])))

(ns thermos-ui.frontend.virtual-table
  (:require [reagent.core :as reagent :refer-macros [with-let]]
            [clojure.string :as s]
            [cljsjs.react]
            [cljsjs.react-virtualized]
            [goog.object :as o]
            ))

(declare component filter!)

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
  [{items :items
    filters :filters}
   count-filtered-items ;; This lets the parent component know how many items have been filtered
   & columns]
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
          sorted-items (sort! items)
          sorted-filtered-items (filter! sorted-items filters)]
      (reset! count-filtered-items (count sorted-filtered-items))
      [:> js/ReactVirtualized.AutoSizer
       (fn [dims]
         (reagent/as-element
          [:> js/ReactVirtualized.Table
           (merge
            {:rowCount (count sorted-filtered-items)
             :rowGetter #(nth sorted-filtered-items (.-index %))
             :rowHeight 50
             :headerHeight 50
             :sortBy sort-col
             :sortDirection sort-dir
             :sort #(reset! sort-order [(.-sortBy %) (.-sortDirection %)])
             }
            {:items items}
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
                :cellDataGetter #(get (o/get % "rowData") key)
                }
               col)])
           ]))])))

(defn filter!
  "Function to filter items if a filter is supplied, otherwise just returns items"
 [items filters]
  (if (not-empty filters)
    (filter
     (fn [item]
       (reduce-kv
        (fn [init k v]
          (if init
            (if (string? v)
              ;; If the filter is a string then check if the item matches the string in a fuzzy way.
              (let [regex-pattern (re-pattern (str "(?i)" (s/join ".*" (s/split v #"")) ".*"))]
                (not-empty
                 (re-seq regex-pattern (k item))))
              ;; If the filter is a set of values the check if the item matches one of them exactly.
              (or (empty? v) (contains? v (k item)))
              )
            false))
        true
        filters))
     items)
    items))

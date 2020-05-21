(ns thermos-frontend.virtual-table
  (:require [reagent.core :as reagent :refer-macros [with-let]]
            [clojure.string :as s]
            [cljsjs.react]
            [cljsjs.react-virtualized]
            [goog.object :as o]
            [thermos-frontend.format :refer [si-number]]
            ))

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
  [{items :items}
   & columns]
  ;; reagent/with-let allows us to define an atom whose lifecycle
  ;; follows that of the component, so it's not recreated when we re-render
  (reagent/with-let [sort-order (reagent/atom [nil nil])
                     ;; we can use the sort-order atom now to define
                     ;; our sort function.
                     sort! (fn [items]
                             (let [[sort-col sort-dir] @sort-order
                                   sort-col (cljs.reader/read-string sort-col)
                                   sort-col (if (and (not (nil? sort-col))
                                                     (seqable? sort-col))
                                              #(get-in % sort-col)
                                              sort-col)]
                               (if sort-col
                                 ((if (= "ASC" sort-dir) identity reverse)
                                  (sort-by sort-col items))
                                 items)))]
    ;; and now the actual component
    (let [[sort-col sort-dir] @sort-order
          items (sort! items)]

      [:> js/ReactVirtualized.AutoSizer
       (fn [dims]
         (reagent/as-element
          [:> js/ReactVirtualized.Table
           (merge
            {:rowCount (count items)
             :rowGetter #(nth items (.-index %))
             :rowHeight 35
             :headerHeight 40
             :sortBy sort-col
             :sortDirection sort-dir
             :sort #(reset! sort-order [(.-sortBy %) (.-sortDirection %)])
             }
            {:items items}
            (js->clj dims :keywordize-keys true))
           (for [{key :key :as col} (remove nil? columns)]
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
                :cellRenderer (fn [arg] (str (o/get arg "cellData")))
                :cellDataGetter #(if (seqable? key)
                                   (get-in (o/get % "rowData") key)
                                   (get (o/get % "rowData") key))
                }
               col
               )])
           ]))])))


(defn render-keyword [arg] (if-let [v (o/get arg "cellData")]
                             (name v)
                             ""))

(defn render-number [& {:keys [unit scale]
                        :or {unit "" scale 1}}]
  (fn [arg]
    (let [x (o/get arg "cellData")]
      (when x
        (str (si-number (* scale x)) unit)))))


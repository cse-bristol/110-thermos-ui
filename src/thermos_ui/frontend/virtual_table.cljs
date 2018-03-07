(ns thermos-ui.frontend.virtual-table
  (:require [reagent.core :as reagent]
            [cljsjs.react]

            [cljsjs.react-virtualized]))

(declare component table generate-column)

(defn component
  "Wrapper for the react-virtualized Table component.
  Automatically wraps it in the AutoSizer component and allows you to specify the columns as a collection of maps.
  The component takes the following keyword arguments:

  `columns` REQUIRED (map) The definition of the columns the table should have, defined as a collection of maps.
      Each column should have the following props:
      `key`      REQUIRED (str)  The key that identifies the column in the collection of data.
                                 If you pass in a namespaced keyword this will automatically get converted into
                                 a string, as the row data will get converted into JS objects.
      `label`    REQUIRED (str)  The table header to be displayed for this column.
      `sortable` OPTIONAL (bool) Whether or not this column is sortable. Defaults to false.
      `props`    OPTIONAL (map)  Additional props to be passed in to the Column constructor, see
                                 https://github.com/bvaughn/react-virtualized/blob/master/docs/Column.md
                                 If either `dataKey`, `label` or `disableSort` are entered here, they will be
                                 override the higher level definitions `key`, `label` and `sortable`.
                                 Sensible defaults are used for other properties where necessary.

  `items` REQUIRED (coll) Collection of data to be rendered in the table.

  `props` OPTIONAL (map) Optional props to be passed to the Table constructor, see
                         https://github.com/bvaughn/react-virtualized/blob/master/docs/Table.md
  "
  [{columns :columns
    items :items
    props :props}]
    [:> js/ReactVirtualized.AutoSizer
     (fn [dimensions]
       (let [height (.-height dimensions)
             width (.-width dimensions)]
         (table columns items props height width)))])

(defn table
  "Generates the table component."
  [columns items props height width]
  (this-as this
           (let [
                 ; component-state ()]
                 sorted-items (reagent/atom (clj->js items))
                 sort (reagent/atom {:sortDirection "ASC"})]
             (println "RENDERING")
             (reagent/as-element
              (into
               [:> js/ReactVirtualized.Table
                (merge
                 ;; Default values for props
                 {:className "virtual-table"
                  :headerHeight 50
                  :height height
                  :overscanRowCount 5
                  :rowCount (count @sorted-items)
                  :rowGetter (fn [arg]
                               (let [index (.-index arg)]
                                 (nth @sorted-items index)))
                  :rowHeight 50
                  :sort (fn [arg] (let [sortBy (.-sortBy arg)
                                        sortDirection (.-sortDirection arg)
                                        sort-fn (if (= sortDirection "ASC") < >)]
                                    (println arg)
                                    (reset! sorted-items (sort-by (fn [item] (aget item sortBy)) sort-fn @sorted-items))
                                    (println (map (fn [x] (x "postcode")) (js->clj @sorted-items)))
                                    (.forceUpdateGrid this)
                                    ; (swap! sort assoc :sortDirection (if (= sortDirection "ASC") "DESC" "ASC"))
                                    )) ;; @TODO 1. Figure out why it only allows ASC
                                       ;; 2. Figure out why it only re-renders when you scroll
                  ; :sortBy "id"
                  ; :sortDirection (@sort :sortDirection)
                  :width width}
                 ;; Custom props
                 props
                 )]
               (map generate-column columns))))))

(defn generate-column
  "Generates a column from the config map."
  [{key :key
    label :label
    sortable :sortable
    props :props}]
  [:> js/ReactVirtualized.Column
   (merge
    ;; Default props
    {:dataKey (name key)
     :key (name key)
     :flexGrow 1
     :width 100
     :label label
     :disableSort (not sortable)}
    ;; Custom props
    props
    )])

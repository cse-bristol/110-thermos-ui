(ns thermos-frontend.popover-menu
  (:require [reagent.core :as reagent]))

(declare component menu-item on-item-click)

(defn component
  [items]
  (let [open-sub-menu (reagent/atom nil)]
    (reagent/create-class
     {:reagent-render
      (fn []
        [:div.popover-menu
         [:ul.popover-menu__list
          (doall (map (fn [item] (menu-item item open-sub-menu)) items))]])
      })))

(defn menu-item
  [item open-sub-menu]

  [:li {:class (str "popover-menu__item"
                    (if (:sub-menu item) " popover-menu__item--has-sub-menu")
                    (if (or (:on-select item) (:sub-menu item)) " popover-menu__item--clickable"))
        :on-click (fn [e] (on-item-click e item open-sub-menu))
        :key (:key item)
        ; :ref (:ref item)
        }
   (if (and (some? (:sub-menu item)) (= (:key item) @open-sub-menu))
     [component (:sub-menu item)])
   (:value item)])

(defn on-item-click
  [e item open-sub-menu]
  (if (= @open-sub-menu (:key item))
    (reset! open-sub-menu nil)
    (reset! open-sub-menu (:key item)))
  ;; If the item has an on-select handler, invoke it
  (if (:on-select item)
    ((:on-select item) e item)))

(ns thermos-frontend.inputs
  (:require [reagent.core :as reagent]
            [clojure.set :refer [map-invert]]
            [thermos-frontend.util :refer [target-value]]))

(defn text [& {:keys [value-atom] :as ks
               :or {value-atom nil}}]
  [:input.input.text-input
   (merge {:type :text
           :placeholder "some text"
           :on-change (when value-atom #(reset! value-atom (target-value %)))
           }
          (when value-atom
            {:value @value-atom})
          ks)])

(defn number [{value-atom :value-atom scale :scale step :step :as ks}]
  (reagent/with-let [element (atom nil)]
    (let [scale (or scale 1)
          step  (or step 1)

          digits (max 0 (- (Math/log10 step)))

          s-value (.toFixed
                   (* scale (or (when value-atom @value-atom) (:value ks)))
                   digits)
          on-change (or (:on-change ks) (partial reset! value-atom))
          parse (if (integer? step)
                  js/parseInt
                  js/parseFloat)
          ]

      [:input.input.number-input
       (merge {:type :number
               :placeholder "0"
               :default-value s-value}

              (dissoc ks :value-atom :scale :value)

              (when value-atom
                {:on-blur

                 #(let [val @value-atom]
                    (set! (.. @element -value)
                          (* val scale)))})

              {:ref #(reset! element %)
               :on-change
               #(let [val (target-value %)
                      val (parse val)]
                  (when (js/isFinite val)
                    (on-change (/ (parse val) scale))))})])))

(defn select [{value-atom :value-atom values :values
               value :value on-change :on-change
               }]
  (let [index->key (into {} (map-indexed #(vector (str %1) %2) (map first values)))
        key->index (map-invert index->key)]
    [:select.select
     {:value (key->index (cond value-atom @value-atom
                               value value))
      :on-change
      (cond value-atom
            #(reset! value-atom (index->key (.. % -target -value)))
            on-change
            #(on-change (index->key (.. % -target -value))))
      }

     (for [[k v] values]
       (let [k (key->index k)]
         [:option {:value (str k) :key k} v]))]))

(defn check-number [{check-atom :check-atom :as ks}]
  [:div {:style {:display :flex}}
   [:input.input
    {:type :checkbox
     :checked (boolean (and check-atom @check-atom))
     :on-change (fn [e] (reset! check-atom (.. e -target -checked)))
     }]
   [number (-> ks
               (dissoc :check-atom)
               (assoc :disabled (not (and check-atom @check-atom))))]])

(def group-ids (atom 0))

(defn radio-group [{options   :options
                    value     :value
                    on-change :on-change}]
  ;; we kind of want a unique name
  (reagent/with-let [group-name (swap! group-ids inc)]
    [:div
     (for [{label :label key :key} options]
       [:label {:key key}
        [:input {:type :radio :value group-name
                 :checked (= value key)
                 :on-change #(on-change key)}]
        label])]))


(defn check [{key :key label :label value :value on-change :on-change}]
  (let [chk (atom nil)
        paint
        (fn [value]

          (set! (.-checked @chk)
                (and value (not= :indeterminate value)))
          (set! (.-indeterminate @chk)
                (= :indeterminate value)))
        ]
    (reagent/create-class
     {:reagent-render
      (fn [{key :key label :label value :value on-change :on-change}]
        [:label {:key key}
         [:input.input {:type :checkbox
                        :ref #(reset! chk %)
                        :on-change #(on-change (.. % -target -checked))}
          ] label]
        )
      :component-did-mount
      (fn [_ ] (paint value))
      :component-will-update
      (fn [_ [_ {value :value}]] (paint value))
      })))

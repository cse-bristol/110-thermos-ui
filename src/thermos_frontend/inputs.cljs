(ns thermos-frontend.inputs
  (:require [reagent.core :as reagent]
            [reagent.dom :as rdom]
            [clojure.set :refer [map-invert]]
            [thermos-frontend.util :refer [target-value]]
            [thermos-frontend.format :as format]))

(defn text [{:keys [value-atom] :as ks
               :or {value-atom nil}}]
  [:input.input.text-input
   (merge {:type :text
           :placeholder "some text"
           :on-change (when value-atom #(reset! value-atom (target-value %)))
           }
          (when value-atom
            {:value @value-atom})
          (dissoc ks :value-atom))])

(defn- default-validate [s x]
  (when (nil? x)
    (str s " is not a valid input")))

(def fmt
  (reagent/create-class
   {:display-name "fmt-input"

    :component-did-update
    (fn [this old-argv]
      (let [{:keys [value print read validate]}
            (-> (reagent/argv this)
                (second))
            element (rdom/dom-node this)]
        (when-not (and
                   (= js/document.activeElement element)
                   ;; this may be a little slow, but it's easy
                   (= (read (.-value element)) value))
          (set! (.-value element) (print value)))))

    :reagent-render
    (fn [{:keys [value print read on-change validate]
          :or {validate default-validate}
          :as atts}]
      [:input
       (-> atts
           (dissoc :value :print :read :validate)
           (assoc :type (:type atts :text)
                  :default-value (print value)
                  :on-change
                  (and on-change
                       (fn [e]
                         (let [s (-> e .-target .-value)
                               v (read s)
                               err (validate s v)]
                           (when (and on-change (not err))
                             (on-change v))
                           )))
                  :on-blur
                  (fn [e]
                    (set! (-> e .-target .-value) (print value))
                    )))
       ])}))

(defn number
  "Render a number input. SCALE of 100 means display value is 100 times larger than real value."
  [{value-atom :value-atom scale :scale step :step
    empty-value :empty-value
    :as ks }]
  (reagent/with-let [element  (atom nil)
                     is-blank (reagent/atom false)]
    (let [scale (or scale 1)
          step  (or step 1)

          digits (max 0 (- (Math/log10 step)))

          has-empty-value empty-value
          [empty-value empty-value-label] empty-value

          value (or (when value-atom @value-atom) (:value ks))

          s-value (if (and has-empty-value (= empty-value value))
                    empty-value-label
                    (.toFixed (* scale value) digits))

          on-change (or (:on-change ks) (when value-atom (partial reset! value-atom)))
          on-blur   (:on-blur ks)
          parse (if (integer? step)
                  js/parseInt
                  js/parseFloat)


          ]

      [:input.input.number-input
       (merge {:type :number
               :placeholder   (if (and has-empty-value
                                       @is-blank)
                                empty-value-label
                                s-value)
               
               :default-value s-value
               :ref #(reset! element %)}

              (dissoc ks :value-atom :scale :value :empty-value)

              (when value-atom
                {:on-blur
                 #(let [val     @value-atom
                        element @element]
                    (.setCustomValidity element "")
                    (cond
                      (and @is-blank has-empty-value)
                      (do (on-change empty-value)
                          (set!  (.. element -value) empty-value-label))

                      :else
                      (set! (.. element -value) s-value)))})

              (when on-blur
                {:on-blur
                 #(let [s-val (target-value %)
                        val (parse s-val)]
                    (reset! is-blank (= "" s-val))
                    (cond
                      (and (= "" s-val) has-empty-value)
                      (on-blur empty-value)
                      
                      (js/isFinite val)
                      (on-blur (/ (parse val) scale))
                      
                      :else
                      (.setCustomValidity @element "Not a number!"))
                    
                    )
                 }
                )
              
              (when on-change
                {:on-change
                 #(let [s-val (target-value %)
                        val (parse s-val)]
                    (reset! is-blank (= "" s-val))
                    (cond
                      (and (= "" s-val) has-empty-value)
                      (on-change empty-value)
                      
                      (js/isFinite val)
                      (on-change (/ (parse val) scale))
                      
                      :else
                      (.setCustomValidity @element "Not a number!"))
                    
                    )}))])))

(defn select [{value-atom :value-atom values :values
               value :value on-change :on-change
               :as attrs
               }]
  (let [index->key (into {} (map-indexed #(vector (str %1) %2) (map first values)))
        key->index (map-invert index->key)]
    [:select.select
     (merge
      {:value (key->index (cond value-atom @value-atom
                                value value))
       :on-change
       (cond value-atom
             #(reset! value-atom (index->key (.. % -target -value)))
             on-change
             #(on-change (index->key (.. % -target -value))))
       }
      (dissoc attrs :value :on-change :values :value-atom))

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

(defn parsed [{:keys [parse render on-change on-blur on-key-down value]
               :or   {render identity parse identity}
               :as   attrs}]
  (let [raw-value    (reagent/atom (render value))
        parsed-value (atom value)]
    (reagent/create-class
     {:reagent-render
      (fn [{:keys [parse render on-change on-blur]
            :or {render identity parse identity}
            :as attrs}]
        (let [attrs (dissoc attrs :parse :render :on-change :on-blur :value
                            :on-key-down)]
          [:input.input
           (merge
            {:value @raw-value
             :on-change
             (fn [e]
               (let [value (-> e .-target .-value)]
                 (reset! raw-value value)
                 (when-let [value (parse value)]
                   (reset! parsed-value value)
                   (set! (.. e -target -parsedValue) value)
                   (when on-change (on-change e value)))))
             :on-blur
             (fn [e]
               (reset! raw-value (render @parsed-value))
               ;; not sure about change of meaning for on-blur
               (when on-blur (on-blur e @parsed-value)))}
            (when on-key-down
              {:on-key-down
               (fn [e]
                 (on-key-down e @parsed-value))})
            attrs)
           ])
        )
      :component-did-update
      (fn [this old-argv]
        (let [new-value (:value (reagent/props this))
              old-value (:value (second old-argv))]
          (when-not (= new-value old-value)
            (reset! parsed-value new-value)
            (reset! raw-value (render new-value)))
          ))})))

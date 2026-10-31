;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.inputs
  "Form inputs. Those which take :value-atom can instead take :path, a path
  into the document, which they read with the :doc/get subscription and
  change with the :doc/set event."
  (:require [reagent.core :as reagent]
            [reagent.ratom]
            [re-frame.core :as rf]
            [re-frame.db]
            [clojure.string :as string]
            [clojure.set :as set :refer [map-invert]]
            [thermos-frontend.util :refer [target-value]]
            [thermos-frontend.format :as format]))

(deftype ^:private DocumentPath [path]
  IDeref
  (-deref [_]
    ;; outside a render (e.g. in an event handler) a subscription would
    ;; not be disposed of, and re-frame complains
    (if reagent.ratom/*ratom-context*
      @(rf/subscribe [:doc/get path])
      (get-in @re-frame.db/app-db path)))

  IReset
  (-reset! [_ value]
    ;; synchronously, so a controlled input's value doesn't lag behind
    (rf/dispatch-sync [:doc/set path value])
    value))

(defn- with-path
  "Input attributes `atts` in which :path, if there is one, has been
  turned into a :value-atom (or whatever `k` is)"
  ([atts] (with-path atts :value-atom))
  ([{:keys [path] :as atts} k]
   (cond-> atts
     path (-> (dissoc :path)
              (assoc k (DocumentPath. path))))))

(defn text [atts]
  (let [{:keys [value-atom] :as ks} (with-path atts)]
    [:input.input.text-input
     (merge {:type :text
             :placeholder "some text"
             :on-change (when value-atom #(reset! value-atom (target-value %)))
             }
            (when value-atom
              {:value (or @value-atom "")})
            (dissoc ks :value-atom))]))

(defn- default-validate [s x]
  (when (nil? x)
    (str s " is not a valid input")))

(defn fmt [{:keys [on-change on-blur value validate read print]
            :or {validate default-validate}}]
  (let [element (atom nil)]
    (reagent/create-class
     {:display-name "fmt-input"
  
      :component-did-update
      (fn [this old-argv]
        (let [{:keys [value print read validate]}
              (-> (reagent/argv this)
                  (second))
              element @element]
          (when-not (and
                     (= js/document.activeElement element)
                     ;; this may be a little slow, but it's easy
                     (= (read (.-value element)) value))
            (set! (.-value element) (print value)))))
  
      :reagent-render
      (fn [{:keys [value print read on-change on-blur validate]
            :or {validate default-validate}
            :as atts}]
        [:input
         (-> atts
             (dissoc :value :print :read :validate)
             (assoc :type (:type atts :text)
                    :default-value (print value)
                    :on-change (and on-change
                                    (fn [e]
                                      (let [s (-> e .-target .-value)
                                            v (read s)
                                            err (validate s v)]
                                        (when (and on-change (not err))
                                          (on-change v)))))
                    :ref (fn [e]
                           (reset! element e)
                           (when-let [ref (:ref atts)] (ref e)))
                    :on-blur (if on-blur
                               (fn [e]
                                 (let [s (-> e .-target .-value)
                                       v (read s)
                                       err (validate s v)]
                                   (when-not err (on-blur v))))
                               
                               (fn [e]
                                 (set! (-> e .-target .-value) (print value))))
                    ))
         ])})))


(defn number2 [atts]
  (let [{:keys [value-atom scale step empty-value on-change on-blur]
         :or {scale 1.0}
         :as atts} (with-path atts)
        has-empty-value empty-value
        [empty-value empty-value-label] empty-value

        on-change
        (if value-atom
          (partial reset! value-atom)
          on-change)

        read-fn
        (fn [s] (if (string/blank? s) empty-value
                    (let [v (js/parseFloat s)]
                      (when (number? v) (/ v scale)))))

        validate-fn
        (fn [s v]
          (when-not (or v (and has-empty-value (string/blank? s)))
            "Should be a number"))

        print-fn
        (fn [x] (if (or (not (number? x))
                        (and has-empty-value (= x empty-value)))
                  "" (str (* scale x))))
        
        ]

    [fmt (-> atts
             (dissoc :scale :value-atom :empty-value)
             (assoc :class "input number-input"
                    :on-change on-change
                    :on-blur on-blur
                    :read  read-fn
                    :validate validate-fn
                    :placeholder empty-value-label
                    :print print-fn)
             (cond-> value-atom
               (assoc :value @value-atom)))]))

(defn number
  "Render a number input. SCALE of 100 means display value is 100 times larger than real value."
  [atts]
  (reagent/with-let [element  (atom nil)
                     is-blank (reagent/atom false)]
    (let [{value-atom :value-atom scale :scale step :step
           empty-value :empty-value
           :as ks} (with-path atts)
          scale (or scale 1)
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
              
              (when-not value-atom {:key s-value})

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

(defn select [atts]
  (let [{value-atom :value-atom values :values
         value :value on-change :on-change
         :as attrs} (with-path atts)
        index->key (into {} (map-indexed #(vector (str %1) %2) (map first values)))
        key->index (map-invert index->key)]
    [:select.select
     (merge
      {:value (or (key->index (cond value-atom @value-atom
                                    value value))
                  ;; when no option matches, as nil would make it uncontrolled
                  "")
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

(defn check-number
  "A number input with a checkbox to enable it. :check-path is a path to
  the checkbox's value, as :path is to the number's"
  [atts]
  (let [{check-atom :check-atom :as ks} (-> atts
                                            (with-path)
                                            (set/rename-keys {:check-path :path})
                                            (with-path :check-atom))]
    [:div {:style {:display :flex}}
     [:input.input
      {:type :checkbox
       :checked (boolean (and check-atom @check-atom))
       :on-change (fn [e] (reset! check-atom (.. e -target -checked)))
       }]
     [number (-> ks
                 (dissoc :check-atom)
                 (assoc :disabled (not (and check-atom @check-atom))))]]))

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


(defn check [{key :key label :label value :value on-change :on-change
              disabled :disabled :or {disabled false}}]
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
      (fn [{key :key label :label value :value on-change :on-change
            disabled :disabled :or {disabled false}}]
        
        [:label {:key key}
         [:input.input {:type :checkbox
                        :disabled disabled
                        :ref #(reset! chk %)
                        :on-change #(on-change (.. % -target -checked))}
          ] label]
        )
      :component-did-mount
      (fn [_ ] (paint value))
      :component-did-update
      (fn [this _] (paint (:value (reagent/props this))))
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

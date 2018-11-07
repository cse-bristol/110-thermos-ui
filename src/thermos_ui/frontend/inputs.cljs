(ns thermos-ui.frontend.inputs
  (:require [reagent.core :as reagent]))

(defn text [& {:keys [value-atom] :as ks
               :or {value-atom nil}}]
  [:input.input
   (merge {:type :text
           :placeholder "some text"
           :value (or (:value ks) (when value-atom @value-atom))
           :on-change (when value-atom #(reset! value-atom (.. % -target -value)))
           } ks)])

(defn number [{value-atom :value-atom scale :scale step :step :as ks}]
  (reagent/with-let [element (atom nil)]
    (let [scale (or scale 1)
         step  (or step 1)

         digits (max 0 (- (Math/log10 step)))
         
         s-value (.toFixed
                  (* scale (or (when value-atom @value-atom) (:value ks)))
                  digits)]
     
     [:input.input
      (merge {:type :number
              :placeholder "0"
              :default-value s-value}
             
             (dissoc ks :value-atom :scale)
             {:ref #(reset! element %)

              :on-blur
              #(set! (.. @element -value) @value-atom)
              
              :on-change
              (let [on-change (or (:on-change ks) (partial reset! value-atom))
                    parse (if (integer? (or (:step ks) 1))
                            js/parseInt
                            js/parseFloat)
                    ]
                #(let [val (.. % -target -value)
                       val (/ (parse val) scale)]
                   (on-change val)))})])))

(defn select [{value-atom :value-atom values :values}]
  [:select
   {:value (str @value-atom)
    :on-change
    (when value-atom #(reset! value-atom (keyword (.substring (.. % -target -value) 1))))}
   
   (let [val @value-atom]
     (for [[k v] values]
       [:option {:value (str k) :key k} v]))])

(defn check-number [{check-atom :check-atom :as ks}]
  [:div
   [:input.input
    {:type :checkbox
     :checked (boolean (and check-atom @check-atom))
     :on-change (fn [e] (reset! check-atom (.. e -target -checked)))
     }]
   [number (-> ks
               (dissoc :check-atom)
               (assoc :disabled (not (and check-atom @check-atom))))]])


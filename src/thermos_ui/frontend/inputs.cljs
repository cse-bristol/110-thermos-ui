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

(defn number [& {:keys [value-atom scale step]
                 :or {value-atom nil scale 1 step 1}
                 :as ks}]
  (let [digits (max 0 (- (Math/log10 step)))
        
        s-value (.toFixed
                 (* scale (or (when value-atom @value-atom) (:value ks)))
                 digits)]
    
    [:input.input
     (merge {:type :number
             :placeholder "0"
             :default-value s-value
             }
            (dissoc ks :value-atom :scale)
            {:on-change
             (let [on-change (or (:on-change ks) (partial reset! value-atom))
                   parse (if (integer? (or (:step ks) 1))
                           js/parseInt
                           js/parseFloat)
                   ]
               #(let [val (.. % -target -value)
                      val (/ (parse val) scale)]
                  (on-change val)))})]))

(defn select [& {:keys [value-atom values]}]
  [:select
   {:default @value-atom
    :on-change
    (when value-atom #(reset! value-atom (keyword (.. % -target -value))))}
   
   (let [val @value-atom]
     (for [[k v] values]
       [:option {:value k :key k} v]))])


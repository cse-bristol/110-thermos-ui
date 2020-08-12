(ns thermos-frontend.events.pipe
  (:require [thermos-frontend.events.core :refer [handle]]
            [thermos-specs.document :as document]
            [com.rpl.specter :as sr]))

(defmethod handle :pipe/add-civil-cost
  [state [_ name]]
  (sr/transform
   [::document/pipe-costs
    (sr/collect-one :diameter (sr/view count))
    :civil-cost
    ]
   (fn [ndia civils]
     (assoc
      civils (inc (reduce max -1 (keys civils)))
      {:name name :cost (vec (repeat ndia 0))}))
   
   state))

(comment
  (->
   {::document/pipe-costs
    {:diameter [10 100]}}
   (handle [:pipe/add-civil-cost "hi"])
   (handle [:pipe/add-civil-cost "there"])

   (=
    {::document/pipe-costs
     {:diameter [10 100]
      :civil-cost
      {0 {:name "hi" :cost [0 0]}
       1 {:name "there" :cost [0 0]}}}})))

(defmethod handle :pipe/rename-civil-cost
  [state [_ id new-name]]
  (assoc-in state [::document/pipe-costs :civil-cost id :name] new-name))

(defmethod handle :pipe/set-civil-cost
  [state [_ id row cost]]
  (assoc-in state [::document/pipe-costs :civil-cost id :cost row] cost))

(defmethod handle :pipe/set-mechanical-cost
  [state [_ row cost]]
  (assoc-in state [::document/pipe-costs :mechanical-cost row] cost))

(defmethod handle :pipe/add-diameter
  [state [_ diameter]]
  state
  ;; not sure what to set as costs
  )

(defmethod handle :pipe/remove-diameter
  [state [_ row]]
  (sr/multi-transform
   [::document/pipe-costs
    (sr/multi-path
     [:diameter row (sr/terminal-val sr/NONE)]
     [:mechanical-cost row (sr/terminal-val sr/NONE)]
     [:civil-cost sr/MAP-VALS :cost row (sr/terminal-val sr/NONE)])]
   state))

(comment
  (-> {::document/pipe-costs
       {:diameter [0 10 20]
        :mechanical-cost [10 20 30]
        :civil-cost {0 {:cost [30 40 50]} 1 {:cost [80 90 100]}}}}
      (handle [:pipe/remove-diameter 1])
      (= 
       {:thermos-specs.document/pipe-costs
        {:diameter [0 20],
         :mechanical-cost [10 30],
         :civil-cost {0 {:cost [30 50]}, 1 {:cost [80 100]}}}})))

(defmethod handle :pipe/set-diameter
  [state [_ row diameter]]
  
  )

(defmethod handle :pipe/remove-civil-cost
  [state [_ id]]
  )


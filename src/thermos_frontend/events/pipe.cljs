(ns thermos-frontend.events.pipe
  (:require [thermos-frontend.events.core :refer [handle]]
            [thermos-specs.document :as document]
            [thermos-specs.path :as path]
            [com.rpl.specter :as sr]
            [thermos-util :refer [assoc-id]]
            ))

(defmethod handle :pipe/change-diameter
  [state [_ from to]]
  (sr/setval
   [::document/pipe-costs
    :rows
    sr/MAP-KEYS
    (sr/pred= from)]
   to
   state))

(comment
  (-> {::document/pipe-costs
       {:rows {100 {:a 1} 200 {:b 1}}}}
      (handle [:pipe/change-diameter 100 150])
      (= {::document/pipe-costs
          {:rows {150 {:a 1} 200 {:b 1}}}}))
  )

(defmethod handle :pipe/rename-civils
  [state [_ cid name]]
  (assoc-in state [::document/pipe-costs :civils cid] name))

(defmethod handle :pipe/change-cost
  [state [_ dia cost]]
  (assoc-in state [::document/pipe-costs :rows dia :pipe] cost))

(defmethod handle :pipe/change-civil-cost
  [state [_ dia cid cost]]
  (assoc-in state [::document/pipe-costs :rows dia cid] cost))

(defmethod handle :pipe/add-diameter
  [state [_ dia]]
  (assoc-in
   state
   [::document/pipe-costs :rows dia]
   {}
   )
  )

(defmethod handle :pipe/add-civils
  [state [_ n]]
  (update-in
   state
   [::document/pipe-costs :civils]
   assoc-id n))

(defmethod handle :pipe/remove-diameter
  [state [_ dia]]
  (update-in state [::document/pipe-costs :rows]
             dissoc dia))

(defmethod handle :pipe/remove-civils
  [state [_ cid]]
  (sr/multi-transform
   (sr/multi-path
    [::document/pipe-costs
     (sr/multi-path
      [:civils cid (sr/terminal-val sr/NONE)]
      [:rows sr/MAP-VALS cid (sr/terminal-val sr/NONE)])]
    [::document/candidates sr/MAP-VALS
     ::path/civil-cost-id (sr/pred= cid)
     (sr/terminal-val sr/NONE)])
   state))

(defmethod handle :pipe/change-capacity
  [state [_ dia capacity-kw]]
  (if (number? capacity-kw)
    (assoc-in state
              [::document/pipe-costs :rows dia :capacity-kwp]
              capacity-kw)
    (update-in state
               [::document/pipe-costs :rows dia]
               dissoc :capacity-kwp)
    )
  )

;; (defmethod handle :pipe/add-civil-cost
;;   [state [_ name]]
;;   (sr/transform
;;    [::document/pipe-costs
;;     (sr/collect-one :diameter (sr/view count))
;;     :civil-cost
;;     ]
;;    (fn [ndia civils]
;;      (assoc
;;       civils (inc (reduce max -1 (keys civils)))
;;       {:name name :cost (vec (repeat ndia 0))}))
   
;;    state))

;; (comment
;;   (->
;;    {::document/pipe-costs
;;     {:diameter [10 100]}}
;;    (handle [:pipe/add-civil-cost "hi"])
;;    (handle [:pipe/add-civil-cost "there"])

;;    (=
;;     {::document/pipe-costs
;;      {:diameter [10 100]
;;       :civil-cost
;;       {0 {:name "hi" :cost [0 0]}
;;        1 {:name "there" :cost [0 0]}}}})))

;; (defmethod handle :pipe/rename-civil-cost
;;   [state [_ id new-name]]
;;   (assoc-in state [::document/pipe-costs :civil-cost id :name] new-name))

;; (defmethod handle :pipe/set-civil-cost
;;   [state [_ id row cost]]
;;   (assoc-in state [::document/pipe-costs :civil-cost id :cost row] cost))

;; (defmethod handle :pipe/set-mechanical-cost
;;   [state [_ row cost]]
;;   (assoc-in state [::document/pipe-costs :mechanical-cost row] cost))

;; (defmethod)

;; ;; (comment

;; ;;   {:civils {0 "Stuff" 1 "Things"}
;; ;;    :rows
;; ;;    {100 {:mechanical 100
;; ;;          :civil {0 30 1 50}
;; ;;          ;; ...
;; ;;          }}
;; ;;    }

;; ;;   )


;; (defmethod handle :pipe/add-diameter
;;   [state [_ diameter]]
  
;;   ;; (let [diameters (-> state ::document/pipe-costs :diameter)]
;;   ;;   (if (contains? (set diameters) diameter)
;;   ;;     state

;;   ;;     (let [nd (count diameters)

;;   ;;           ;; would it be better to have rows as a blob

            
            
;;   ;;           ip ;; slow but who cares
;;   ;;           (loop [i 0]
;;   ;;             (cond
;;   ;;               (= nd i) i
;;   ;;               (> (nth diameters i) diameter) i
;;   ;;               (recur (inc i))))]

;;   ;;       (sr/multi-transform
;;   ;;        [::document/pipe-costs
;;   ;;         (sr/multi-path
;;   ;;          [:diameter (sr/srange ip ip) sr/AFTER-ELEM (sr/terminal-val diameter)]
;;   ;;          [:mechanical-cost (sr/srange ip ip) sr/AFTER-ELEM (sr/terminal-val 2)]
;;   ;;          [:civil-cost sr/MAP-VALS :cost
;;   ;;           (sr/srange ip ip) sr/AFTER-ELEM (sr/terminal-val 2)])]))))
;;   )


;; (defmethod handle :pipe/remove-diameter
;;   [state [_ row]]
;;   (sr/multi-transform
;;    [::document/pipe-costs
;;     (sr/multi-path
;;      [:diameter row (sr/terminal-val sr/NONE)]
;;      [:mechanical-cost row (sr/terminal-val sr/NONE)]
;;      [:civil-cost sr/MAP-VALS :cost row (sr/terminal-val sr/NONE)])]
;;    state))

;; (comment
;;   (-> {::document/pipe-costs
;;        {:diameter [0 10 20]
;;         :mechanical-cost [10 20 30]
;;         :civil-cost {0 {:cost [30 40 50]} 1 {:cost [80 90 100]}}}}
;;       (handle [:pipe/remove-diameter 1])
;;       (= 
;;        {:thermos-specs.document/pipe-costs
;;         {:diameter [0 20],
;;          :mechanical-cost [10 30],
;;          :civil-cost {0 {:cost [30 50]}, 1 {:cost [80 100]}}}})))

;; (defmethod handle :pipe/set-diameter
;;   [state [_ row diameter]]
  
;;   )

;; (defmethod handle :pipe/remove-civil-cost
;;   [state [_ id]]
;;   )


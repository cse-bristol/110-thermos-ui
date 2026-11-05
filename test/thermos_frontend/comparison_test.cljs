;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.comparison-test
  (:require [cljs.test :refer [deftest testing is use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db]
            [cljts.core :as jts]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.document :as document]
            [thermos-specs.path :as path]
            [thermos-specs.solution :as solution]
            [thermos-specs.supply :as supply]
            [thermos-specs.tariff :as tariff]
            [thermos-specs.view :as view]
            [thermos-frontend.comparison :as comparison]
            [thermos-frontend.db :as db]
            [thermos-frontend.events :as events]
            [thermos-frontend.fx :as fx]
            [thermos-frontend.subs]
            [thermos-frontend.undo :as undo]))

(use-fixtures :each
  {:before #(set! (.-restore js/goog.global) (rf/make-restore-fn))
   :after  #((.-restore js/goog.global))})

;; The differences

(defn- building [id & {:as more}]
  (merge {::candidate/id id ::candidate/type :building ::candidate/inclusion :optional} more))

(defn- path [id & {:as more}]
  (merge {::candidate/id id ::candidate/type :path ::candidate/inclusion :optional} more))

(def connected {::solution/included true ::solution/connected true})

(def gas {::solution/included true ::solution/alternative {::supply/name "Gas"}})

(defn- piped [mm] {::solution/included true ::solution/diameter-mm mm})

(defn- doc [& candidates]
  {::document/candidates (into {} (for [c candidates] [(::candidate/id c) c]))
   ::document/npv-term 40
   ::document/npv-rate 0.03
   ::solution/state :valid})

(defn- diff-of [a b] (comparison/candidate-diff a b true))

(deftest candidate-differences
  (testing "on the network"
    (is (= {:id "a" :type :building :network :both :category :same}
           (diff-of (building "a" connected) (building "a" connected))))
    (is (= {:id "a" :type :building :network :here :system ["network" "Gas"] :category :added}
           (diff-of (building "a" connected) (building "a" gas))))
    (is (= {:id "a" :type :building :network :there :system [nil "network"] :category :removed}
           (diff-of (building "a") (building "a" connected))))
    (is (= :added (:category (diff-of (path "p" (piped 50)) (path "p"))))))

  (testing "a different heating system off the network"
    (is (= {:id "a" :type :building :system ["Gas" nil] :category :changed}
           (diff-of (building "a" gas) (building "a"))))
    (is (nil? (:system (comparison/candidate-diff (building "a" gas) (building "a") false)))
        "not when one network has no solution"))

  (testing "pipe size"
    (is (= {:id "p" :type :path :network :both :diameter [50 80] :category :changed}
           (diff-of (path "p" (piped 50)) (path "p" (piped 80))))))

  (testing "constraints"
    (is (= {:id "a" :type :building :inclusion [:required :optional] :category :changed}
           (diff-of (building "a" ::candidate/inclusion :required) (building "a"))))
    (is (= [:optional :forbidden] (:inclusion (diff-of (building "a") nil)))
        "a candidate the other network doesn't have is forbidden there"))

  (testing "other inputs"
    (is (:edited (diff-of (building "a" ::demand/kwh 10) (building "a" ::demand/kwh 20))))
    (is (= :unchanged (:category (diff-of (building "a" ::candidate/selected true
                                                   ::candidate/modified true
                                                   :some.ns/transient 1)
                                         (building "a"))))
        "but not the selection, whether it was edited, or editor-only keys"))

  (testing "only in one"
    (is (= {:id "a" :type :building :presence :here :category :added}
           (diff-of (building "a") :deleted)))
    (is (= {:id "a" :type :building :presence :there :network :there :category :removed}
           (diff-of :deleted (building "a" connected))))))

(deftest document-differences
  (let [here  (-> (doc (building "a" connected)
                       (building "b" ::candidate/inclusion :forbidden)
                       (building "c" ::candidate/inclusion :forbidden ::candidate/modified true)
                       (path "p" (piped 50)))
                  (assoc ::document/deletions ["d"]))
        there (doc (building "a")
                   (building "c" ::candidate/inclusion :forbidden ::candidate/modified true)
                   (building "d" connected)
                   (path "p" (piped 50))
                   (building "e" gas))
        diff  (comparison/diff here there)]
    (is (= #{"a" "c" "d" "e" "p"} (set (keys diff)))
        "the candidates either saves, but not those the editor loaded which neither does")
    (is (= :added (:category (diff "a"))))
    (is (= :unchanged (:category (diff "c"))))
    (is (= {:presence :there :network :there :category :removed}
           (select-keys (diff "d") [:presence :network :category]))
        "deleted here")
    (is (= [:forbidden :optional] (:inclusion (diff "e"))) "not in this one at all")
    (is (= :same (:category (diff "p"))))
    (is (= {[:building :added] 1 [:building :unchanged] 1 [:building :removed] 1
            [:building :changed] 1 [:path :same] 1}
           (comparison/category-counts diff)))))

(deftest candidate-rows
  (let [rows (comparison/candidate-rows (path "p" (piped 50)) (path "p" (piped 80)) :heating)]
    (is (= {:label "Diameter" :unit "mm" :here 50 :there 80}
           (first (filter (comp #{"Diameter"} :label) rows))))
    (is (= #{"Constraint" "In solution" "Diameter"} (set (map :label rows)))
        "only rows either has a value for"))
  (is (= [nil "optional"]
         ((juxt :here :there)
          (first (comparison/candidate-rows :deleted (building "a") :heating))))))

(deftest summaries
  (let [here  (comparison/summary
               (assoc (doc (merge (building "a" ::demand/kwh 100) connected)
                           (building "b" ::candidate/inclusion :forbidden ::demand/kwh 1000)
                           (merge (path "p" ::path/length 10) (piped 50))
                           (building "s" ::supply/capacity-kwp 500
                                     ::solution/included true ::solution/capacity-kw 20
                                     ::solution/output-kwh 120))
                     ::solution/objective 10))
        there (comparison/summary (doc (building "a" ::demand/kwh 100)))
        rows  (into {} (for [r (comparison/summary-rows here there :heating)] [(:key r) r]))]
    (is (= 1 (:network-count here)))
    (is (= 2 (:input-building-count here))
        "the buildings it saves, not those loaded around the map")
    (is (= 500 (:supply-capacity here)))
    (is (number? (:capex here)) "costs from the solution summary")
    (is (= {:here 1 :there 0 :delta 1} (select-keys (:network-count rows) [:here :there :delta])))
    (is (= {:here 10 :there nil :delta nil}
           (select-keys (:objective-value rows) [:here :there :delta])))
    (is (= "Cold delivered"
           (:label (first (filter (comp #{:network-kwh} :key)
                                  (comparison/summary-rows here there :cooling))))))))

(deftest table-rows
  (let [columns [{:label "Length"} {:label "Cost per m" :sum? false}]
        here  [{:key 50 :labels ["50"] :values [10 2]}
               {:key 80 :labels ["80"] :values [5 3]}
               {:key :all :labels ["All"] :values [15 2.3]}]
        there [{:key 50 :labels ["50"] :values [4 nil]}
               {:key 65 :labels ["65"] :values [6 4]}
               {:key :all :labels ["All"] :values [10 3]}]
        rows  (comparison/table-rows columns here there)
        by-key (into {} (map (juxt :key identity)) rows)]
    (is (= [50 65 80 :all] (map :key rows))
        "a row only there goes after its neighbour there, not after the totals")
    (is (= {:labels ["50"] :here [10 2] :there [4 nil] :delta [6 nil]}
           (dissoc (by-key 50) :key))
        "no change unless both are numbers")
    (is (= [nil [6 4] [-6 nil]] ((juxt :here :there :delta) (by-key 65)))
        "a missing row counts as 0 in columns which sum things")
    (is (= [[10 2] nil] ((juxt :here :there) (first (comparison/table-rows columns here nil)))))))

(deftest parameters
  (let [tariffs (fn [& names] (into {} (map-indexed (fn [i n] [i {::tariff/id i ::tariff/name n}]) names)))
        here  {::document/npv-term 30 ::document/objective :network
               ::document/tariffs (tariffs "A" "B")
               ::document/pipe-costs {:rows {50 {:pipe 1} 100 {:pipe 2}} :civils {1 "Soft"}}}
        there {::document/npv-term 40 ::document/objective :network
               ::document/tariffs (assoc-in (tariffs "A" "C") [0 ::tariff/unit-charge] 2)
               ::document/pipe-costs {:rows {50 {:pipe 3}} :civils {1 "Soft"}}}
        rows  (into {} (for [r (comparison/parameter-differences here there)] [(:key r) r]))]
    (is (= #{::document/npv-term ::document/tariffs ::document/pipe-costs} (set (keys rows))))
    (is (= {:category :objective :label "NPV term" :here 30 :there 40}
           (dissoc (rows ::document/npv-term) :key)))
    (is (= "only here: B; only there: C; changed: A" (:detail (rows ::document/tariffs))))
    (is (= "sizes only here: 100 mm; costs changed for 1 sizes"
           (:detail (rows ::document/pipe-costs))))))

;; Comparing in the editor

(def server
  {:project-id 1 :map-id 2 :network-id 3
   :needs-save false :needs-load false
   :queue-position 0 :run-state nil :revision 0})

;; without candidates here, which have no geometry for the spatial index
(def a-db
  {::document/npv-term 10
   ::view/view-state {::view/map-view ::view/solution}
   ::db/server server})

(def other-document
  {::document/npv-term 20
   ::document/candidates
   {"x" (building "x" ::candidate/geometry
                  (js/JSON.stringify
                   (clj->js {:type "Polygon"
                             :coordinates [[[0 0] [1 0] [1 1] [0 1] [0 0]]]})))}})

(deftest comparison-events
  (testing "fetching the networks to choose from"
    (is (= {:url "/project/1/map/2/net/history" :on-response [:compare/got-networks]}
           (::fx/get-transit (events/compare-fetch-networks {:db a-db} [:compare/fetch-networks]))))
    (is (= :failed (:networks (db/comparison (events/compare-got-networks a-db [:compare/got-networks nil]))))))

  (testing "fetching the network to compare with"
    (let [{:keys [db] :as fx} (events/compare-with {:db a-db} [:compare/with {:id 7 :name "N"}])]
      (is (= {:url "/project/1/map/2/net/7" :on-response [:compare/loaded 7]} (::fx/get-transit fx)))
      (is (not (db/comparing? db)) "not until it comes")
      (let [comparing (events/compare-loaded db [:compare/loaded 7 other-document])]
        (is (db/comparing? comparing))
        (is (= {:id 7 :name "N"} (:network (db/comparison comparing))))
        (is (= #{"x"} (:picked (db/comparison
                                (events/compare-pick comparing
                                                     [:compare/pick (jts/create-point 0.5 0.5) :replace]))))
            "clicking finds the other network's candidates"))
      (is (identical? db (events/compare-loaded db [:compare/loaded 8 other-document]))
          "a network which is no longer wanted")
      (is (= {:id 7 :name "N"} (:failed (db/comparison (events/compare-loaded db [:compare/loaded 7 nil]))))
          "a network which couldn't be had"))))

(deftest comparing-is-not-editing
  ;; as after :compare/with, without fetching anything
  (reset! re-frame.db/app-db (assoc a-db ::db/compare {:loading {:id 7 :name "N"}}))
  (rf/dispatch-sync [:compare/loaded 7 other-document])
  (let [comparing @re-frame.db/app-db]
    (is (db/comparing? comparing))
    (is (= 20 (get-in comparing [::db/compare :document ::document/npv-term])))
    (is (not (db/unsaved-change? a-db comparing)) "entering")
    (is (not (undo/edit? a-db comparing)))
    (is (not (db/needs-save? comparing)))
    (is (nil? (undo/history comparing)) "nothing to undo")

    (rf/dispatch-sync [:compare/stop])
    (let [stopped @re-frame.db/app-db]
      (is (not (db/comparing? stopped)))
      (is (not (db/unsaved-change? comparing stopped)) "leaving")
      (is (not (undo/edit? comparing stopped)))
      (is (not (db/needs-save? stopped)))
      (is (nil? (undo/history stopped)))))

  (testing "editing while comparing"
    (reset! re-frame.db/app-db (assoc a-db ::db/compare {:loading {:id 7}}))
    (rf/dispatch-sync [:compare/loaded 7 other-document])
    (rf/dispatch-sync [:doc/set [::document/npv-term] 11])
    (is (db/comparing? @re-frame.db/app-db) "carries on comparing")
    (is (db/needs-save? @re-frame.db/app-db))
    (is (= {:here 11 :there 20}
           (select-keys (first (filter (comp #{::document/npv-term} :key)
                                       @(rf/subscribe [:compare/parameters])))
                        [:here :there]))
        "and the differences follow")
    (rf/dispatch-sync [:doc/undo])
    (is (= 10 (::document/npv-term @re-frame.db/app-db)))
    (is (db/comparing? @re-frame.db/app-db) "undoing leaves the comparison alone"))

  (testing "a run's result loading"
    (reset! re-frame.db/app-db (assoc a-db ::db/compare {:loading {:id 7}}))
    (rf/dispatch-sync [:compare/loaded 7 other-document])
    (rf/dispatch-sync [:server/loaded {:document {::document/npv-term 30}}])
    (is (= 30 (::document/npv-term @re-frame.db/app-db)))
    (is (db/comparing? @re-frame.db/app-db) "carries on comparing")))

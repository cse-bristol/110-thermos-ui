;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.spatial-test
  (:require [cljs.test :refer [deftest testing is]]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.document :as document]
            [thermos-specs.view :as view]
            [thermos-frontend.spatial :as spatial]))

(defn- square
  "A candidate whose geometry is a unit square at x, y"
  [id x y & {:as more}]
  (merge
   {::candidate/id id
    ::candidate/type :building
    ::candidate/geometry
    (js/JSON.stringify
     (clj->js {:type "Polygon"
               :coordinates [[[x y] [(inc x) y] [(inc x) (inc y)] [x (inc y)] [x y]]]}))}
   more))

(defn- doc [& candidates]
  {::document/candidates (into {} (for [c candidates] [(::candidate/id c) c]))})

(defn- ids-in [document min-x min-y max-x max-y]
  (set (spatial/find-candidates-ids-in-bbox
        document {:minX min-x :minY min-y :maxX max-x :maxY max-y})))

(deftest index
  (let [d (spatial/update-index (doc (square "a" 0 0) (square "b" 10 10)))]
    (is (= #{"a"} (ids-in d -1 -1 2 2)))
    (is (= #{"a" "b"} (ids-in d -1 -1 20 20)))
    (is (= #{} (ids-in d 5 5 6 6)))

    (testing "adding and removing candidates"
      (let [d (-> d
                  (update ::document/candidates dissoc "a")
                  (assoc-in [::document/candidates "c"] (square "c" 5 5))
                  (spatial/update-index))]
        (is (= #{} (ids-in d -1 -1 2 2)))
        (is (= #{"b" "c"} (ids-in d -1 -1 20 20)))
        (is (= #{"b" "c"} (::spatial/indexed-candidates d)))))

    (testing "nothing to do"
      (is (identical? d (spatial/update-index d))))))

(deftest zoom-to-selection
  (let [d (-> (doc (square "a" 0 0 ::candidate/selected true)
                   (square "b" 10 10 ::candidate/selected true)
                   (square "c" 50 50))
              (spatial/update-index)
              (spatial/zoom-to-selection))]
    (is (= {:north 11 :south 0 :east 11 :west 0}
           (::view/bounding-box (::view/view-state d))))))

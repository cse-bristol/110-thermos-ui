(ns thermos-ui.store-service.geometry-store-test
  (:require [clojure.test :refer :all]
            [thermos-ui.store-service.store.geometries :as geoms]))

(deftest bounding-box-calc
  ;;51.288813, -2.820036
  ;;51.288548, -2.820036
  (testing "Get Bounding box a res 18"
    (let [bb (geoms/create-bounding-box 51.288548 -2.820036 18)]
      (is (not (nil? bb)) "Points vector")
      (is (= 5 (count bb)) "Num points")
      ;;TODO Test distance between some points?)))

(deftest database-connection
  (testing "connecting..."
    (is (not (nil? (geoms/get-buildings 1 2 3))))))

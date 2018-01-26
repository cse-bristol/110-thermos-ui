(ns thermos-ui.store-service.geometry-store-test
  (:require [clojure.test :refer :all]
            [thermos-ui.store-service.store.geometries :as geoms]))

(deftest bounding-box-calc
  ;;51.288813, -2.820036 - axbrige
  ;;51.288548, -2.820036 - axbridge
  ;;-0.103539149033575,51.5505511091328 - dunno!
  (testing "Get Bounding box a res 18"
    (let [bb (geoms/create-bounding-box -0.103539149033575 51.550551109132 18)
          points (:points bb)]
      (is (not (nil? points)) "Points vector")
      (is (= 5 (count points)) "Num points"))))

(deftest database-connection
  (testing "connecting..."
    (is (not (nil? (geoms/get-buildings 1 2 3))))))

(deftest can-get-connections-in-grid
  (testing "Grid query"
    (let [connections (geoms/get-buildings -0.103539149033575 51.550551109132 18)]
      (is (= 194 (count connections)))
                                        ;(println connections)
      )))

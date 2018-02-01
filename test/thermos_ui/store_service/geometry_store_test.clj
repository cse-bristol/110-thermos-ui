(ns thermos-ui.store-service.geometry-store-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [thermos-ui.handler :refer :all]
            [thermos-ui.store-service.store.geometries :as geoms]))

(deftest bounding-box-calc
  (testing "Get Bounding box a res 18"
    (let [bb (geoms/create-bounding-box 18 130993 87111)
          points (:points bb)]
      (is (not (nil? points)) "Points vector")
      (is (= 5 (count points)) "Num points"))))

(deftest ^:integration database-connection
  (testing "connecting..."
    (is (not (nil? (geoms/get-connections 1 2 3))))))

(deftest ^:integration can-get-connections-in-grid
  (testing "Grid query at zoom 18"
    (let [connections (geoms/get-connections 18 130993 87111)]
      (is (= 1 (count connections)))))
  (testing "Grid query at zoom 17"
    (let [connections (geoms/get-connections 17 65494 43553)]
      (is (= 49 (count connections)) "Number of connections"))))

(deftest ^:integration connections-from-handler
  (testing "connections-call"
    (let [response (app (mock/request :get "/map/connections/18/130993/87111/"))]
      (is (= (:status response) 200) "Status code of response"))))

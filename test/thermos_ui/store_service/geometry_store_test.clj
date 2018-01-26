(ns thermos-ui.store-service.geometry-store-test
  (:require [clojure.test :refer :all]
            [thermos-ui.store-service.store.geometries :as geoms]))

(deftest database-connection
  (testing "connecting..."
    (is (not (nil? (geoms/get-buildings 1 2 3))))))

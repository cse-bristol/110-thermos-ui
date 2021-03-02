(ns thermos-test.backend.importer.heat-degree-days
  (:require [clojure.test :refer :all]
            [thermos-backend.importer.heat-degree-days :as heat-degree-days]))

(deftest get-hdd
    (is (= (heat-degree-days/get-hdd -9.235382080078 51.530950746531566) 2811.23333437882)) ;; Ireland
    (is (= (heat-degree-days/get-hdd -10.935382080078 51.530950746531566) 2811.23333437882)) ;; Off Irish coast
    (is (= (heat-degree-days/get-hdd 1.7090606689453127 50.84041871148014) 2312.17942342397)) ;; France
    (is (= (heat-degree-days/get-hdd -0.1176309585571289 51.55600849758184) -1.0))) ;; UK
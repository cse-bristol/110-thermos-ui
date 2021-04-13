;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-test.pages.map-import-components
  (:require [clojure.test :refer :all]
            [thermos-pages.map-import-components :as map-import-components]))


(deftest centroid
  (let [*form-state-osm (atom {:geometry {:source :osm
                                          :centroid {:lat 50.1 :lng -1.1}}})
        *form-state-files (atom {:geometry {:source :files
                                            :files {:f1 {:centroid {:lat 50.1 :lng -1.1}}
                                                    :f2 {}
                                                    :f3 {:centroid {:lat 50.6 :lng -1.6}}}}})]
    (is (= (map-import-components/centroid *form-state-osm) {:lat 50.1 :lng -1.1}))
    (is (= (map-import-components/centroid *form-state-files) {:lat 50.35 :lng -1.35}))))
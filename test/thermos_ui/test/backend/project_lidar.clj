;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-test.backend.project-lidar
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [thermos-backend.project-lidar :as project-lidar]))


(deftest test-reproject-bounds
  (is (= (project-lidar/reproject-bounds
          {:x1 358708 :y1 172640 :x2 358839 :y2 172478} "EPSG:27700" "EPSG:4326")
         {:x1 -2.595617543314191 :y1 51.451228420369354 :x2 -2.593713549164707 :y2 51.44978143443339})))

(deftest test-is-tiff?
  (is (= true (project-lidar/is-tiff? (io/file "data" "sample-lidar.tif"))))
  (is (= false (project-lidar/is-tiff? (io/file "data" "sample-fake-tif.tif")))))

(deftest test-can-access-tif?
  (is (= true (project-lidar/can-access-tiff? 1 (io/file "lidar" "project" "1" "file.tif"))))
  (is (= true (project-lidar/can-access-tiff? 1 (io/file "lidar" "project" "1" "sub" "file.tif"))))
  (is (= true (project-lidar/can-access-tiff? 1 (io/file "lidar" "file.tif"))))
  (is (= true (project-lidar/can-access-tiff? 1 (io/file "lidar" "sub" "file.tif"))))
  (is (= false (project-lidar/can-access-tiff? 1 (io/file "lidar" "project" "10" "file.tif"))))
  (is (= false (project-lidar/can-access-tiff? 1 (io/file "lidar" "project" "2" "file.tif")))))

(deftest test-is-system-lidar?
  (is (= false (project-lidar/is-system-lidar? (io/file "lidar" "project" "1" "file.tif"))))
  (is (= true (project-lidar/is-system-lidar? (io/file "lidar" "file.tif"))))
  (is (= true (project-lidar/is-system-lidar? (io/file "lidar" "sub" "file.tif"))))
  (is (= false (project-lidar/is-system-lidar? (io/file "not-lidar" "project" "10" "file.tif")))))

(deftest test-lidar-properties
  (is (= (project-lidar/lidar-properties (io/file "data" "sample-lidar.tif"))
         {:filename "sample-lidar.tif"
          :crs "EPSG:27700" 
          :bounds {:x1 254379.0 :y1 81434.0 :x2 254408.0 :y2 81478.0}
          :source :project}))
  (is (= (project-lidar/lidar-properties (io/file "data" "sample-lidar.tif") :force-crs "EPSG:4326")
         {:filename "sample-lidar.tif"
          :crs "EPSG:4326"
          :bounds {:x1 -4.059645024851869 :y1 50.614390550693784 :x2 -4.059252679285609 :y2 50.614793223904286}
          :source :project})))

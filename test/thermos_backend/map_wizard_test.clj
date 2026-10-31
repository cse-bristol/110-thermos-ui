(ns thermos-backend.map-wizard-test
  (:require [thermos-backend.map-wizard :as wizard]
            [thermos-backend.importer.process :as process]
            [cheshire.core :as json]
            [clojure.test :refer :all]))

(def files-state
  (-> wizard/start-state
      (assoc :project-id 1 :name "A map")
      (assoc-in [:geometry :source] :files)
      (assoc-in [:geometry :files "b"]
                {:base-name "b" :id "uuid" :state :uploaded
                 :count-by-type {:polygon 3}
                 :fields-by-type {:polygon #{"height" "Floor Area" "other"}}})))

(def files-with-roads-state
  (assoc-in files-state [:geometry :files "r"]
            {:base-name "r" :id "uuid2" :state :uploaded
             :count-by-type {:line-string 2}
             :fields-by-type {:line-string #{"id"}}}))

(deftest step-order
  (is (= :geometry (wizard/next-step wizard/start-state :name)))
  (is (= :lidar (wizard/next-step wizard/start-state :geometry))
      "OSM goes straight to lidar")
  (is (= :building-cols (wizard/next-step files-state :geometry)))
  (is (= :lidar (wizard/next-step files-state :building-cols)))
  (is (= :road-cols (wizard/next-step files-with-roads-state :building-cols)))
  (is (= :lidar (wizard/next-step files-with-roads-state :road-cols)))
  (is (= :road-cols (wizard/previous-step files-with-roads-state :lidar)))
  (is (nil? (wizard/previous-step files-state :name))))

(deftest requirements
  (is (seq (wizard/requirements wizard/start-state :name)))
  (is (empty? (wizard/requirements files-state :name)))
  (is (= ["Select an area on the map to continue"]
         (wizard/requirements wizard/start-state :geometry)))
  (is (empty? (wizard/requirements files-state :geometry)))
  (is (seq (wizard/requirements
            (assoc-in files-state [:geometry :files "x"] {:state :invalid})
            :geometry)))
  (is (seq (wizard/requirements
            (assoc-in files-state [:geometry :files] {})
            :geometry))))

(deftest grouping-uploads
  (let [groups (wizard/group-uploads
                [{:filename "a.shp"} {:filename "a.dbf"} {:filename "a.shx"}
                 {:filename "b.GeoJSON"}
                 {:filename "c.txt"}
                 {:filename "d.shp"} {:filename "d.dbf"} {:filename "d.shx"} {:filename "d.prj"}])]
    (is (= #{"a" "b" "c" "d"} (set (keys groups))))
    (is (= "Missing component of shapefile" (get-in groups ["a" :message])))
    (is (nil? (get-in groups ["b" :state])))
    (is (= "Unsupported file type" (get-in groups ["c" :message])))
    (is (nil? (get-in groups ["d" :state])))
    (is (= 4 (count (get-in groups ["d" :uploads]))))))

(deftest field-mapping
  (let [cols (wizard/columns-for-step files-state :building-cols)]
    (is (= [["b" "Floor Area"] ["b" "height"] ["b" "other"]] cols))

    (let [state (wizard/set-mapping files-state :building-cols ["floor-area" "none" "bogus"])]
      (is (= {"b" {"Floor Area" :floor-area}} (get-in state [:buildings :mapping]))))

    (let [state (wizard/guess-mapping files-state :building-cols false)]
      (is (= {"b" {"Floor Area" :floor-area "height" :height}}
             (get-in state [:buildings :mapping]))))

    (let [state (-> files-state
                    (wizard/set-mapping :building-cols ["identity" "none" "none"])
                    (wizard/guess-mapping :building-cols true))]
      (is (= {"b" {"Floor Area" :identity "height" :height "other" :user-fields}}
             (get-in state [:buildings :mapping]))
          "guessing keeps what's already chosen, and harder makes the rest user fields"))))

(deftest drawn-box-parameters
  ;; this is what map-box.js posts for a drawn box
  (let [boundary (json/parse-string
                  "{\"type\":\"Feature\",\"properties\":{\"bounds\":{\"x1\":-0.12,\"y1\":51.5,\"x2\":-0.1,\"y2\":51.52}},
                    \"geometry\":{\"type\":\"Polygon\",\"coordinates\":[[[-0.12,51.5],[-0.12,51.52],[-0.1,51.52],[-0.1,51.5],[-0.12,51.5]]]}}")
        state (-> wizard/start-state
                  (assoc :name "x" :project-id 1)
                  (assoc-in [:geometry :osm] {:boundary boundary}))
        params (wizard/parameters state)]
    (is (empty? (wizard/requirements state :geometry)))
    (is (= [51.5 -0.12 51.52 -0.1]
           (#'process/geojson-bounds (-> params :geometry :osm :boundary)))
        "the importer can read the boundary")))

(deftest too-large
  (let [big (json/parse-string
             "{\"type\":\"Feature\",\"geometry\":{\"type\":\"Polygon\",\"coordinates\":[[[0,0],[0,1],[1,1],[1,0],[0,0]]]}}")]
    (is (= ["Select a smaller area on the map to continue"]
           (wizard/requirements
            (assoc-in wizard/start-state [:geometry :osm] {:boundary big})
            :geometry)))))

(deftest parameters-drop-uploads
  (is (not (contains? (get-in (wizard/parameters
                               (assoc-in files-state [:geometry :files "b" :uploads] [{}]))
                              [:geometry :files "b"])
                      :uploads))))

(deftest centroid
  (is (= {:lat 50.1 :lng -1.1}
         (wizard/centroid {:geometry {:source :osm
                                      :centroid {:lat 50.1 :lng -1.1}}})))
  (is (= {:lat 50.35 :lng -1.35}
         (wizard/centroid {:geometry {:source :files
                                      :files {:f1 {:centroid {:lat 50.1 :lng -1.1}}
                                              :f2 {}
                                              :f3 {:centroid {:lat 50.6 :lng -1.6}}}}}))))

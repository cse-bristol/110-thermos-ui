;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.pages.lidar  
  (:require [thermos-backend.pages.common :refer [page]]))


(defn manage-lidar [project-name lidar-info]
  (page
   {:title "Manage LIDAR"
    :js ["/js/manage-lidar.js"]
    :css ["/css/manage-lidar.css"]}
   [:div
    [:div.card {:style {:margin-bottom "2em"}}
     [:h1 "Manage LIDAR for project " project-name]
     [:p "LIDAR data is used to calculate building heights and volumes for heat demand estimation.
          LIDAR data is only used during map import, and adding or deleting LIDAR will not affect
          any existing maps or networks."]
     [:p "LIDAR coverage is not mandatory, but building height data of some sort will 
          improve the quality of any demand estimates produced from the built-in regression model.
          This can be from a field on the building polygons, or from OpenStreetMap if that
          is the source of buildings. OpenStreetMap does not always contain height data."]
     [:div#manage-lidar-map {:style {:width "100%" :height "500px" :margin-bottom "2em"}}]
     [:table {:style {:width "100%" :margin-bottom "2em"}}
      [:thead
       [:tr (map (fn [header] [:th header]) ["File name" "Bounds" "CRS" "Download" "Delete"])]]
      [:tbody
       (map 
        (fn [row] (let [filename (:filename row)
                        bounds (:bounds row)]
                    [:tr {:style {:vertical-align "top"}}
                     [:td filename]
                     [:td (str "(" (:x1 bounds) " " (:y1 bounds) "), (" (:x2 bounds) " " (:y2 bounds) ")")]
                     [:td (:crs row)]
                     [:td [:form {:method :GET :action filename}
                           [:button.button {:type :submit :value "DOWNLOAD"} "DOWNLOAD"]]]
                     [:td [:a.button.button--danger {:href (str "delete/" filename)} "DELETE"]]])) 
        (sort-by :filename lidar-info))]]
     (when (empty? lidar-info) 
       [:p {:style {:text-align "center" :font-style "italic"}} "This project has no LIDAR data associated with it."])
     [:div
      [:form#upload-form {:method :POST :enctype "multipart/form-data"}
       [:label.button [:input {:value ""
                               :name :file
                               :onchange "document.getElementById('upload-form').submit()"
                               :style {:width 0 :height 0}
                               :accept ".tiff, .tif"
                               :type :file
                               :multiple :multiple}] "ADD LIDAR"]]]]]))

(defn delete-lidar [project-name filename]
  (page
   {:title "Delete LIDAR"}
   [:div
    [:div.card {:style {:margin-bottom "2em"}}
     [:h1 "Deleting LIDAR file " filename " from project " project-name]
     [:p "Are you sure you want to delete this LIDAR file?"]
     [:form {:method :POST }
      [:button.button.button--danger {:type :submit :value "DELETE"} "DELETE"]]]]))
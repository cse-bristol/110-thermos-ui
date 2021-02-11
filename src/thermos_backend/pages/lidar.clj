(ns thermos-backend.pages.lidar  
  (:require [thermos-backend.pages.common :refer [page]]))


(defn manage-lidar [project-name lidar-info]
  (page
   {:title "Manage LIDAR"}
   [:div
    [:div.card {:style {:margin-bottom "2em"}}
     [:h1 {:style {:margin-bottom "2em"}} "Manage LIDAR for project " project-name]
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
        lidar-info)]]
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
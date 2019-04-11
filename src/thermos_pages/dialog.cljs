(ns thermos-pages.dialog
  (:require [rum.core :as rum]))

(defonce dialog-container (atom nil))

(declare close-dialog!)

(rum/defc overlay [content]
  [:div {:style {:background "rgba(0.5,0.5,0.5,0.75)"
                 :position :fixed
                 :top 0 :left 0 :bottom 0 :right 0 :z-index 500
                 :display :flex}
;;         :on-click close-dialog!
         }
   [:div.card {:style {:max-width :80%
                       :margin-top :auto
                       :margin-right :auto
                       :margin-left :auto
                       :margin-bottom :auto}}
    content]])

(defn show-dialog!
  "Render a dialog box containing the content over the top of the page.
   Return a function that can be used to destroy the dialog when you are done with it."
  [content]
  (swap!
   dialog-container
   (fn [dialog-container]
     (when-not dialog-container
       (let [dialog-container (js/document.createElement "div")]
         (.appendChild js/document.body dialog-container)
         (rum/mount (overlay content) dialog-container)
         dialog-container)))))

(defn close-dialog! []
  (swap!
   dialog-container
   (fn [dialog-container]
     (when dialog-container
       (rum/unmount dialog-container)
       (.removeChild js/document.body dialog-container))
     nil)))




(ns thermos-pages.dialog
  (:require [rum.core :as rum]
            [clojure.string :as string]))

(defonce dialog-container (atom nil))

(declare close-dialog!)

(rum/defc overlay [content]
  [:div {:style {:background "rgba(0.5,0.5,0.5,0.75)"
                 :position :fixed
                 :top 0 :left 0 :bottom 0 :right 0 :z-index 500
                 :display :flex}}
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

(rum/defcs delete-dialog < (rum/local true ::button-disabled)
  [{button-disabled ::button-disabled}
   {object-name :name
    confirm-name :confirm-name
    message :message
    on-delete :on-delete}]
  [:div
   [:h1 "Really delete " object-name "?"]
   [:div message]
   (when confirm-name
     [:div.flex-cols
      {:style {:margin-top :1em :margin-bottom :1em}}
      [:span "Enter " [:code object-name] " to confirm: "]
      [:input.text-input.flex-grow
       {:type :text
        :on-change
        #(reset! button-disabled
                 (not= (string/lower-case (.. % -target -value))
                       (string/lower-case object-name)))}]])
   [:div.flex-cols
    [:button.button {:style {:margin-left :auto} :on-click close-dialog!} "CANCEL"]
    [:button.button.button--danger
     {:disabled (and confirm-name @button-disabled)
      :on-click #(do (on-delete)
                     (close-dialog!))}
     "DELETE"]]]
  )

(defn show-delete-dialog! [{object-name :name
                            confirm-name :confirm-name
                            message :message
                            on-delete :on-delete
                            :as args}]
  (show-dialog! (delete-dialog args)))


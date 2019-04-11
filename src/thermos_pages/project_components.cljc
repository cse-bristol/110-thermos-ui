(ns thermos-pages.project-components
  (:require [thermos-pages.menu :refer [menu]]
            [clojure.string :as string]
            [rum.core :as rum]
            [net.cgrand.macrovich :as macro]
            [thermos-pages.common :refer [fn-js] :refer-macros [fn-js]]
            [thermos-pages.spinner :refer [spinner]]
            [ajax.core :refer [POST]]
            [thermos-pages.symbols :as symbols]
            #?@(:cljs
                [[thermos-pages.dialog :refer [show-dialog! close-dialog!]]])))

(rum/defcs project-user-list < rum/reactive
  [state
   {:keys [on-close on-save]}
   users]
  [:div
   [:h1 "Project participants"]
   [:ul
    (for [u (rum/react users)]
      [:li {:key (:id u)}
       [:div
        [:a {:href (str "mailto:" (:id u))} (:name u)]
        (when (= :admin (:auth u))
          [:span {:style {:border "1px #eee solid" :border-radius :4px :padding :2px}} "project admin"])
        (when (:new u)
          [:span {:style {:background "#afa" :border "1px #eee solid" :border-radius :4px :padding :2px :margin :2px}} "New"])]])]
   
   [:input {:ref "invite" :placeholder "an.email@address.com"}]
   [:button {:on-click #(let [input (rum/ref state "invite")
                              input-value (.-value input)]
                          (swap! users conj {:id input-value :name input-value
                                             :new true :auth :read})
                          (set! (.-value input) nil))
             } "Add"]
   [:div
    (when on-close [:button {:on-click on-close} "Cancel"])
    (when on-save [:button {:on-click on-save} "Save"])]])

(rum/defc version-list-widget [map-id versions]
  [:div {:on-click (fn-js [] (close-dialog!))}
   [:table
    [:thead
     [:tr [:th "Date saved"] [:th "Author"] [:th "Optimisation"] [:th]]]
    [:tbody
     (for [v versions]
       [:tr {:key (:id v)}
        [:td (:created v)]
        [:td (:user-name v)]
        [:td (cond
               (:has-run v) "Finished"
               (:job-id v) "In queue")]
        
        [:td
         [:a {:href (str "map/" map-id "/net/" (:id v))} "view"]
         " "
         [:a {:href (str "map/" map-id "/net/" (:id v) "/data.json") :title "Download"} "↓"]
         ]])]]])

(rum/defcs delete-project-widget < (rum/local nil ::wrong-name)
  [state {:keys [on-close on-delete]}
   project wrong-name]
  (let [wrong-name-js (::wrong-name state)]
    [:div
     [:div "Do you really want to delete this project?"]
     [:div "This will delete everything related to it:"
      [:ul
       (for [m (:maps project)]
         [:li [:b (:name m)] " (map)"
          [:ul
           (for [n (:networks m)]
             [:li [:b (:name n)] " (network)"])]])]]
     [:div
      {:style {:margin-bottom :1em}}
      "If you really want to delete the project enter its name "
      [:span {:style {:font-family :Monospace
                      :font-size :2em
                      :border "1px #aaa solid"
                      :background "#fff"}}
       (:name project)]
      " below and click delete"]
     (when (or wrong-name @wrong-name-js)
       [:div "The project name was not entered correctly. "
        "You need to enter the project name below to delete the project."])
     [:div
      [:input {:ref :delete-field
               :style {:font-size :2em
                       :background "#faa"
                       :color :black}
               :type :text :name :project-name}]
      [:div
       (when on-close [:button {:on-click on-close} "Cancel"])
       (when on-delete [:button {:on-click
                                 #(if (= (string/lower-case
                                          (.-value (rum/ref state :delete-field)))
                                         (string/lower-case (:name project)))
                                    (on-delete)
                                    (reset! wrong-name-js true))}
                        "DELETE"])]]]))

(rum/defc project-page-body [project]
  (let [am-admin (:user-is-admin project)]
    [:div
     [:div.card
      [:div.flex-cols
       [:div {:style {:flex-grow 1}}
        (let [d (:description project)]
          (if (string/blank? d)
            [:em "This project has no description"]
            [:span d]))]
       [:div
        [:a.button {:style {:margin-left :1em}
                    :on-click (fn-js [e]
                         (let [user-state (atom (:users project))]
                           (show-dialog!
                            (project-user-list
                             {:on-save
                              #(do (POST "users" {:params {:users@user-state}})
                                   (close-dialog!))
                              :on-close close-dialog!}
                             user-state))
                           (.preventDefault e)))
                    :href "users"}
         (let [n (count (:users project))]
           (str n " USER" (when (> n 1) "S") " " symbols/person))
         ]
        
        (when am-admin
          [:a.button
           {:style {:margin-left :1em}
            :href "delete"
            :on-click
            (fn-js [e]
              (show-dialog!
               (delete-project-widget
                {:on-delete
                 #(POST "delete" {:params {:project-name (:name project)}
                                  :handler (fn [b] (println "success in delete") (js/window.location.replace "/"))
                                  :error-handler (fn [e] (println "error in delete")(close-dialog!))})
                 :on-close #(close-dialog!)}
                project nil))
              (.preventDefault e))}
           
           "DELETE PROJECT " symbols/dustbin])
        [:a.button {:style {:margin-left :1em}
                    :href "map/new"} "NEW MAP " symbols/plus]]]]

     
     (if-let [maps (seq (:maps project))]
       (for [m maps]
         [:div.card {:key (:id m)}
          [:div.flex-cols
           [:h1 {:style {:flex-grow 1}}
            (:name m)]
           [:div
            [:a.button
             {:style {:margin-left :1em}
              :href (str "map/" (:id m) "/data.json")} "DOWNLOAD " symbols/down-arrow]
            [:a.button
             {:style {:margin-left :1em}
              :href (str "map/" (:id m) "/delete")} "DELETE MAP " symbols/dustbin]
            (if (:import-completed m)
              [:a.button {:style {:margin-left :1em}
                          :href (str "map/" (:id m) "/net/new")} "NEW NETWORK " symbols/plus])
            ]]
          
          [:span (:description m)]

          (if (:import-completed m)
            [:div
             [:img {:style {:float :right}
                    :src (str "map/" (:id m) "/icon.png")
                    :alt (str "An image for the map " (:name m)) }]
             
             (if-let [networks (seq (:networks m))]
               [:div.flex-grid
                (for [[name versions] networks]
                  (let [max-id (reduce max (map :id versions))
                        net-url (str "map/" (:id m) "/net/" (reduce max (map :id versions)))]
                    [:div.card {:key name :style {:width :20em}}
                     [:div.flex-cols
                      [:b [:a {:href net-url} name]]
                      [:div {:style {:margin-left :auto}}
                       (when (seq (rest versions))
                         [:a {:on-click
                              (fn-js [e]
                                (show-dialog! (version-list-widget (:id m) versions))
                                (.preventDefault e))
                              
                              :href (str "map/" (:id m) "/net/history/" name)}
                          (count versions) " versions"])
                       [:a {:style {:margin-left :1em}
                            :href (str net-url "/data.json")}
                        symbols/down-arrow]
                       
                       [:a {:style {:margin-left :1em}
                            :href (str "map/" (:id m) "/net/delete/" name)}
                        symbols/dustbin]

                       (when (some (fn [v]
                                     (and (:job-id v) (not (:has-run v))))
                                   versions)
                         spinner)
                       ]]
                     
                     ]))]

               [:div
                "This map has no network designs associated with it yet."])

             [:br {:style {:clear :both}}]]

            [:div
             spinner
             "Map import in progress..."
             (:status m)]
            
            )])
       [:div.card
        "This project has no maps in it yet. "
        "Get started by creating a new map above."])]))


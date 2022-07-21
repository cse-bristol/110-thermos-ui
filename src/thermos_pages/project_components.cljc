;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-pages.project-components
  (:require [clojure.string :as string]
            [rum.core :as rum]
            [net.cgrand.macrovich :as macro]
            [thermos-pages.common :refer [fn-js] :refer-macros [fn-js]]
            [thermos-frontend.format :refer [si-number]]
            [thermos-pages.spinner :refer [spinner]]
            [thermos-util :refer [to-fixed format-seconds]]
            [ajax.core :refer [POST DELETE]]
            [thermos-pages.symbols :as symbols]
            [thermos-pages.restriction-components :as restriction-comps]
            #?@(:cljs
                [[thermos-pages.dialog :refer [show-delete-dialog! show-dialog! close-dialog!]]])
            [clojure.string :as str]))

(defn- remove-index [v i]
  (vec (concat (subvec v 0 i) (subvec v (inc i)))))

(defn- parse-emails [in]
  (let [parts (string/split in #"[;,\n]"+)]
    (for [part parts]
      (let [part (string/trim part)
            email-part (re-find #"[^<> ]+@[^<> ]+" part)]
        (if email-part
          {:id (string/lower-case (string/trim email-part))     :name (string/trim part)}
          {:id (string/lower-case (string/trim part)) :name (string/trim part)})))))

(rum/defcs project-user-list < rum/reactive
  [state
   {:keys [on-close on-save]}
   users is-public]
  [:div
   [:h1 "Project participants"]
   [:div
    (for [[i u] (map-indexed vector (rum/react users))]
      [:div.flex-cols {:key (:id u)
                       :style {:padding :0.25em
                               :border "1px #eee solid"
                               :border-radius :0.5em
                               :margin-top :0.5em
                               }}
       [:div.flex-grow
        [:a {:href (str "mailto:" (:id u))} (:name u)]]

       (when (= :admin (:auth u))
         [:span {:style {:border "1px #eee solid" :border-radius :4px :padding :2px}} "project admin"])
       (when (:new u)
         [:span {:style {:background "#afa" :border "1px #eee solid" :border-radius :4px :padding :2px :margin :2px}} "New"])

       (if (= :admin (:auth u))
         [:button.button {:on-click #(swap! users assoc-in [i :auth] :read)
                          :style {:margin-left :1em} :title "Demote from admin"} "↓"]
         [:button.button {:on-click #(swap! users assoc-in [i :auth] :admin)
                          :style {:margin-left :1em}:title "Promote to admin"} "↑"])

       [:button.button {:on-click #(swap! users remove-index i)
                        :title "Remove from project"} symbols/cross]
       ])

    [:div.flex-cols {:style {:margin-top :1em}}
     [:input.input.text-input.flex-grow {:ref "invite" :placeholder
                                         "comma-separated email addresses"}]
     [:button.button {:style { :margin-left :1em}
                      :on-click #(let [input (rum/ref state "invite")
                                       input-value (.-value input)
                                       emails (parse-emails input-value)]
                                   (swap! users
                                          into
                                          (for [e emails] (assoc e :new true :auth :read)))
                                   (set! (.-value input) nil))
                      } "Add"]]
    
    ]
   
   [:h1 "Global visibility"]
   [:div
    [:label
     [:input {:type :checkbox
              :checked (rum/react is-public)
              :on-click #(reset! is-public (-> % .-target .-checked))
              }]
     "Make project, maps, and networks visible to anyone who knows the URL"]]
   
   [:div.flex-cols {:style {:margin-top :1em}}
    (when on-close [:button.button {:on-click on-close :style {:margin-left :auto}} "Close"])
    (when on-save  [:button.button {:on-click on-save :style {:margin-left :1em}} "Save changes"])]])

(rum/defc version-list-widget [map-id versions]
  [:div {:on-click (fn-js [] (close-dialog!))
         :style {:max-height :70%
                 :overflow-y :auto}}
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

(rum/defc table [{columns :columns rows :rows row-key :row-key
                  :as params}]
  [:table
   (dissoc params :columns :rows :row-key)
   [:thead
    (loop [columns   columns
           group-row []
           title-row [:tr]
           cur-title nil
           span      0]
      (if (empty? columns)
        (if (= [] group-row)
          (list title-row)
          (list (into [:tr.groups]
                      (conj group-row
                            [(if cur-title
                               :th.group
                               :th) {:colspan span} cur-title]))
                title-row))

        (let [[column & columns] columns
              group-title (:group column)
              next-group (or (not= group-title cur-title)
                             (nil? group-title))]
          (recur columns
                 (cond-> group-row
                   (and (pos? span)
                        next-group)
                   (conj  [(if cur-title
                             :th.group
                             :th) {:colspan span} cur-title]))
                 
                 (conj title-row [(if (and group-title next-group)
                                    :th.group
                                    :th) (:title column)])
                 group-title
                 (if next-group 1 (inc span)))
          )))]
   
   [:tbody
    (for [row rows]
      [:tr {:key (row-key row)}
       (for [{key :key value :value} columns]
         (let [value (or value key)]
           [:td {:key key}
            (value row)]))])]])

(defn- today-date []
  #?(:clj
     (.format (java.text.SimpleDateFormat. "yyyy-MM-dd")
              (java.util.Date.))
     :cljs
     (subs (.toISOString (js/Date.)) 0 10)))

(defn- tidy-date [today date]
  (let [date-part (subs date 0 10)]
    (if (= date-part today)
      (subs date 11 19)
      date-part)))

(defn desc [a b] (compare b a))

(defn- percentage [x]  (and x (str (to-fixed (* 100 x) 2) "%")))
(defn- cents [x]       (and x (str (* 100 x ) "c")))
(defn- kilo-number [x] (si-number (* x 1000)))
(defn- mega-number [x] (si-number (* x 1000000)))

(rum/defcs network-table < (rum/local nil ::expanded-row-name)
  [{*expanded ::expanded-row-name} map-id networks {on-event :on-event user-auth :user-auth}]
  (let [today         (today-date)
        expanded-name @*expanded]
    (table
     {:class "network-table"
      :rows
      (let [networks
            (->>
             (for [[name versions] networks
                   :let            [versions (sort-by :id desc versions)]]
               (merge (first versions)
                      {:versions versions
                       :name     name})
               )
             (sort-by :id desc))
            ]
        (if expanded-name
          (mapcat
           (fn [x]
             (if (= (:name x) expanded-name)
               (concat [x] (rest (:versions x)))
               [x]))
           networks)
          
          networks))
      
      :columns
      (let [meta-column
            (fn [& {:keys [fmt] :as m}]
              (assoc (dissoc m :fmt)
                     :value (comp (or fmt identity) (:value m (:key m)) :meta)))]
        
        [{:key   :controls
          :value (fn [r]
                   [:span
                    (when (and user-auth (:name r))
                      [:a
                       {:on-click
                        (fn-js [e]
                          (show-delete-dialog!
                           {:name      (:name r)
                            :message   "Are you sure you want to delete this network?"
                            :on-delete #(DELETE (str "map/" map-id "/"
                                                     "net/" (:name r))
                                            {:handler on-event})})
                          (.preventDefault e))
                        
                        :href (str "map/" map-id
                                   "/net/delete/" (:name r))}
                       [:span symbols/delete]])
                    " "
                    [:a
                     {:title "Download this network as GIS data"
                      :href  (str "map/" map-id
                                  "/net/" (:id r)
                                  "/data.json")}
                     [:span symbols/download]]
                    ])
          :title ""
          }
         {:key   :state
          :value #(cond
                    (:has-run %)
                    [:b {:title "Model run finished"} "✓"]
                    
                    (:job-id %)
                    [:button
                     {:style {:border :none :background :none
                              :cursor :pointer}

                      :on-click
                      (fn-js
                        [e]
                        (when (js/confirm "Cancel running job?")
                          (POST (str "/admin/job/" (:job-id %))
                              {:params {:action "cancel"}})
                          ))
                      }
                     (spinner {:size 16})])
          :title ""
          }
         {:key   :name
          :value (fn [row]
                   [:a {:href  (str "map/" map-id
                                    "/net/" (:id row))
                        :title (str (:meta row))}
                    (or (:name row) [:span {:style {:margin-left :1em}} "v" (:id row)])])
          :title "Name"}
         
         {:key   :author
          :value :user-name
          :title "Author"}

         {:key   :date
          :value (comp (partial tidy-date today) :created)
          :title "Date"}
         
         {:key   :expando
          :value (fn [row]
                   (when (seq (:versions row))
                     [:span {:style    {:cursor      :pointer
                                        :display     :inline-block
                                        :font-weight (when (= (:name row) expanded-name) :bold)}
                             :on-click #(swap! *expanded (fn [x] (if (= (:name row) x) nil (:name row))))}
                      (str (count (:versions row)) " versions")
                      ]))
          :title "History"}

         ;; (meta-column :key :mode                  :title "Mode"               )
         (meta-column :key :objective             :title "Objective"          )
         (meta-column :key :npv-term              :title "Term"               )
         (meta-column :key :npv-rate              :title "Discount"
                      :fmt percentage)
         (meta-column :key :input-building-count  :title "In problem" :group "Buildings")
         (meta-column :key :network-count :title "On network" :group "Buildings")
         (meta-column :key :input-building-kwh    :title "Demand (Wh/yr)"
                      :fmt si-number :group "Buildings")
         
         (meta-column :key :input-path-length     :title "In problem (m)" :group "Paths"
                      :fmt si-number)
         (meta-column :key :network-length :title "In network (m)" :fmt si-number :group "Paths")
         
         
         (meta-column :key :supply-prices         :title "Heat price"
                      :fmt #(string/join ", " (map cents (set %))) :group "Supply")
                  
         (meta-column :key :network-kwh :title "Capacity (Wh)" :fmt kilo-number :group "Supply")
         (meta-column :key :network-kwp :title "Capacity (Wp)" :fmt kilo-number :group "Supply")


         (meta-column :key :solution-state     :title "State" :group "Solution")
         (meta-column :key :mip-gap     :title "Gap" :fmt percentage :group "Solution")
         (meta-column :key :objective-value :title "Value" :fmt si-number :group "Solution")
         (meta-column :key :runtime :title "Runtime" :fmt format-seconds :group "Solution")
         

         
         ])
      :row-key :id
      })))

(defn- estimation-method-label [method-name]
  (let [method-name (name method-name)]
    (cond-> ""
      (= "given" method-name)
      (str "Demand input as part of user GIS data")

      (str/ends-with? method-name "svm")
      (str "Support vector machine")

      (str/ends-with? method-name "lm")
      (str "Linear model")
      
      (str/starts-with? method-name "3d-")
      (str " using high-quality 3d predictors.")
      
      (str/starts-with? method-name "2d-")
      (str " using low-quality 2d predictors."))))

(defn- mostly-bad-estimates [stats]
  (try (let [total (reduce + 0 (vals stats))
             bad   (reduce + 0
                           (for [[k v] stats :when (str/starts-with? (name k) "2d-")]
                             v))]
         (> (/ bad total) 0.5))
       (catch #?(:cljs :default
                 :clj Exception) e)))

(rum/defc buttons [& content]
  [:div.flex-cols {:style {:flex-wrap :wrap}}
   (for [[n item] (map-indexed vector content) :when item]
     [:div {:key n
            :style {:margin-bottom :0.5em :margin-left :1em}} item])])

(rum/defcs map-component
  [{} m & {:keys [on-event user-auth] :or {on-event #()}}]
  [:div.wide.card {:key (:id m)
                   :style {:flex-grow 1}}
   [:div.flex-cols
    [:h1 {:style {:flex-grow 1}}
     (case (keyword (:state m))
       (:ready :running)
       [:div (spinner {:size 16})]
       :completed
       [:img {:width :32px :height :32px :src (str "map/" (:id m) "/icon.png")
              :style {:margin-right :1em
                      :vertical-align :middle}
              :alt (str "An image for the map " (:name m))}]
       "")
     
     [:span (:name m)]]

    (buttons
     [:a.button
      {:href (str "map/" (:id m) "/data.json")} "DOWNLOAD " symbols/download]
     (when user-auth
       [:a.button
        {:on-click (fn-js [e]
                     (show-delete-dialog!
                      {:name (:name m)
                       :message [:span "Are you sure you want to delete this map "
                                 "and the " (count (:networks m))
                                 " network problems associated with it?"]
                       :on-delete
                       ;; issue the relevant request - the map should disappear later.
                       #(DELETE (str "map/" (:id m))
                            :handler on-event)})
                     
                     (.preventDefault e))
         
         :href (str "map/" (:id m) "/delete")} "DELETE " symbols/delete])
     (when (:import-completed m)
       [:a.button {:href (str "map/" (:id m) "/net/new")} "HEAT NET " symbols/plus])
     (when (:import-completed m)
       [:a.button {:href (str "map/" (:id m) "/net/new?mode=cooling")} "COLD NET " symbols/plus])
     )
    ]
   
   (when-let [description (:description m)] [:div description])
   
   (if (:import-completed m)
     [:div.flex-rows.flex-grow {:style {:max-width :100%}}
      [:div.flex-grow 
       {:style {:margin-top :1em :max-width :100%}}
       (if-let [networks (seq (:networks m))]
         [:div {:style {:max-width :100%}} [:b "Networks:"]
          (network-table (:id m) networks {:on-event on-event :user-auth user-auth})]
          
         "This map has no network designs associated with it yet.")]

      [:div {:style {:margin-top :1em}}
       [:b "Demand estimates: "]
       (let [stats (:estimation-stats m)
             tot (reduce + 0 (vals stats))]
         (doall
          (interpose
           ", "
           (for [[k v] (sort-by second desc (:estimation-stats m))]
             [:span
              (to-fixed (* 100 (/ v tot)) 0) "% "
              (name k)])))
         
         )
       
       
       (when (mostly-bad-estimates (:estimation-stats m))
         [:div {:style {:margin-top :1em}}
          [:span {:style {:background :pink :color :white
                          :border-radius :0.5em
                          :padding-left :0.25em
                          :padding-right :0.25em
                          :margin-right :0.25em
                          :font-weight :bold}} "Warning: "]
          "A majority of the demand estimates in this map are using low-quality 2d predictors. "
          "The estimates may be inaccurate."])]
      
      ]
     

     (case (keyword (:state m))
       :ready
       [:div 
        "Map import is queued, and waiting to start"]
       :running
       [:div
        (str (:message m)
             ", "
             (or (:progress m) 0)
             "% complete")]
       :completed
       [:div
        "Map import is completed, but not ready for presentation"]
       :failed
       [:div "Map has failed to import, because of an error:"
        [:div (:message m)]]
       :cancel
       [:div (spinner) "Map import cancel requested"]
       :cancelling
       [:div (spinner) "Map import cancelling"]
       :cancelled
       [:div "Map import cancelled"]

       [:div "Map import status unknown!"
        [:pre {:style {:white-space :pre-wrap}}
         (str m)]
        ]))])

(rum/defc project-page-body [project & {:keys [on-event] :or {on-event #()}}]
  (let [am-admin (:user-is-admin project)
        user-auth (:user-auth project)
        me (:user project)
        other-admins (filter
                      #(and (= :admin (:auth %))
                            (not= me (:id %)))
                      (:users project))
        restriction-info (:restriction-info project)]
    [:div
     [:div
      (restriction-comps/show-user-restrictions restriction-info :as-card true)]
     
     [:div.wide.card {:style {:margin-top :0.1em}}
      [:div.flex-cols
       [:h1 {:style {:flex-grow 1}}
        (let [d (:description project)]
          (if (string/blank? d)
            "Project options:"
            d))
        
        ]
       
       (buttons
        (when user-auth
          [:button.button
           {:on-click (fn-js [e]
                        (let [user-state (atom (:users project))
                              is-public  (atom (:public project))
                              ]
                          (show-dialog!
                           (project-user-list
                            {:on-save
                             #(do (POST "users" {:params {:users @user-state
                                                          :public @is-public}})
                                  (on-event)
                                  (close-dialog!))
                             :on-close close-dialog!}
                            user-state is-public))
                          (.preventDefault e)))
            :href "users"}
           (let [n (count (:users project))]
             (str n "USER" (when (> n 1) "S") " " symbols/person))
           ])
        
        (when am-admin
          [:button.button
           {:href "delete"
            :on-click
            (fn-js [e]
              (show-delete-dialog!
               {:name (:name project)
                :message [:div
                          [:div "Are you sure you want to delete this project? "]
                          [:div "All these associated maps and networks will be deleted: "]
                          [:ul
                           (for [m (:maps project)]
                             [:li (:name m)
                              [:ul
                               (for [n (:networks m)]
                                 [:li (first n)])]])]]
                :confirm-name true
                :on-delete
                #(DELETE "" {:handler (fn [b] (js/window.location.replace "/"))})})
              
              (.preventDefault e))
            }
           
           "DELETE " symbols/delete])

        (when (and user-auth (> (count (:users project)) 1))
          [:button.button
           {:on-click
            (fn-js [e]
              (show-dialog!
               [:div
                [:p "If you leave this project, you will need to ask another admin user to invite you back in to undo this."]

                (when-not (seq other-admins)
                  [:p "Because there are no other admins, all other users will be converted into admins if you leave."])
                
                [:div.flex-cols
                 [:button.button {:style {:margin-left :auto} :on-click close-dialog!} "CANCEL"]
                 [:button.button.button--danger
                  {:on-click (fn-js [e]
                               (POST "leave" {:handler #(js/window.location.replace "/")})
                               )}
                  "LEAVE"]]
                ]
               )
              (.preventDefault e)
              )
            }
           "LEAVE 👋" 
           ])
        
        (when user-auth
          (if (and (:max-gis-features restriction-info)
                   (> (:num-gis-features restriction-info) (:max-gis-features restriction-info)))
            [:button.button.button--disabled {:disabled true} "MAP " symbols/plus]
            [:a.button {:href "map/new"} "MAP " symbols/plus]))
        
        (when user-auth
          [:a.button {:href "lidar"} "LIDAR"]))]]

     ;; data uploady box...
     
     (if-let [maps (seq (sort-by :name (:maps project)))]
       ;; maps in a tab-strip?
       [:div
        {:style {:display :flex
                 :flex-direction :column}}
        
        (for [m maps]
          (map-component m :on-event on-event :user-auth user-auth))]
       [:div.wide.card
        "This project has no maps in it yet. "
        "Get started by creating a new map above."])]))


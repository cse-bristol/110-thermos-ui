;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.pages.landing-page
  (:require [thermos-backend.pages.common :refer [page]]
            [thermos-pages.restriction-components :as restriction-comps]
            [thermos-pages.common :refer [style]]
            [thermos-pages.symbols :as symbols]
            [thermos-frontend.format :refer [si-number]]
            [thermos-util :refer [format-seconds]]
            [thermos-backend.restrictions :as restrictions]
            [clojure.string :as string]
            [thermos-backend.changelog :refer [changelog]]
            ))

(defn- project-name [project]
  (if (string/blank? (:name project))
    "The project with no name"
    (:name project)))

(defn- date-time
  "A timestamp from the database as yyyy-mm-dd hh:mm"
  [t]
  (when t (subs (str t) 0 16)))

(defn- tidy-date
  "Just the time for today, otherwise just the date"
  [today t]
  (when t
    (let [s (date-time t)]
      (if (string/starts-with? s today) (subs s 11) (subs s 0 10)))))

(defn- today []
  (str (java.time.LocalDate/now)))

(defn- sortable
  "A header for a .sortable table; pages.js sorts by the data-sort
  attribute of the cells in this column, or else their text."
  [label & [type sorted]]
  [:th {:data-sort-type (name (or type :text)) :title "Click to sort"
        :class (when sorted (str "sorted-" (name sorted)))}
   label])

(defn- project-table [user projects]
  (let [today (today)
        sysadmin (= :admin (:auth user))]
    [:table.project-table.sortable
     [:thead
      [:tr
       (sortable "Name")
       (sortable "Maps" :number)
       (sortable "Networks" :number)
       (sortable "Members" :number)
       (sortable "Your role")
       (sortable "Last saved" :date :desc)
       [:th]]]
     [:tbody
      (for [{:keys [id description auth public running last-saved] :as project} projects
            :let [is-admin (or sysadmin (= :admin auth))
                  dialog (fn [label title op]
                           [:a.button {:href "#" :title title
                                       :hx-get (str "/project/" id "/" op)
                                       :hx-target "#modal"}
                            label])]]
        [:tr
         [:td.name {:data-sort (string/lower-case (project-name project))}
          [:a {:href (str "/project/" id)} [:b (project-name project)]]
          (when public
            [:span.tag {:title "Anyone with the link can see this project"} "public"])
          (when (pos? (or running 0))
            [:span.tag.running {:title "Model runs queued or running"}
             "⏳ " running " running"])
          (when-not (string/blank? description)
            [:div.description description])]
         [:td.number (:map-count project)]
         [:td.number (:network-count project)]
         [:td.number (:user-count project)]
         [:td (case auth :admin "Admin" :write "Member" :read "Read only" (some-> auth name))]
         [:td.date {:data-sort (or (date-time last-saved) "")
                    :title (date-time last-saved)}
          (or (tidy-date today last-saved) [:span.empty "never"])]
         [:td.actions
          (when is-admin (dialog symbols/rename "Rename project" "rename?return=/"))
          (when is-admin (dialog symbols/delete "Delete project" "delete"))
          (when (and auth (> (:user-count project) 1))
            (dialog "👋" "Leave project" "leave"))]])]]))

(defn- run-summary [{{:keys [solution-state objective-value network-count runtime]} :meta}]
  (string/join
   ", "
   (remove nil?
           [solution-state
            (when network-count (str network-count " buildings connected"))
            (when objective-value (str "value " (si-number objective-value)))
            (when runtime (str "took " (format-seconds runtime)))])))

(defn- recent-runs [runs]
  (let [today (today)]
    [:div.card
     [:h1 "Recent model runs"]
     [:table.project-table
      [:tbody
       (for [{:keys [id name map-id map-name project-id project-name state finished] :as run} runs]
         [:tr
          [:td {:title (clojure.core/name state)}
           (case state
             :completed "✓"
             :failed    [:span.failed "✗"]
             :cancelled [:span.empty "—"]
             nil)]
          [:td [:a {:href (str "/project/" project-id "/map/" map-id "/net/" id)} [:b name]]]
          [:td [:a {:href (str "/project/" project-id)} project-name] " / " map-name]
          [:td.description (if (= :completed state)
                             (run-summary run)
                             (clojure.core/name state))]
          [:td.date {:title (date-time finished)} (tidy-date today finished)]])]]]))

(defn landing-page [user projects runs]
  (page
   {:title (str "Welcome, " (:name user))
    :css ["/css/projects.css"]}
   [:div
    (restriction-comps/show-user-restrictions (restrictions/get-restriction-info user) 
                                              :as-card true)
    [:div.card
     [:div.flex-cols {:style (style :align-items :center)}
      (if (seq projects)
        (let [c (count projects)]
          [:h1 "You are participating in " c " project" (when (> c 1) "s")])
        [:h1 "You have no projects"])
      [:a.button {:style {:margin-left "auto"}
                  :href "/project/new"} "New Project"]]
     (when (seq projects)
       (project-table user (sort-by #(str (:last-saved %)) #(compare %2 %1) projects)))]

    (when (seq runs)
      (recent-runs runs))

    (let [last-log (:changelog-seen user 0)
          max-log (count changelog)
          new-items (- max-log last-log)
          changelog (take new-items changelog)]
      (when (seq changelog)
        [:div {:style {:position "fixed"
                       :background "rgba(1,1,1,0.5)"
                       :left 0
                       :top 0
                       :width "100%"
                       :height "100%"
                       }}
         [:div.card {:style {:margin "5em"}}
          [:div.flex-cols
           [:h1 "THERMOS has been updated"]
           [:a.button {:href "/help/changelog"
                       :target "help"
                       :style {:margin-left "auto"}}
            "🛈 More details"]
           [:a.button {:href (str "?changes=" max-log)
                       :style {:margin-left "1em"}}
            "Close"]]
          
          [:p {:style {:margin-bottom 0}}
           "Now featuring: "
           (interpose " • "
                      (for [rel changelog
                            change (:changes rel)]
                        (:title change)
                        ))]]
         ]))
    ]
   ))

;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.pages.admin
  "The admin page has tabs (overview, users, projects and jobs), which
  htmx swaps into #admin-body. Most things in them are changed in place,
  or in dialogs in #modal; responses can also put a message in
  #admin-flash with `flash`."
  (:require [thermos-backend.pages.common :refer [page]]
            [thermos-backend.db.users :refer [user-auth-types user-auth-ordering]]
            [thermos-backend.db.admin :as admin-db]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]])
  (:import [java.time.format DateTimeFormatter]))

;; Formatting

(def ^:private time-format (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))

(defn- fmt-time [t]
  (some-> (cond
            (instance? java.sql.Timestamp t) (.toLocalDateTime ^java.sql.Timestamp t)
            (instance? java.time.LocalDateTime t) t)
          (.format time-format)))

(defn- fmt-bytes [n]
  (loop [n (double (or n 0))
         [unit & units] ["B" "kB" "MB" "GB" "TB"]]
    (if (and (>= n 1024) (seq units))
      (recur (/ n 1024) units)
      (format (if (= "B" unit) "%.0f %s" "%.1f %s") n unit))))

(defn- fmt-duration [seconds]
  (when seconds
    (let [s (long seconds)
          h (quot s 3600)
          m (quot (rem s 3600) 60)]
      (cond
        (pos? h) (format "%dh %02dm" h m)
        (pos? m) (format "%dm %02ds" m (rem s 60))
        :else    (str s "s")))))

(defn- json-str [x] (json/write-str x))

;; Bits used all over

(def ^:private tabs
  [[:overview "Overview" "/admin"]
   [:users    "Users"    "/admin/users"]
   [:projects "Projects" "/admin/projects"]
   [:jobs     "Jobs"     "/admin/jobs"]])

(defn- nav-link
  "A link that shows `url` in the admin page, as if the browser had gone there"
  [url attrs & content]
  (into [:a (merge {:href url :hx-get url :hx-target "#admin-body"
                    :hx-swap "outerHTML" :hx-push-url "true"}
                   attrs)]
        content))

(defn- tab
  "The admin page's body, for tab `active`. This stops htmx keeping pages
  in its history cache, so admin data isn't left in local storage; going
  back gets the page again instead."
  [active & content]
  [:div#admin-body {:hx-history "false"}
   [:nav.tabs__tabs
    (for [[k label url] tabs]
      (nav-link url {:class (str "tabs__tab" (when (= k active) " tabs__tab--active"))}
                label))]
   content])

(defn admin-page
  "The whole admin page, with the content of a tab"
  [body]
  (page
   {:title "THERMOS admin"
    :css ["/css/admin.css"]
    :js ["/js/vendor/leaflet.js" "/js/map-box.js"]}
   [:div#admin-flash]
   body))

(defn flash
  "A message for the top of the page, which a response can include to have it
  swapped in out of band"
  ([message] (flash message false))
  ([message error]
   [:div#admin-flash {:hx-swap-oob "true"}
    [:div {:class (str "admin-flash" (when error " admin-flash--error"))} message]]))

(defn- small-button
  "A small button; `kind` is :danger, :outline or nil"
  [kind attrs label]
  [:button (merge {:type :button
                   :class (str "button button--small"
                               (some->> kind name (str " button--")))}
                  attrs)
   label])

(defn- dialog-buttons [& buttons]
  [:div.admin-dialog-buttons buttons])

(def ^:private close-button
  [:button.button.button--outline {:type :button :data-close true} "Close"])

(defn- options
  "<option>s for `values`, with `current` selected"
  [values current]
  (for [v values]
    [:option {:value v :selected (= v current)} v]))

(defn- auth-select [attrs current]
  [:select attrs (options (map name user-auth-types) current)])

(defn- sort-th
  "A column header which sorts the table when clicked (see pages.js)"
  ([label] (sort-th label :text))
  ([label type]
   [:th {:data-sort-type (name type) :title "Click to sort"} label]))

(defn- limit-note [rows what]
  (when (>= (count rows) admin-db/list-limit)
    [:p.admin-hint "Only the first " admin-db/list-limit " " what
     " are shown; search to find others."]))

;; Overview

(defn- stat [label value & [detail]]
  [:div.admin-stat
   [:div.admin-stat__value value]
   [:div.admin-stat__label label]
   (when detail [:div.admin-stat__detail detail])])

(def ^:private summary-states
  "Job states which always get a column in the queue summary"
  #{"ready" "running" "completed" "failed" "cancelled"})

(defn queue-summary
  "Job counts by state for each queue, from `admin-db/queue-counts`, and
  `capacities`, how many jobs at once each queue's consumer takes"
  [counts capacities]
  (let [queues (sort (distinct (concat (keys counts) (keys capacities))))
        states (filter #(or (summary-states %) (some (fn [c] (get c %)) (vals counts)))
                       admin-db/job-states)]
    [:div#queue-summary
     [:table.admin-table
      [:thead
       [:tr [:th "Queue"] [:th.num "Capacity"]
        (for [s states] [:th.num s])
        [:th]]]
      [:tbody
       (for [q queues :let [c (get counts q)]]
         [:tr
          [:td (nav-link (str "/admin/jobs?queue=" q) {} q)]
          [:td.num (get capacities q "–")]
          (for [s states :let [n (get c s 0)]]
            [:td.num (if (pos? n)
                       (nav-link (str "/admin/jobs?queue=" q "&state=" s) {} n)
                       "–")])
          [:td.admin-actions
           (small-button :outline
                         {:hx-post (str "/admin/queue/" q "/clean")
                          :hx-confirm (str "Delete all the finished jobs on " q "?")
                          :hx-target "#queue-summary" :hx-swap "outerHTML"}
                         "Clean up")
           (small-button :danger
                         {:hx-post (str "/admin/queue/" q "/purge")
                          :hx-confirm (str "Delete ALL the jobs on " q
                                           ", including queued and running ones?"
                                           " Running jobs carry on, but nothing will hear their results.")
                          :hx-target "#queue-summary" :hx-swap "outerHTML"}
                         "Purge")]])]]]))

(defn table-sizes
  "The biggest tables, and a button to empty the tile cache"
  [tables]
  [:div#table-sizes
   [:table.admin-table
    [:thead [:tr [:th "Table"] [:th.num "Rows (roughly)"] [:th.num "Size"]]]
    [:tbody
     (for [t tables]
       [:tr [:td (:name t)] [:td.num (:rows t)] [:td.num (fmt-bytes (:bytes t))]])]]
   [:p
    (small-button :outline
                  {:hx-post "/admin/tile-cache/clear"
                   :hx-confirm "Empty the heat density tile cache? Tiles are drawn again when someone looks at them."
                   :hx-target "#table-sizes" :hx-swap "outerHTML"}
                  "Clear tile cache")]])

(defn overview [{:keys [counts users-by-auth queue-counts capacities tables map-bounds]}]
  (tab
   :overview
   [:div.card
    [:h1 "System"]
    [:div.admin-stats
     (stat "users" (:users counts)
           (list (string/join " · " (for [a (reverse user-auth-types)
                                          :let [n (get users-by-auth (name a))]
                                          :when n]
                                      (str n " " (name a))))
                 [:br]
                 (:new-users counts) " new in the last 30 days, "
                 (:logged-in-users counts) " have logged in"))
     (stat "projects" (:projects counts)
           (list (:public-projects counts) " public, "
                 (if (pos? (:orphan-projects counts 0))
                   (nav-link "/admin/projects?orphans=true" {}
                             (:orphan-projects counts) " with no members")
                   "none without members")))
     (stat "maps" (:maps counts))
     (stat "networks" (:networks counts) (str (:network-versions counts) " saved versions"))
     (stat "parameter sets" (:parameter-sets counts))
     (stat "push subscriptions" (:push-subscriptions counts))
     (stat "database" (fmt-bytes (:db-size counts)))]
    [:p
     [:button.button {:type :button :hx-get "/admin/email" :hx-target "#modal"}
      "Send system email…"]]]

   [:div.admin-grid
    [:div.card [:h1 "Queues"] (queue-summary queue-counts capacities)]
    [:div.card [:h1 "Largest tables"] (table-sizes tables)]]

   [:div.card
    [:h1 "Map extents"]
    [:div.map-box {:data-config (json-str {:boundary map-bounds :labels true})
                   :style "width:100%; height:500px; margin-top:1em"}]]))

(defn email-dialog [{:keys [subject message recipients error]}]
  [:form.admin-dialog {:hx-post "/admin/email" :hx-target "#modal"
                       :hx-confirm (str "Send this to " recipients " people?")}
   [:h1 "Send system email"]
   [:p "This goes to the " recipients
    " users who haven't turned off system messages in their settings."]
   (when error [:p.admin-error error])
   [:label "Subject"
    [:input {:type :text :name "subject" :required true :value subject}]]
   [:label "Message"
    [:textarea {:name "message" :rows 12 :required true} message]]
   (dialog-buttons
    [:button.button {:type :submit} "Send"]
    close-button)])

;; Users

(defn user-row
  "A row of the users table; `me` is the current user's id, whose
  account can't be deleted or have its authority changed here."
  [me {:keys [id name auth has-password login-count created
              project-count recent-jobs system-messages job-emails]}]
  (let [self? (= id me)
        vals  (json-str {:id id})]
    [:tr
     [:td [:a {:href "#" :hx-get "/admin/user" :hx-vals vals :hx-target "#modal"} id]]
     [:td name]
     [:td {:data-sort (user-auth-ordering (keyword auth))}
      (auth-select {:name "auth" :hx-post "/admin/user/auth" :hx-vals vals
                    :hx-target "closest tr" :hx-swap "outerHTML"
                    :disabled self?
                    :title (when self? "You can't change your own authority")}
                   auth)]
     [:td {:data-sort (if has-password 1 0)} (if has-password "✓" "–")]
     [:td.num (or login-count 0)]
     [:td.num project-count]
     [:td.num recent-jobs]
     [:td (fmt-time created)]
     [:td (string/join ", " (cond-> []
                              system-messages (conj "system")
                              job-emails      (conj "jobs")))]
     [:td.admin-actions
      (small-button :outline
                    {:hx-post "/admin/user/reset" :hx-vals vals :hx-swap "none"
                     :hx-confirm (str "Email " id " a link to reset their password?")}
                    "Reset password")
      (small-button :danger
                    {:hx-delete "/admin/user" :hx-vals vals
                     :hx-target "closest tr" :hx-swap "outerHTML"
                     :disabled self?
                     :hx-confirm (str "Delete " id "? They will leave all their projects; "
                                      "networks they saved are kept.")}
                    "Delete")]]))

(defn user-table [me users]
  [:div#user-table {:hx-get "/admin/users/table" :hx-include "#user-filters"
                    :hx-trigger "users-changed from:body" :hx-swap "outerHTML"
                    :hx-disinherit "*"}
   [:table.admin-table.sortable
    [:thead
     [:tr (sort-th "Email") (sort-th "Name") (sort-th "Authority" :number)
      (sort-th "Password" :number) (sort-th "Logins" :number) (sort-th "Projects" :number)
      [:th {:data-sort-type "number" :title "Problems run in the last 7 days"} "Jobs (7d)"]
      (sort-th "Created") [:th "Emails"] [:th]]]
    [:tbody (for [u users] (user-row me u))]]
   (limit-note users "users")])

(defn users-tab [me search users]
  (tab
   :users
   [:div.card
    [:form#user-filters.admin-toolbar
     {:hx-get "/admin/users/table" :hx-trigger "input delay:300ms, submit"
      :hx-target "#user-table" :hx-swap "outerHTML" :hx-disinherit "*"}
     [:input {:type :search :name "q" :value search :placeholder "Search by email or name"}]
     [:button.button {:type :button :hx-get "/admin/users/new" :hx-target "#modal"}
      "New user…"]]
    (user-table me users)]))

(defn user-dialog [user projects gis-features]
  [:div.admin-dialog
   [:h1 (:name user) " — " (:id user)]
   [:table.admin-table.admin-details
    [:tr [:th "Authority"] [:td (:auth user)]]
    [:tr [:th "Created"] [:td (fmt-time (:created user))]]
    [:tr [:th "Logins"] [:td (or (:login-count user) 0)]]
    [:tr [:th "Has a password"] [:td (if (:has-password user) "yes" "no")]]
    [:tr [:th "Problems run, last 7 days"]
     [:td (nav-link (str "/admin/jobs?user=" (:id user)) {:data-close true}
                    (:recent-jobs user))]]
    [:tr [:th "GIS features"] [:td gis-features]]
    [:tr [:th "System emails"] [:td (if (:system-messages user) "yes" "no")]]
    [:tr [:th "Job emails"] [:td (if (:job-emails user) "yes" "no")]]]
   [:h1 "Projects"]
   (if (seq projects)
     [:table.admin-table
      [:thead [:tr [:th "Project"] [:th "Role"]]]
      [:tbody
       (for [p projects]
         [:tr [:td [:a {:href (str "/project/" (:id p))} (:name p)]] [:td (:auth p)]])]]
     [:p "None."])
   (dialog-buttons close-button)])

(defn new-user-dialog [{:keys [email name auth send-email error]}]
  [:form.admin-dialog {:hx-post "/admin/users" :hx-target "#modal"}
   [:h1 "New user"]
   (when error [:p.admin-error error])
   [:label "Email" [:input {:type :email :name "email" :required true :value email}]]
   [:label "Name" [:input {:type :text :name "name" :required true :value name}]]
   [:label "Authority " (auth-select {:name "auth"} auth)]
   [:label [:input {:type :checkbox :name "send-email" :value "true" :checked send-email}]
    " Email them a link to set their password"]
   (dialog-buttons
    [:button.button {:type :submit} "Create"]
    close-button)])

;; Projects

(def ^:private project-auths ["read" "write" "admin"])

(defn project-row [{:keys [id name description public map-count network-count
                           last-saved members]}]
  [:tr (when (empty? members) {:class "admin-orphan" :title "This project has no members"})
   [:td.num id]
   [:td [:a {:href (str "/project/" id) :title description} name]]
   [:td (when public "public")]
   [:td (if (seq members)
          (interpose ", " (for [m members]
                            [:span (:id m)
                             (when (not= "write" (:auth m)) [:small " (" (:auth m) ")"])]))
          [:em "none"])]
   [:td.num map-count]
   [:td.num network-count]
   [:td (fmt-time last-saved)]
   [:td.admin-actions
    (small-button :outline
                  {:hx-get (str "/admin/project/" id "/members") :hx-target "#modal"}
                  "Members…")
    (small-button :danger
                  {:hx-delete (str "/admin/project/" id)
                   :hx-target "closest tr" :hx-swap "outerHTML"
                   :hx-confirm (str "Delete the project " name ", with its "
                                    map-count " maps and " network-count
                                    " networks? This can't be undone.")}
                  "Delete")]])

(defn project-table [projects]
  [:div#project-table {:hx-get "/admin/projects/table" :hx-include "#project-filters"
                       :hx-trigger "projects-changed from:body" :hx-swap "outerHTML"
                       :hx-disinherit "*"}
   [:table.admin-table.sortable
    [:thead
     [:tr (sort-th "ID" :number) (sort-th "Name") (sort-th "Public") (sort-th "Members")
      (sort-th "Maps" :number) (sort-th "Networks" :number) (sort-th "Last saved") [:th]]]
    [:tbody (map project-row projects)]]
   (limit-note projects "projects")])

(defn projects-tab [{:keys [search orphans]} projects]
  (tab
   :projects
   [:div.card
    [:form#project-filters.admin-toolbar
     {:hx-get "/admin/projects/table" :hx-trigger "input delay:300ms, submit"
      :hx-target "#project-table" :hx-swap "outerHTML" :hx-disinherit "*"}
     [:input {:type :search :name "q" :value search
              :placeholder "Search by name, id or member"}]
     [:label [:input {:type :checkbox :name "orphans" :value "true" :checked orphans}]
      " Only projects with no members"]]
    (project-table projects)]))

(defn members-dialog
  "The members of `project`, which can be changed; `user-ids` are all
  the users, to choose one to add"
  [project members user-ids error]
  (let [url (str "/admin/project/" (:id project) "/members")
        change (fn [attrs] (merge {:hx-post url :hx-target "#project-members"
                                   :hx-swap "outerHTML"}
                                  attrs))]
    [:div#project-members.admin-dialog
     [:h1 "Members of " (:name project)]
     (when error [:p.admin-error error])
     (if (seq members)
       [:table.admin-table
        [:thead [:tr [:th "User"] [:th "Name"] [:th "Role"] [:th]]]
        [:tbody
         (for [m members]
           [:tr
            [:td (:id m)]
            [:td (:name m)]
            [:td [:select (change {:name "auth"
                                   :hx-vals (json-str {:user (:id m) :op "set"})})
                  (options project-auths (:auth m))]]
            [:td (small-button :danger
                               (change {:hx-vals (json-str {:user (:id m) :op "remove"})
                                        :hx-confirm (str "Take " (:id m) " out of this project?")})
                               "Remove")]])]]
       [:p "This project has no members."])
     [:form.admin-toolbar (change {})
      [:input {:type :hidden :name "op" :value "add"}]
      [:input {:name "user" :list "admin-user-ids" :required true
               :placeholder "Email of an existing user"}]
      [:datalist#admin-user-ids (for [u user-ids] [:option {:value u}])]
      [:select {:name "auth"} (options project-auths "write")]
      [:button.button {:type :submit} "Add"]]
     [:p.admin-hint "People added here aren't sent an email."]
     (dialog-buttons close-button)]))

;; Jobs

(defn- job-owner
  "Links to the project, map and network a job is for, and who saved the network"
  [{:keys [project-id project-name map-id map-name network-id network-name user-id]}]
  (if project-id
    (list
     [:a {:href (str "/project/" project-id)} project-name]
     (when map-id (list " / " map-name))
     (when network-id
       (list " / " [:a {:href (str "/project/" project-id "/map/" map-id "/net/" network-id)}
                    network-name]))
     (when user-id (list [:br] [:small user-id])))
    "–"))

(defn- job-actions
  "Buttons to cancel or restart a job, which say jobs-changed when done"
  [{:keys [id state]}]
  (list
   (when (#{"ready" "running"} state)
     (small-button :danger
                   {:hx-post (str "/admin/job/" id) :hx-vals (json-str {:action "cancel"})
                    :hx-swap "none" :hx-confirm (str "Cancel job " id "?")}
                   "Cancel"))
   (when (#{"completed" "failed" "cancelled"} state)
     (small-button :outline
                   {:hx-post (str "/admin/job/" id) :hx-vals (json-str {:action "restart"})
                    :hx-swap "none" :hx-confirm (str "Run job " id " again?")}
                   "Restart"))))

(defn- job-state [state]
  [:span {:class (str "job-state job-state--" state)} state])

(defn job-row [{:keys [id queue-name state queued started updated expected-runtime progress]
                :as job}]
  [:tr
   [:td.num {:data-sort id}
    [:a {:href (str "/admin/job/" id) :hx-get (str "/admin/job/" id) :hx-target "#modal"} id]]
   [:td queue-name]
   [:td (job-state state)]
   [:td (job-owner job)]
   [:td (fmt-time queued)]
   [:td (fmt-time started)]
   [:td (fmt-time updated)]
   [:td.num {:data-sort (or expected-runtime 0)} (fmt-duration expected-runtime)]
   [:td.num {:data-sort (or progress 0)} (when (and progress (pos? progress)) (format "%.0f%%" (double progress)))]
   [:td.admin-actions (job-actions job)]])

(defn job-table
  "A page of jobs; `jobs` may have one more than fits, to say there's a next page.
  This refreshes itself every few seconds, or when jobs-changed happens."
  [jobs {:keys [page per-page]}]
  (let [more? (> (count jobs) per-page)
        jobs  (take per-page jobs)
        page-button (fn [page label]
                      (small-button :outline
                                    {:hx-get "/admin/jobs/table" :hx-include "#job-filters"
                                     :hx-vals (json-str {:page page})
                                     :hx-target "#job-table" :hx-swap "outerHTML"}
                                    label))]
    [:div#job-table {:hx-get "/admin/jobs/table" :hx-include "#job-filters"
                     :hx-vals (json-str {:page page})
                     :hx-trigger (str "every 5s [document.getElementById('jobs-auto-refresh').checked], "
                                      "jobs-changed from:body")
                     :hx-swap "outerHTML" :hx-disinherit "*"}
     [:table.admin-table.sortable
      [:thead
       [:tr (sort-th "ID" :number) (sort-th "Queue") (sort-th "State") [:th "For"]
        (sort-th "Queued") (sort-th "Started") (sort-th "Updated")
        (sort-th "Expected" :number) (sort-th "Progress" :number) [:th]]]
      [:tbody
       (if (seq jobs)
         (map job-row jobs)
         [:tr [:td {:colspan 10} "No jobs."]])]]
     [:div.admin-pager
      (when (pos? page) (page-button (dec page) "← Newer"))
      [:span "Page " (inc page)]
      (when more? (page-button (inc page) "Older →"))]]))

(defn jobs-tab [queues {:keys [queue state user] :as filters} jobs]
  (tab
   :jobs
   [:div.card
    [:form#job-filters.admin-toolbar
     {:hx-get "/admin/jobs/table" :hx-trigger "input delay:400ms, submit"
      :hx-target "#job-table" :hx-swap "outerHTML" :hx-disinherit "*"}
     [:select {:name "queue"}
      [:option {:value ""} "All queues"]
      (options queues queue)]
     [:select {:name "state"}
      [:option {:value ""} "All states"]
      (options admin-db/job-states state)]
     [:input {:type :search :name "user" :value user :placeholder "Saved by user"}]
     [:label [:input#jobs-auto-refresh {:type :checkbox :checked true}] " Refresh every 5s"]]
    (job-table jobs filters)]))

(defn- job-details
  "What we know about a job: `job` from `admin-db/job`, `details` from
  `queue/job-details`, and `timing` from `queue/job-timing` if it's
  waiting or running"
  [job details timing]
  (list
   [:table.admin-table.admin-details
    [:tr [:th "Queue"] [:td (:queue-name job)]]
    [:tr [:th "State"] [:td (job-state (:state job))]]
    [:tr [:th "For"] [:td (job-owner job)]]
    [:tr [:th "Queued"] [:td (fmt-time (:queued job))]]
    [:tr [:th "Started"] [:td (fmt-time (:started job))]]
    [:tr [:th "Updated"] [:td (fmt-time (:updated job))]]
    [:tr [:th "Expected runtime"] [:td (fmt-duration (:expected-runtime job))]]
    [:tr [:th "Progress"] [:td (some->> (:progress job) double (format "%.0f%%"))]]
    (when timing
      (list
       (when (:position timing)
         [:tr [:th "Place in queue"] [:td (:position timing)]])
       [:tr [:th "Starts within"] [:td (fmt-duration (:starts-in timing))]]
       [:tr [:th "Finishes within"] [:td (fmt-duration (:finishes-in timing))]]))]
   [:details {:open (not (string/blank? (:message details)))}
    [:summary "Message"]
    [:pre (:message details)]]
   [:details
    [:summary "Arguments"]
    [:pre (with-out-str (pprint (:args details)))]]))

(defn job-dialog [job details timing]
  [:div.admin-dialog
   [:h1 "Job " (:id job)]
   (job-details job details timing)
   (dialog-buttons (job-actions job) close-button)])

(defn job-page
  "A job on its own page, for if you're not using htmx"
  [job details timing]
  (page
   {:title (str "THERMOS admin - job " (:id job))
    :css ["/css/admin.css"]}
   [:div.card
    [:p [:a {:href "/admin/jobs"} "← All jobs"]]
    [:h1 "Job " (:id job)]
    (job-details job details timing)
    [:form {:method :POST :action (str "/admin/job/" (:id job))}
     [:button.button {:name "action" :value "restart" :type "submit"} "Re-run job"]
     [:button.button.button--danger {:name "action" :value "cancel" :type "submit"} "Cancel job"]]]))

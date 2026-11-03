;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.pages.parameter-sets
  "The library of parameter sets, and its dialogs (see
  `thermos-backend.routes.parameter-sets`)."
  (:require [thermos-backend.pages.common :refer [page]]
            [thermos-pages.common :refer [style]]
            [thermos-pages.symbols :as symbols]
            [thermos-specs.parameters :as parameters]
            [clojure.data.json :refer [json-str]]
            [clojure.string :as string]))

(defn- dialog-buttons [& buttons]
  (into [:div.flex-cols {:style (style :margin-top :1em :gap :1em :justify-content :flex-end)}
         [:button.button {:type :button :data-close true} "CANCEL"]]
        buttons))

(defn scope-value
  "How a scope is written in a form: system, user, or project:<id>"
  [scope project-id]
  (if (= :project scope) (str "project:" project-id) (name scope)))

(defn parse-scope
  "The [scope project-id] in `s`, as written by `scope-value`"
  [s]
  (let [[_ scope pid] (re-find #"^(system|user|project)(?::(\d+))?$" (or s ""))]
    (when scope
      [(keyword scope) (when pid (Long/parseLong pid))])))

(defn- scope-options
  "Options for a select of the scopes the user can put a set in"
  [{:keys [projects sysadmin]} selected]
  (concat
   [[:option {:value "user" :selected (= selected "user")} "My parameter sets"]]
   (for [p projects
         :let [v (scope-value :project (:id p))]]
     [:option {:value v :selected (= selected v)} (str "Project: " (:name p))])
   (when sysadmin
     [[:option {:value "system" :selected (= selected "system")} "THERMOS (everyone)"]])))

(defn new-set-form [data & {:keys [project-id]}]
  [:form {:method :POST :action "/parameters/new"}
   [:h1 "New parameter set"]
   [:p "A new set starts with THERMOS's built-in parameters, which you can then edit."]
   [:div [:label "Name " [:input.text-input {:type :text :name :name :required true}]]]
   [:div {:style (style :margin-top :0.5em)}
    [:label "Belongs to "
     (into [:select {:name :scope}]
           (scope-options data (if project-id (scope-value :project project-id) "user")))]]
   (dialog-buttons [:button.button {:type :submit} "CREATE"])])

(defn copy-set-form [data ps]
  [:form {:method :POST :action (str "/parameters/" (:id ps) "/copy")}
   [:h1 "Copy " (:name ps)]
   [:div [:label "Name " [:input.text-input {:type :text :name :name :required true
                                             :value (str (:name ps) " (copy)")}]]]
   [:div {:style (style :margin-top :0.5em)}
    [:label "Copy to " (into [:select {:name :scope}] (scope-options data "user"))]]
   (dialog-buttons [:button.button {:type :submit} "COPY"])])

(defn rename-set-form [ps]
  [:form {:method :POST :action (str "/parameters/" (:id ps) "/rename")}
   [:h1 "Rename " (:name ps)]
   [:div [:label "Name " [:input.text-input {:type :text :name :name :required true
                                             :value (:name ps)}]]]
   [:div {:style (style :margin-top :0.5em)}
    [:textarea {:name :description :rows 3 :style (style :width :100%)
                :placeholder "Description"}
     (:description ps)]]
   (dialog-buttons [:button.button {:type :submit} "RENAME"])])

(defn default-set-form
  "Choose where `ps` is a default. Projects the set doesn't belong to
  can't use it unless it is a system or user set."
  [{:keys [projects user-defaults sysadmin system-default]} ps]
  (let [usable-in (fn [p] (or (not= :project (:scope ps)) (= (:id p) (:project-id ps))))
        check (fn [field checked label]
                [:div [:label [:input {:type :checkbox :name field :value "on" :checked checked}]
                       " " label]])]
    [:form {:method :POST :action (str "/parameters/" (:id ps) "/default")}
     [:h1 "Use " (:name ps) " for new networks"]
     [:p "New networks start with the parameters from the first of these which applies: "
      "your choice for the project, the project's default, your own default, and the THERMOS default."]
     (when-not (= :project (:scope ps))
       (check "mine" (= (:id ps) (get user-defaults nil)) "My default everywhere"))
     (for [p projects :when (usable-in p)]
       [:div {:key (:id p)}
        (check (str "mine-" (:id p)) (= (:id ps) (get user-defaults (:id p)))
               (str "My default in " (:name p)))
        (when (or sysadmin (= :admin (:auth p)))
          (check (str "project-" (:id p)) (= (:id ps) (:default-parameter-set-id p))
                 (str "Everyone's default in " (:name p))))])
     (when (and sysadmin (= :system (:scope ps)))
       (check "system" (= (:id ps) system-default) "Everyone's default everywhere"))
     (dialog-buttons [:button.button {:type :submit} "SAVE"])]))

;; The library is a table of sets in sections. On the main page the
;; star is the user's own default (everywhere, or for a project set in
;; its project); on a project's page there are stars for the user's
;; default in the project, and the project's default for everyone.

(defn- star
  "A star to toggle whether set `ps` is a default; `which` is :mine or
  :project, in `project-id` (or everywhere for nil). Not clickable
  unless `can-change`."
  [{:keys [view-project]} ps which project-id on? can-change title]
  (let [symbol (if on? "★" "☆")
        style (style :font-size :1.3em :line-height 1 :padding 0
                     :background :none :border :none
                     :color (if on? "#e0a000" "#999")
                     :cursor (if can-change :pointer :default))]
    (if can-change
      [:button {:type :button :title title :style style
                :hx-post (str "/parameters/" (:id ps) "/star")
                :hx-vals (json-str {"which" (name which)
                                    "project" (str project-id)
                                    "view" (str view-project)})
                :hx-target "#parameter-sets" :hx-swap "outerHTML"}
       symbol]
      [:span {:title title :style style} (when on? symbol)])))

(defn- other-defaults
  "Where else `ps` is a default, other than what the stars show"
  [{:keys [projects user-defaults system-default view-project]} ps]
  (let [project-name (into {} (map (juxt :id :name) projects))]
    (->> (concat
          (when (= (:id ps) system-default) ["THERMOS default"])
          (for [[pid sid] user-defaults
                :when (and (= sid (:id ps)) pid
                           (not= pid view-project)
                           (not= pid (:project-id ps)))]
            (str "Mine in " (project-name pid "a project")))
          (for [p projects
                :when (and (= (:id ps) (:default-parameter-set-id p))
                           (not= (:id p) view-project))]
            (str "Default in " (:name p))))
         (seq))))

(defn- date [d] (when d (subs (str d) 0 10)))

(defn- action [label attrs]
  [:a (merge {:href "#" :style (style :margin-right :0.75em)} attrs) label])

(defn- set-row [{:keys [view-project user-defaults view-project-info] :as data} ps]
  (let [editable ((:can-edit? data) ps)
        project-set (= :project (:scope ps))
        view-query (when view-project (str "?project=" view-project))]
    [:tr
     (if view-project
       (list
        [:td (star data ps :mine view-project
                   (= (:id ps) (get user-defaults view-project)) true
                   "My default for new networks in this project")]
        [:td (star data ps :project view-project
                   (= (:id ps) (:default-parameter-set-id view-project-info))
                   (:can-set-project-default view-project-info)
                   "Everyone's default for new networks in this project")])
       [:td (if project-set
              (star data ps :mine (:project-id ps)
                    (= (:id ps) (get user-defaults (:project-id ps))) true
                    (str "My default for new networks in " (:project-name ps)))
              (star data ps :mine nil
                    (= (:id ps) (get user-defaults nil)) true
                    "My default for new networks"))])
     [:td
      [:a {:href (str "/parameters/" (:id ps))} (:name ps)]
      (when-not (string/blank? (:description ps))
        [:div.description (:description ps)])]
     [:td.categories
      (if (= (set parameters/categories) (set (:categories ps)))
        "Everything"
        (string/join ", " (map #(parameters/category-labels % (name %))
                               (filter (:categories ps) parameters/categories))))]
     [:td.defaults (when-let [d (other-defaults data ps)] (string/join ", " d))]
     [:td.date (date (:updated ps))]
     [:td.actions
      (action (if editable "Edit" "View") {:href (str "/parameters/" (:id ps))})
      (action "Copy" {:hx-get (str "/parameters/" (:id ps) "/copy") :hx-target "#modal"})
      (when editable
        (action "Rename" {:hx-get (str "/parameters/" (:id ps) "/rename") :hx-target "#modal"}))
      (when editable
        (action "Delete" {:hx-delete (str "/parameters/" (:id ps) view-query)
                          :hx-confirm (str "Really delete " (:name ps) "?")
                          :hx-target "#parameter-sets" :hx-swap "outerHTML"}))
      (action "Defaults…" {:hx-get (str "/parameters/" (:id ps) "/default") :hx-target "#modal"
                           :title "Where this set is a default"})]]))

(defn set-list
  "The table of sets, in sections; for one project if :view-project"
  [{:keys [sets projects view-project view-project-info] :as data}]
  (let [by-scope (group-by (juxt :scope :project-id) sets)
        columns (if view-project 7 6)
        section (fn [title k empty]
                  (let [sets (get by-scope k)]
                    (when (or (seq sets) empty)
                      (list
                       [:tr.section [:th {:colspan columns} title]]
                       (if (seq sets)
                         (for [ps sets] (set-row data ps))
                         [:tr [:td.empty {:colspan columns} empty]])))))]
    [:div#parameter-sets
     [:table.parameter-table
      [:thead
       [:tr
        (if view-project
          (list [:th {:title "My default for new networks in this project"} "Mine"]
                [:th {:title "Everyone's default for new networks in this project"} "Project"])
          [:th {:title "My default for new networks"} "★"])
        [:th "Name"] [:th "Contains"] [:th "Also default"] [:th "Updated"] [:th]]]
      [:tbody
       (if view-project
         (section (str (:name view-project-info) " sets") [:project view-project]
                  "This project has no parameter sets. Members can share sets by making them belong to it.")
         (for [p projects]
           (section (str "Project: " (:name p)) [:project (:id p)] nil)))
       (section "My sets" [:user nil]
                "You have no parameter sets of your own. Make one here, or save one from a network.")
       (section "THERMOS sets" [:system nil] "There are no THERMOS-wide parameter sets.")]]]))

(def ^:private default-reasons
  {:mine-in-project "your choice for this project"
   :project         "this project's default"
   :mine            "your default"
   :system          "the THERMOS default"})

(defn library-page [{:keys [view-project view-project-info default-choice sets] :as data}]
  (page {:title (if view-project
                  (str "Parameter sets for " (:name view-project-info))
                  "Parameter sets")
         :css ["/css/projects.css"]}
        [:div.card
         [:div.flex-cols {:style (style :align-items :center :gap :1em)}
          [:div {:style (style :flex-grow 1)}
           (if view-project
             (let [[set-id reason] default-choice
                   ps (first (filter #(= set-id (:id %)) sets))]
               [:div
                [:a {:href (str "/project/" view-project)} "← Back to " (:name view-project-info)]
                [:div {:style (style :margin-top :0.5em)}
                 "New networks in this project start with "
                 (if ps
                   [:span [:b (:name ps)] ", " (default-reasons reason) "."]
                   "THERMOS's built-in parameters.")
                 " Click a star to change this."]])
             [:span
              "A parameter set holds costs, tariffs, profiles and other parameters, which you can apply to a network, "
              "or use to start new networks; click a star to make a set your default. "
              "Changing a set doesn't change networks it was applied to; they show that the set has changed, "
              "and you can apply it again."])]
          [:a.button {:style (style :white-space :nowrap)
                      :href "#" :hx-get (str "/parameters/new" (when view-project (str "?project=" view-project)))
                      :hx-target "#modal"}
           "NEW SET " symbols/plus]]]
        [:div.card (set-list data)]))

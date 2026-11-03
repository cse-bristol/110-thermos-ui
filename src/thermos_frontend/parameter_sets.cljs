;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.parameter-sets
  "Parameter sets in the network editor: saving the network's parameters
  as a set, applying a set to the network, and saying when a set which
  was applied has changed since (see thermos-specs.parameters and
  thermos-backend.routes.parameter-sets)."
  (:require [goog.net.XhrIo :as xhr]
            [reagent.core :as reagent]
            [re-frame.core :as rf]
            [thermos-specs.document :as document]
            [thermos-specs.parameters :as parameters]
            [thermos-frontend.db :as db]
            [thermos-frontend.doc :as doc]
            [thermos-frontend.inputs :as inputs]
            [thermos-frontend.merge-upload :as merge-upload]
            [thermos-frontend.popover :as popover]
            [thermos-frontend.preload :as preload]
            [thermos-frontend.toaster :as toaster]
            [thermos-frontend.transit :as transit]))

(defn- get-transit
  "GET `url`, and call `handler` with the transit it sends"
  [url handler]
  (xhr/send url
            (fn [e]
              (let [^js target (.-target e)]
                (if (.isSuccess target)
                  (handler (transit/read (.getResponseText target)))
                  (toaster/show! [:div.toaster.toaster--error "Could not get the parameter sets"]))))
            "GET" nil #js {"Accept" transit/content-type}))

(defn- project-id [] (:project-id (db/server (doc/current []))))

(defn- category-checks
  "Checkboxes to choose some of `available` categories, in `*chosen`"
  [available *chosen]
  [:div
   (for [c available]
     [:div {:key c}
      [inputs/check {:label (parameters/category-labels c (name c))
                     :value (contains? @*chosen c)
                     :on-change #(swap! *chosen (if % conj disj) c)}]])])

;; Saving the parameters as a set

(defn- save-set! [name scope categories]
  (let [content (parameters/extract (document/keep-interesting (doc/current [])) categories)
        data (js/FormData.)]
    (.append data "name" name)
    (.append data "scope" scope)
    (.append data "content" (js/Blob. #js [(transit/write content)]
                                      #js {"type" transit/content-type})
             "content.json")
    (xhr/send "/parameters/new"
              (fn [e]
                (let [^js target (.-target e)]
                  (if (.isSuccess target)
                    (let [{:keys [id]} (transit/read (.getResponseText target))]
                      (toaster/show!
                       [:div.toaster.toaster--success
                        "Saved parameter set " [:a {:href (str "/parameters/" id) :target "_blank"} name]]))
                    (toaster/show! [:div.toaster.toaster--error "Could not save the parameter set"]))))
              "POST" data)))

(defn- save-dialog []
  (reagent/with-let [*name (reagent/atom "")
                     *scope (reagent/atom (if (project-id) (str "project:" (project-id)) "user"))
                     *categories (reagent/atom (set parameters/categories))]
    [:div.popover-dialog
     [:h2.popover-header "Save parameters as a set"]
     [:button.popover-close-button {:on-click popover/close!} "⨯"]
     [:div [:label "Name "
            [:input.text-input {:type :text :value @*name
                                :on-change #(reset! *name (.. % -target -value))}]]]
     [:div {:style {:margin-top :0.5em}}
      [:label "Belongs to "
       [inputs/select {:value @*scope
                       :on-change #(reset! *scope %)
                       :values (cond-> [["user" "Me"]]
                                 (project-id) (conj [(str "project:" (project-id)) "This project"]))}]]]
     [:b "Parameters to save:"]
     [category-checks parameters/categories *categories]
     [:div.align-right {:style {:margin-top "1em"}}
      [:button.button.button--danger {:on-click popover/close! :style {:margin-left :auto}} "Cancel"]
      [:button.button
       {:disabled (or (empty? @*categories) (= "" (.trim @*name)))
        :on-click #(do (save-set! (.trim @*name) @*scope @*categories)
                       (popover/close!))}
       "Save"]]]))

(defn show-save-dialog! []
  (popover/open! [save-dialog] :middle))

;; Applying a set

(defn apply-set!
  "Fetch set `id` and show the dialog to apply it, with `categories`
  ticked (or all it has)"
  [id & {:keys [categories]}]
  (get-transit
   (str "/parameters/" id)
   (fn [{:keys [content name revision]}]
     (merge-upload/show-merge-dialog
      content
      :title (str "Apply " name)
      :source {:id id :name name :revision revision}
      :categories (when categories
                    (filter (set categories) (parameters/categories-in content)))))))

(defn- scope-label [{:keys [scope project-name]}]
  (case scope
    :system "THERMOS"
    :user "Mine"
    :project (str "Project " project-name)
    ""))

(defn- apply-dialog [sets]
  [:div.popover-dialog
   [:h2.popover-header "Apply a parameter set"]
   [:button.popover-close-button {:on-click popover/close!} "⨯"]
   (if (empty? sets)
     [:p "There are no parameter sets you can use here. "
      [:a {:href "/parameters" :target "_blank"} "Make one"] ", or save this network's parameters as one."]
     [:table
      [:thead [:tr [:th "Set"] [:th "Belongs to"] [:th "Has"] [:th]]]
      [:tbody
       (for [{:keys [id name categories] :as s} sets]
         [:tr {:key id}
          [:td name]
          [:td (scope-label s)]
          [:td [:small (apply str (interpose ", " (map parameters/category-labels
                                                     (filter categories parameters/categories))))]]
          [:td [:button.button.button--small
                {:on-click #(do (popover/close!) (apply-set! id))}
                "Apply..."]]])]])
   [:div.align-right {:style {:margin-top "1em"}}
    [:button.button.button--danger {:on-click popover/close! :style {:margin-left :auto}} "Cancel"]]])

(defn show-apply-dialog! []
  (get-transit
   (str "/parameters.json" (when-let [p (project-id)] (str "?project=" p)))
   #(popover/open! [apply-dialog %] :middle)))

;; Telling the user when a set they applied has changed

(defn- fetch-revisions! [ids *revisions]
  (when (seq ids)
    (xhr/send (str "/parameters/revisions?ids=" (apply str (interpose "," ids)))
              (fn [e]
                (let [^js target (.-target e)]
                  (when (.isSuccess target)
                    (reset! *revisions (transit/read (.getResponseText target))))))
              "GET" nil #js {"Accept" transit/content-type})))

(defn changed-banner
  "A banner for each parameter set applied to the network which has been
  changed since, unless the user has dismissed that change. The
  revisions are asked for once, when the editor opens."
  []
  (reagent/with-let [*revisions (reagent/atom {})
                     _ (fetch-revisions! (keys (doc/current [::document/parameter-sources])) *revisions)]
    (let [sources (doc/value [::document/parameter-sources])
          changed (parameters/changed-sources {::document/parameter-sources sources} @*revisions)]
      (when (seq changed)
        [:div {:style {:background "#fff3cd" :border-bottom "1px solid #e0c080"
                       :padding "0.25em 1em"}}
         (for [{:keys [id name categories current-revision]} changed]
           [:div.flex-cols {:key id :style {:align-items :center :gap :1em}}
            [:span {:style {:flex-grow 1}}
             "The parameter set " [:b name] " has changed since it was applied to this network."]
            [:button.button.button--small
             {:on-click #(apply-set! id :categories categories)}
             "Apply again..."]
            [:button.button.button--small
             {:on-click #(doc/set-value! [::document/parameter-sources id :dismissed-revision]
                                         current-revision)}
             "Dismiss"]])]))))

;; The parameter set editor

(defn set-editor-bar
  "In the parameter set editor, which set this is, and the categories of
  parameters it has, which the user can change. Only those categories
  are saved."
  []
  (let [{:keys [scope project-name]} (db/parameter-set)
        chosen (set (doc/value [::parameters/categories]))
        read-only (preload/get-value :read-only)]
    [:div.parameter-set-bar {:style {:background "#eef4ff" :border-bottom "1px solid #b8c8e8"
                   :padding "0.25em 1em"}}
     [:div.flex-cols {:style {:align-items :center :gap :1em :flex-wrap :wrap}}
      [:b (case scope
            :system "A THERMOS parameter set"
            :user "One of your parameter sets"
            :project (str "A parameter set for " project-name)
            "A parameter set")]
      (if read-only
        [:span "You can't change it, but you can copy it from the "
         [:a {:href "/parameters"} "parameter sets page"] "."]
        [:span "It has these parameters; the rest are left alone when it is applied to a network:"])]
     [:div.flex-cols {:style {:gap :1em :flex-wrap :wrap}}
      (for [c parameters/categories]
        [:span {:key c}
         [inputs/check {:label (parameters/category-labels c (name c))
                        :value (contains? chosen c)
                        :disabled read-only
                        :on-change #(doc/set-value! [::parameters/categories]
                                                    ((if % conj disj) chosen c))}]])]]))

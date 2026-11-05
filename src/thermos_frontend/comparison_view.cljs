;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.comparison-view
  "The editor's comparison mode, in which the map is coloured by how the
  network differs from another saved network (an earlier version of it,
  or another network on the map), and the panel beside it shows the
  differences (see thermos-frontend.comparison). The comparison is
  editor-only state (see db/comparison), so it is never saved; the
  network can be edited and run while comparing, and the differences
  follow."
  (:require [re-frame.core :as rf]
            [clojure.string :as string]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.document :as document]
            [thermos-specs.view :as view]
            [thermos-frontend.comparison :as comparison]
            [thermos-frontend.doc :as doc]
            [thermos-frontend.format :as format]
            [thermos-frontend.popover :as popover]
            [thermos-frontend.preload :as preload]
            [thermos-frontend.theme :as theme]))

(defn- date [d]
  (when (instance? js/Date d)
    (.toLocaleString d js/undefined #js {:dateStyle "medium" :timeStyle "short"})))

(defn- network-label
  "A network from the list, as its name and version"
  [{:keys [id name]}]
  [:<> [:b name] " " [:small "v" id]])

;; Choosing what to compare with

(defn- network-rows [networks current-id]
  (for [{:keys [id has-run author created meta] :as network} networks]
    [:tr {:key id}
     [:td [network-label network]
      (when (= id current-id) [:small " (as last saved)"])]
     [:td (date created)]
     [:td author]
     [:td (if has-run "✓" "")]
     [:td.numeric (format/si-number (:objective-value meta))]
     [:td.numeric (some-> (:network-length meta) (format/si-number) (str "m"))]
     [:td.numeric (:network-count meta)]
     [:td [:button.button.button--small
           {:on-click #(do (popover/close!)
                           (rf/dispatch [:compare/with network]))}
           "Compare"]]]))

(defn- chooser []
  (let [networks   @(rf/subscribe [:compare/networks])
        current-id (:network-id @(rf/subscribe [:server]))
        name       (preload/get-value :name)
        {versions true others false}
        (when (sequential? networks) (group-by #(= name (:name %)) networks))
        table      (fn [title networks]
                     (when (seq networks)
                       [:<>
                        [:h3 {:style {:margin-top :1em}} title]
                        [:table.table.table--hover
                         [:thead
                          [:tr [:th "Network"] [:th "Saved"] [:th "By"] [:th "Run"]
                           [:th.numeric "Objective"] [:th.numeric "Length"]
                           [:th.numeric "Connected"] [:th]]]
                         [:tbody (network-rows networks current-id)]]]))]
    [:div.popover-dialog.popover-dialog--wide
     [:h2.popover-header "Compare with another network"]
     [:button.popover-close-button {:on-click popover/close!} "⨯"]
     [:p "The map will show where this network differs from the one you choose, "
      "and the panel beside it how the results and parameters differ. "
      "You can carry on editing while you compare."]
     (cond
       (nil? networks)      [:p "Fetching the networks on this map..."]
       (= :failed networks) [:p "Could not fetch the networks on this map."]
       (empty? networks)    [:p "There are no saved networks on this map yet."]
       :else
       [:<>
        [table "Versions of this network" versions]
        [table "Other networks on this map" others]])
     [:div.align-right {:style {:margin-top "1em"}}
      [:button.button.button--danger {:on-click popover/close!} "Cancel"]]]))

(defn show-chooser! []
  (rf/dispatch [:compare/fetch-networks])
  (popover/open! [chooser] :middle))

;; Which way round things are

(def category-labels
  {:added     "Added"
   :removed   "Removed"
   :changed   "Changed"
   :same      "Same, on both networks"
   :unchanged "Same, on neither network"})

(def ^:private category-explanations
  {:added     "On this network but not the other, or only in this problem"
   :removed   "On the other network but not this one, or only in its problem"
   :changed   "Its constraint, heating system, pipe size or other inputs differ"
   :same      "On both networks, and the same in both"
   :unchanged "On neither network, and the same in both"})

;; The map's legend and the bar under it

(defn- legend []
  (let [counts @(rf/subscribe [:compare/category-counts])]
    [:div.map-legend
     [:dl
      (for [c [:added :removed :changed :same]
            :let [b (get counts [:building c] 0)
                  p (get counts [:path c] 0)]]
        [:div {:key c :title (category-explanations c)}
         [:dt.key {:class (str "compare-" (name c) "-key")}]
         [:dd (category-labels c) " "
          [:small "(" b " building" (when (not= 1 b) "s")
           ", " p " path" (when (not= 1 p) "s") ")"]]])
      [:div {:title (category-explanations :unchanged)}
       [:dt.key.forbidden-key] [:dd (category-labels :unchanged)]]]]))

(defn view-control
  "The bar under the map while comparing, instead of the choice of
  constraints or solution"
  []
  [:div.view-control
   [:h2.view-control__label "Map view:"]
   [:span "Comparing with " [network-label @(rf/subscribe [:compare/network])]]
   [:button.view-control__button
    {:style {:margin-left :1em}
     :on-click #(rf/dispatch [:compare/stop])}
    "Stop comparing"]
   [legend]])

;; The banner, on every page

(defn banner
  "Says what is being compared, if anything, with the way out"
  []
  (let [{:keys [loading failed network]} @(rf/subscribe [:compare/state])
        comparing @(rf/subscribe [:compare/comparing])
        tab-path [::view/view-state ::view/selected-tab]
        on-map (= :candidates (doc/value tab-path :candidates))]
    (when (or loading failed comparing)
      [:div.flex-cols {:style {:background "#eef4ff" :border-bottom "1px solid #b8c8e8"
                               :padding "0.25em 1em" :align-items :center :gap :1em}}
       [:span {:style {:flex-grow 1}}
        (cond
          loading [:<> "Fetching " [network-label loading] " to compare with..."]
          failed  [:<> "Could not fetch " [network-label failed] " to compare with."]
          :else   [:<> "Comparing this network with " [network-label network]
                   (when-let [d (date (:created network))] (str ", saved " d))
                   ". The map shows the differences."])]
       (when failed
         [:button.button.button--small {:on-click #(rf/dispatch [:compare/dismiss-failure])}
          "Dismiss"])
       (when (and comparing (not on-map))
         [:button.button.button--small {:on-click #(doc/set-value! tab-path :candidates)}
          "Show map"])
       (when comparing
         [:button.button.button--small {:on-click show-chooser!} "Compare with another..."])
       (when (or comparing loading)
         [:button.button.button--small {:on-click #(rf/dispatch [:compare/stop])}
          "Stop comparing"])])))

;; The panel beside the map

(defn- value [v {:keys [unit scale]}]
  (cond
    (number? v)  (let [n (format/si-number (* v (or scale 1)))]
                   ;; si-number ends with a space or a prefix, except for 0
                   (cond (nil? unit)                n
                         (string/ends-with? n " ") (str n unit)
                         (re-find #"\d$" n)        (str n " " unit)
                         :else                     (str n unit)))
    (nil? v)     "--"
    (keyword? v) (name v)
    (boolean? v) (if v "yes" "no")
    :else        (str v)))

(defn- delta [d figure]
  (when (and d (not (zero? d)))
    [:span {:style {:color theme/dark-grey}}
     (if (pos? d) "+" "−")
     (value (Math/abs d) figure)]))

(defn- swatch [category]
  [:span {:style {:display :inline-block :width :0.8em :height :0.8em
                  :margin-right :0.4em :vertical-align :middle
                  :border (str "2px solid " (theme/compare-colours category theme/light-grey))}}])

(def ^:private max-shown
  "How many candidates the panel shows the differences of"
  10)

(defn- candidate-card [id diff here there mode]
  (let [a (get here id)
        b (get there id)
        {:keys [category type presence]} (get diff id {:category :unchanged
                                                       :type (::candidate/type (or a b))})
        rows (comparison/candidate-rows a b mode)]
    [:div {:style {:padding "0.5em 1em" :border-bottom "1px solid #ccc"}}
     [:div {:title (category-explanations category)}
      [swatch category]
      [:b (case type :building "Building" :path "Path" "Candidate")]
      " " [:small id] " — " (category-labels category)
      (case presence
        :here  [:small " (not in the other network)"]
        :there [:small " (deleted from this network)"]
        nil)]
     (when (seq rows)
       [:table.table {:style {:margin-top :0.25em}}
        [:thead [:tr [:th] [:th.numeric "This"] [:th.numeric "Other"]]]
        [:tbody
         (for [{:keys [label here there] :as row} rows]
           [:tr {:key label :style (when (not= here there) {:font-weight :bold})}
            [:td label]
            [:td.numeric (value here row)]
            [:td.numeric (value there row)]])]])]))

(defn- picked-candidates
  "The before and after of the selection, and of what was clicked on in
  the other network"
  []
  (let [selected @(rf/subscribe [:candidates/selected-ids])
        picked   @(rf/subscribe [:compare/picked])
        ids      (vec (distinct (concat (sort selected) (sort picked))))
        diff     @(rf/subscribe [:compare/diff])
        here     @(rf/subscribe [:doc/candidates])
        there    @(rf/subscribe [:compare/candidates])
        deleted  (set (doc/value [::document/deletions]))
        here     (fn [id] (if (deleted id) :deleted (get here id)))
        mode     @(rf/subscribe [:doc/mode])]
    [:section
     [:h3 {:style {:padding "0.5em 1em 0 1em"}} "Selected"]
     (if (empty? ids)
       [:p {:style {:padding "0 1em"}}
        "Click on the map to see how something differs between the networks."]
       [:<>
        (for [id (take max-shown ids)]
          ^{:key id} [candidate-card id diff {id (here id)} there mode])
        (when (> (count ids) max-shown)
          [:p {:style {:padding "0 1em"}}
           "And " (- (count ids) max-shown) " more. Select fewer to see them."])])]))

(defn- summary-table []
  (let [rows @(rf/subscribe [:compare/summary-rows])]
    [:section {:style {:padding "0.5em 1em"}}
     [:h3 "Summary"]
     [:table.table.table--hover
      [:thead [:tr [:th] [:th.numeric "This"] [:th.numeric "Other"] [:th.numeric "Change"]]]
      [:tbody
       (for [{k :key :keys [label here there] d :delta :as row} rows
             :when (or (some? here) (some? there))]
         [:tr {:key k}
          [:th label]
          [:td.numeric (value here row)]
          [:td.numeric (value there row)]
          [:td.numeric [delta d row]]])]]]))

(defn- parameters-table []
  (let [rows @(rf/subscribe [:compare/parameters])]
    [:section {:style {:padding "0.5em 1em"}}
     [:h3 "Parameters"]
     (if (empty? rows)
       [:p "The parameters are the same."]
       [:table.table
        [:thead [:tr [:th] [:th "This"] [:th "Other"]]]
        [:tbody
         (for [{k :key :keys [label here there detail]} rows]
           [:tr {:key k}
            [:th label]
            (if detail
              [:td {:col-span 2} detail]
              [:<> [:td (value here {})] [:td (value there {})]])])]])]))

(defn panel
  "Beside the map while comparing, instead of the selection info"
  []
  [:div.component--selection-info
   [:header.selection-header "Comparison"]
   [:div.selection-table
    [picked-candidates]
    [summary-table]
    [parameters-table]]])

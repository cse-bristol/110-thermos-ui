;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.page-nav
  "The bar of tabs under the nav bar, which switches between the
  editor's pages"
  (:require [re-frame.core :as rf]
            [thermos-specs.document :as document]
            [thermos-specs.view :as view]
            [thermos-frontend.db :as db]
            [thermos-frontend.doc :as doc]))

(def ^:private tab-path [::view/view-state ::view/selected-tab])

(defn- groups
  "The pages to show, as [[group-label [[tab label] ...]] ...]. A parameter
  set (`set-mode`) has no map or solution."
  [set-mode has-solution has-supply-solution]
  (->> [[nil (when-not set-mode [[:candidates "Map"]])]
        ["Network" [[:parameters "Objective"]
                    [:tariffs "Tariffs"]
                    [:pipe-costs "Pipes & connections"]
                    [:insulation "Insulation"]
                    [:alternatives "Individual systems"]
                    [:supply-types "Supply types"]]]
        ["Supply" [[:profiles "Profiles"]
                   [:supply-technologies "Technologies"]
                   [:supply-objective "Objective"]]]
        ["Results" (when-not set-mode
                     (concat
                      (when has-solution
                        [[:solution "Network solution"]
                         [:run-log "Run log"]])
                      (when has-supply-solution
                        [[:supply-solution "Supply solution"]])))]]
       (filter (comp seq second))))

(defn- tabs-in [document]
  (->> (groups (some? (db/parameter-set))
               (document/has-solution? document)
               (document/has-supply-solution? document))
       (mapcat second)
       (map first)
       (vec)))

(rf/reg-event-db
 ::step
 ;; show the page `delta` pages along from this one, wrapping round
 (fn [db [_ delta]]
   (let [tabs (tabs-in db)
         here (.indexOf tabs (get-in db tab-path :candidates))
         ;; when on a page not in the bar, as for a parameter set's map,
         ;; take it as before the first
         here (if (neg? here) (if (pos? delta) -1 0) here)]
     (view/switch-to-tab
      db (nth tabs (mod (+ here delta) (count tabs)))))))

(defn component
  "The bar of tabs, with `selected` the page being shown"
  [selected]
  (let [set-mode (some? (db/parameter-set))]
    [:nav.page-nav
     (for [[group tabs] (groups set-mode
                                @(rf/subscribe [:doc/has-solution])
                                @(rf/subscribe [:doc/has-supply-solution]))]
       [:div.page-nav__group {:key (or group :none)}
        (when group [:span.page-nav__group-label group])
        (for [[tab label] tabs]
          [:button.page-nav__tab
           {:key tab
            :class (when (= tab selected) "page-nav__tab--selected")
            :on-click #(doc/set-value! tab-path tab)}
           label])])]))

(ns thermos-ui.frontend.search-box
    (:require
      [reagent.core :as reagent]
      [goog.net.XhrIo :as xhr]
      [clojure.string :as s]
      ))

(declare component result-item perform-search fly-to-result map-to-query-string)

(defn component
  "The search box component"
  [leaflet-map]
  (reagent/with-let [results (reagent/atom [])
                     active-result (reagent/atom nil)
                     active (reagent/atom false)
                     input-value (reagent/atom "")
                     timeout (reagent/atom nil)]
    [:div {:class (str "map-search__container"
                         (if @active " map-search__container--active")
                         (if (not-empty @results) " map-search__container--has-results"))
             ;; This is not ideal, but it works - we need to set the z index of this element's parent
             ;; so that the results list doesn't disappear behind the layers control.
             :ref (fn [element] (if element (set! (.. element -parentNode -style -zIndex) 801)))}
       [:input#map-search-input.map-search__input
        {:type "text"
         :value @input-value
         :placeholder "Search..."

         ;; this horrible hack appears to be needed
         ;; because of react's weird event stuff
         :ref #(when %
                 (.addEventListener % "keypress"
                                    (fn [e] (.stopPropagation e))
                                    false))
         
         :on-change #(reset! input-value (.. % -target -value))
         :on-key-up (fn [e]
                      (js/clearTimeout @timeout)
                      
                      (case (.-key e)
                        "ArrowUp"
                        (when-not (empty? @results)
                          (.preventDefault e)
                          
                          (cond
                            (= @active-result 0) (reset! active-result nil)
                            (> @active-result 0) (swap! active-result dec)
                            (nil? @active-result) (reset! active-result (- (count @results) 1))))
                        
                        "ArrowDown"
                        (when-not (empty? @results)
                          (.preventDefault e)
                          (cond
                            (nil? @active-result) (reset! active-result 0)
                            (= @active-result (- (count @results) 1)) (reset! active-result nil)
                            (>= @active-result 0) (swap! active-result inc)))

                        "Enter" ;; If you press enter while on a result, go there!
                        (when (some? @active-result)
                          (let [result (nth @results @active-result)]
                            (fly-to-result leaflet-map result)
                            (.blur e.target)
                            (set! (.. e -target -value) "")
                            (reset! results [])))

                        "Escape"
                        (do (.blur e.target)
                            (set! (.. e -target -value) "")
                            (reset! results []))

                        ;; default case it's a normal key
                        (reset! timeout (js/setTimeout #(perform-search @input-value results) 250))))
         
         :on-focus (fn [e] (reset! active true))
         :on-blur (fn [e] (reset! active false))}
        ]
       [:i.map-search__icon]

       ;; The list of results
       [:ul.map-search__results-list
        (if (not-empty @results)
          (doall (map-indexed
                  (fn [index result]
                    (let [is-active (= index @active-result)]
                      (result-item leaflet-map result is-active index results)))
                  @results)))]
     ]))

(defn result-item
  "Child component for a single result in the list."
  [leaflet-map result is-active index results]
  [:li
   {:class (str "map-search__results-list-item" (if is-active " map-search__results-list-item--active"))
    :key (:place_id result)
    :title (:display_name result)
    :on-click (fn [e] (fly-to-result leaflet-map result)
                (set! (.-value (js/document.getElementById "map-search-input")) "")
                (reset! results []))
    }
   (:display_name result)])

(defn perform-search
  "Make request to Nominatim to search locations.
  `search-string` is the string you are searching for.
  `results` is the results atom which will be updated with the results that are returned."
  [search-string results]
  (let [;; For now, set the viewbox to be a box roughly covering London
        viewbox "-0.3862380981445313,51.63868928488592,0.12359619140625001,51.40241847742742"]
    (xhr/send (str "https://nominatim.openstreetmap.org/search"
                   (map-to-query-string {:q search-string
                                              :limit 5
                                              :format "json"
                                              :addressdetails 1
                                         ;; :countrycodes "gb"
                                         ;; :viewbox viewbox
                                         }))
              (fn [response]
                (let [request-results (.getResponseJson response.target)]
                  (reset! results (js->clj request-results :keywordize-keys true))))
              )))

(defn fly-to-result
  "Pan to the selected result, with the bounding box based on the size of the location."
  [leaflet-map result]
  (let [latlng-bounds (clj->js [[(nth (:boundingbox result) 0) (nth (:boundingbox result) 2)]
                                [(nth (:boundingbox result) 1) (nth (:boundingbox result) 3)]])]
    (.flyToBounds leaflet-map latlng-bounds #js {:duration 0.5})
    ))

(defn map-to-query-string
  "Turn a map of query parameters and values into a query string which can be used in a GET request"
  [map]
  (reduce-kv
   (fn [init k v]
     (if (empty? init)
       (str "?" (name k) "=" v)
       (str init "&" (name k) "=" v)))
   ""
   map))

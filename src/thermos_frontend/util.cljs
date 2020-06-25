(ns thermos-frontend.util)

(defn target-value [e]
  (.. e -target -value))

(defn focus-next []
  (let [candidates (->> (js/document.querySelectorAll
                         "a:not([disabled]), button:not([disabled]), input[type=text]:not([disabled]), [tabindex]:not([disabled]):not([tabindex=\"-1\"])")
                        (js/Array.prototype.slice.call)
                        (filter
                         (fn [el]
                           (or (pos? (.-offsetWidth el))
                               (pos? (.-offsetHeight el))
                               (= js/document.activeElement el)))))

        target (->> (concat candidates [(first candidates)])
                    (drop-while #(not= js/document.activeElement %))
                    (second))
        ]
    (when target
      (.focus target)
      (.select target))))



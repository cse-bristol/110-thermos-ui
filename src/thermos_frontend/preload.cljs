(ns thermos-frontend.preload
  "In thermos-backend.pages.common there's code to stick js variables into
  scope at the top of the page.

  This is the other half of that, providing access to them. The
  purpose is to pass some info into a cljs page without having to load
  it and then fire off an xmlhttprequest.")

(def preloaded-values
  (atom (js->clj (aget js/window "thermos_preloads") :keywordize-keys true)))

(defn get-value [key & {:keys [clear]}]
  (let [out (get @preloaded-values key)]
    (when clear (swap! preloaded-values dissoc key))
    out))

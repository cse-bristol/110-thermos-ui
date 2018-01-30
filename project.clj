(defn deep-merge [v & vs]
  (letfn [(rec-merge [v1 v2]
                     (if (and (map? v1) (map? v2))
                       (merge-with deep-merge v1 v2)
                       v2))]
    (when (some identity vs)
      (reduce #(rec-merge %1 %2) v vs))))

(defproject thermos-ui "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.9.0-beta4"]
                 [org.clojure/clojurescript "1.9.946"]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/core.async  "0.3.443"]

                 [reagent "0.7.0"]
                 [compojure "1.6.0"]
                 [ring/ring-defaults "0.3.1"]
                 [ring/ring-json "0.4.0"]

                 [org.clojure/java.jdbc "0.7.5"]
                 [org.postgresql/postgresql "9.4.1212.jre7"]

                 [environ "1.1.0"]]

  :plugins [[lein-figwheel "0.5.14"]
            [lein-cljsbuild "1.1.7" :exclusions [[org.clojure/clojure]]]
            [lein-ring "0.12.3"]
            [lein-environ "1.1.0"]]

  :source-paths ["src"]

  :cljsbuild
  {:builds
   ~(let [common '{:source-paths ["src"]
                   :compiler {:npm-deps {:rbush "2.0.1"
                                         :jsts "1.4.0"}
                              :install-deps true
                              :asset-path "js/compiled/out"
                              :infer-externs true
                              }
                   }]
      (into []
            (map #(deep-merge common %)
                 '[{:id "dev"
                    :figwheel {:on-jsload "thermos-ui.core/on-js-reload"
                               :open-urls ["http://localhost:3449/index.html"]}

                    :compiler {:main thermos-ui.core
                               :asset-path "js/compiled/out"
                               :output-to "resources/public/js/compiled/thermos_ui.js"
                               :output-dir "resources/public/js/compiled/out"
                               :source-map-timestamp true
                               :preloads [devtools.preload]}}

                   {:id "min"
                    :compiler {:output-to "resources/public/js/compiled/thermos_ui.js"
                               :main thermos-ui.core
                               :optimizations :advanced
                               :pretty-print false}}])))
   }



  :figwheel {:ring-handler thermos-ui.handler/app
             :css-dirs ["resources/public/css"] ;; watch and update CSS
             }


  ;; Setting up nREPL for Figwheel and ClojureScript dev
  ;; Please see:
  ;; https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl
  :ring {:handler thermos-ui.handler/app}
  :profiles {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                                  [ring/ring-mock "0.3.0"]
                                  [binaryage/devtools "0.9.4"]
                                  [figwheel-sidecar "0.5.14"]
                                  [com.cemerick/piggieback "0.2.2"]]
                   ;; need to add dev source path here to get user.clj loaded
                   :source-paths ["src" "dev"]
                   ;; for CIDER
                   ;; :plugins [[cider/cider-nrepl "0.12.0"]]
                   :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
                   ;; need to add the compliled assets to the :clean-targets
                   :clean-targets ^{:protect false} ["resources/public/js/compiled"
                                                     :target-path]
                   :env {:problem-store "test-resources/data/problems"
                         :pg-host "172.21.0.3"
                         :pg-user "postgres"
                         :pg-password "therm0s"
                         :pg-db-geometries "thermos_geometries"}}})

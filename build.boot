(set-env!
 :source-paths #{"src"}
 :resource-paths #{"resources"}
 :dependencies
 '[[org.clojure/clojure "1.9.0-beta4" :scope "provided"]
   [org.clojure/clojurescript "1.9.946" :scope "test"]
   [org.clojure/test.check "0.9.0" :scope "test"]
   [org.clojure/core.async  "0.3.443"]

   [cljsjs/leaflet "1.2.0-0"]
   [cljsjs/jsts  "1.6.0-0"]
   [cljsjs/rbush "2.0.1-0"]
   [reagent      "0.7.0"]
   [secretary    "1.2.3"]
   [venantius/accountant "0.2.3" :exclusions [org.clojure/tools.reader]]

   ;; Server-side dependencies
   [compojure                 "1.6.0"]
   [environ                   "1.1.0"]
   [ring/ring-defaults        "0.3.1"]
   [ring/ring-json            "0.4.0"]
   [org.clojure/java.jdbc     "0.7.5"]
   [org.postgresql/postgresql "9.4.1212.jre7"]
   [hiccup                    "1.0.5"]


   ;; Build tooling dependencies:

   [adzerk/boot-cljs              "LATEST"  :scope "test"]
   [powerlaces/boot-figreload     "LATEST"  :scope "test"]
   [pandeiro/boot-http            "0.7.6"   :scope "test"]

   [adzerk/boot-cljs-repl         "0.3.3"   :scope "test"]
   [com.cemerick/piggieback       "0.2.1"   :scope "test"]
   [weasel                        "0.7.0"   :scope "test"]
   [org.clojure/tools.nrepl       "0.2.13"  :scope "test"]

   [binaryage/dirac               "RELEASE" :scope "test"]
   [binaryage/devtools            "RELEASE" :scope "test"]
   [powerlaces/boot-cljs-devtools "0.2.0"   :scope "test"]
   [boot-environ                  "1.1.0"   :scope "test"]
   [deraen/boot-less              "0.6.1"   :scope "test"]
   ])

(require '[adzerk.boot-cljs              :refer [cljs]]
         '[adzerk.boot-cljs-repl         :refer [cljs-repl]]
         '[powerlaces.boot-figreload     :refer [reload]]
         '[powerlaces.boot-cljs-devtools :refer [dirac cljs-devtools]]
         '[pandeiro.boot-http            :refer [serve]]
         '[environ.boot                  :refer [environ]]
         '[deraen.boot-less              :refer [less]])


(deftask dev []
  (comp
   (serve :handler 'thermos-ui.backend.handler/app
          :port 8080
          :reload true
          :httpkit true)
   (environ :env {:problem-store "test-resources/data/problems"
                  :disable-cache "1"
                  :pg-host "172.21.0.3"
                  :pg-user "postgres"
                  :pg-password "therm0s"
                  :pg-db-geometries "thermos_geometries"})
   (watch)
   (cljs-devtools)
   (reload)
   (dirac)
   (cljs :source-map true
         :optimizations :none
         :compiler-options
         {:parallel-build true
          :preloads ['devtools.preload]
          :external-config
          {:devtools/config {:features-to-install [:formatters :hints :async]
                             :fn-symbol "λ"
                             :print-config-overrides true}}
          }


         )
   (less :source-map true)
   (target)
   ))

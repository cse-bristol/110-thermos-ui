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
   [cljsjs/react-virtualized "9.11.1-1" :exclusions [cljsjs/react cljsjs/react-dom]]
   [cljsjs/leaflet-draw "0.4.12-0"]

   [reagent      "0.7.0"]
   [secretary    "1.2.3"]
   [venantius/accountant "0.2.3" :exclusions [org.clojure/tools.reader]]

   ;; Server-side dependencies
   [compojure                 "1.6.0"]
   [ring/ring-defaults        "0.3.1"]
   [ring/ring-json            "0.4.0"]
   [org.clojure/java.jdbc     "0.7.5"]
   [org.postgresql/postgresql "9.4.1212.jre7"]
   [hiccup                    "1.0.5"]
   [digest                    "1.4.6"]
   ;; the actual server:
   [http-kit                  "2.2.0"]

   [javax.servlet/servlet-api "2.5" :scope "test"]
   [ring/ring-mock "0.3.0"  :scope "test"]

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
   [deraen/boot-less              "0.6.1"   :scope "test"]
   [adzerk/boot-test "1.2.0" :scope "test"]
   ])

(require '[adzerk.boot-cljs              :refer [cljs]]
         '[adzerk.boot-cljs-repl         :refer [cljs-repl]]
         '[powerlaces.boot-figreload     :refer [reload]]
         '[powerlaces.boot-cljs-devtools :refer [dirac cljs-devtools]]
         '[pandeiro.boot-http            :refer [serve]]
         '[deraen.boot-less              :refer [less]]
         '[adzerk.boot-test :refer :all] )

(deftask testing
  "Profile setup for running tests."
  []
  (set-env! :source-paths #(conj % "test")
            :resource-paths #(conj % "test-resources"))
  identity)

(task-options!
 pom {:project 'thermos
      :version "0.1.0-SNAPSHOT"}
 aot {:namespace #{'thermos-ui.backend.main}}
 jar {:main 'thermos-ui.backend.main})

(deftask dev []
  (comp
   (serve :handler 'thermos-ui.backend.handler/app
          :port 8080
          :reload true
          :httpkit true)
   (watch)
   (less :source-map true)
   (cljs-devtools)
   (reload)
   (cljs :source-map true
         :optimizations :none
         :compiler-options
         {:parallel-build true
          :preloads ['devtools.preload]
          :external-config
          {:devtools/config {:features-to-install [:formatters :hints :async]
                             :fn-symbol "λ"
                             :print-config-overrides true}}
          })))

(deftask package
  "Create a runnable jar containing all the thermos stuff."
  []
  (comp
   (less :compression true)
   (cljs :compiler-options {:preloads nil
                            :infer-externs true
                            ;; the next two settings help when
                            ;; debugging this stuff with advanced
                            ;; optimisations. they make the compiler
                            ;; output renamed but readable symbols,
                            ;; and format the code. Process is
                            ;; something like:
                            ;; 1. Compile with advanced, everything breaks
                            ;; 2. Turn these things on and recompile, reload
                            ;; 3. Find where it broke and add an extern to e.g. leaflet-extra.js
                            ;; 4. Recompile, round and round we go.
                            :pretty-print false
                            :pseudo-names false
                            :externs ["externs/leaflet-extra.js"]
                            }
         :optimizations :advanced
         )
   (aot)
   (pom)
   (uber)
   (jar :file "thermos.jar")
   (sift :include #{#".*\.jar"})
   (target)))

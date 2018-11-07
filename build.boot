(set-env!
 :source-paths #{"src"}
 :resource-paths #{"resources"}
 :dependencies
 '[[org.clojure/clojure       "1.9.0-beta4" :scope "provided"]
   [org.clojure/clojurescript "1.9.946" :scope "test"]
   [org.clojure/test.check    "0.9.0" :scope "test"]
   [org.clojure/core.async    "0.3.443"]

   [cljsjs/leaflet "1.2.0-0"]
   [cljsjs/jsts    "1.6.0-0"]
   [cljsjs/rbush   "2.0.1-0"]
   [cljsjs/react-virtualized "9.11.1-1" :exclusions [cljsjs/react cljsjs/react-dom]]
   [cljsjs/leaflet-draw "0.4.12-0"]

   [reagent      "0.7.0"]
   [re-com       "0.9.0"]

   ;; Server-side dependencies
   [compojure                 "1.6.1"]
   [ring/ring-core            "1.6.3"]
   [ring/ring-defaults        "0.3.1"]
   [ring/ring-json            "0.4.0"]

   [org.clojure/tools.logging "0.4.1"]
   [log4j/log4j "1.2.17"
    :exclusions [javax.mail/mail
                 javax.jms/jms
                 com.sun.jmdk/jmxtools
                 com.sun.jmx/jmxri]]

   ;; we need this because we have hikari thing which uses slf4j,
   ;; which then does nothing if we don't have a logging doodad
   [org.slf4j/slf4j-log4j12   "1.7.10"]

   [com.stuartsierra/component "0.3.2"]
   
   [org.clojure/java.jdbc     "0.7.5"]
   [org.postgresql/postgresql "9.4.1212.jre7"]
   [hiccup                    "1.0.5"] ;; for HTML templating
   [digest                    "1.4.6"] ;; for MD5 convenience
   [honeysql                  "0.9.2"] ;; for SQL generation
   [nilenso/honeysql-postgres "0.2.3"] ;; postgres specific bits
   [ragtime                   "0.7.2"] ;; for DB migrations
   [org.clojure/data.json     "0.2.6"] ;; for parsing the geojson from the db.
   ;; the actual server:
   [http-kit                  "2.2.0"]
   [ring-logger               "1.0.0"]
   
   [funcool/clojure.jdbc      "0.9.0"] ;; sensible db access
   [hikari-cp                 "1.2.4"] ;; connection pooling


   [org.clojure/data.csv      "0.1.4"]
   [aysylu/loom               "1.0.1"] ;; Graph data structures / algo
   [org.tobereplaced/nio.file "0.4.0"] ;; nio functions
   
   [javax.servlet/servlet-api "2.5"    :scope "test"]
   [ring/ring-mock            "0.3.0"  :scope "test"]

   ;; Build tooling dependencies:

   [adzerk/boot-cljs              "2.1.4"  :scope "test"]
   [powerlaces/boot-figreload     "0.5.14"  :scope "test"]
   [pandeiro/boot-http            "0.7.6"   :scope "test"]

   [adzerk/boot-cljs-repl         "0.4.0-SNAPSHOT"   :scope "test"]
   [cider/piggieback              "0.3.8"   :scope "test"]
   [weasel                        "0.7.0"   :scope "test"]
   [org.clojure/tools.nrepl       "0.2.13"  :scope "test"]
   [com.cemerick/piggieback       "0.2.2"   :scope "test"] ;; this is bad
   
   [binaryage/dirac               "RELEASE" :scope "test"]
   [binaryage/devtools            "RELEASE" :scope "test"]
   ;; [powerlaces/boot-cljs-devtools "0.2.0"   :scope "test"]
   [deraen/boot-less              "0.6.1"   :scope "test"]
   [adzerk/boot-test              "1.2.0"   :scope "test"]

   [org.danielsz/system "0.4.1"]
   ])

(require '[adzerk.boot-cljs              :refer [cljs]]
         '[adzerk.boot-cljs-repl         :refer [cljs-repl-env start-repl cljs-repl]]
         '[powerlaces.boot-figreload     :refer [reload]]
         ;; '[powerlaces.boot-cljs-devtools :refer [dirac cljs-devtools]]
         '[pandeiro.boot-http            :refer [serve]]
         '[deraen.boot-less              :refer [less]]
         '[adzerk.boot-test :refer :all]

         '[system.boot :refer [system]]
         )

(require 'clojure.java.shell)
(require 'boot.filesystem)
(in-ns 'boot.filesystem)
(let [mkvisitor-0 mkvisitor]
  (defn mkvisitor [^Path root tree & {:keys [ignore]}]
    (let [^SimpleFileVisitor v0 (mkvisitor-0 root tree :ignore ignore)]
      (proxy [SimpleFileVisitor] []
        (preVisitDirectory [path attr] (.preVisitDirectory v0 path attr))
        (visitFile [path attr] (.visitFile v0 path attr))
        (visitFileFailed [path exc]
          (clojure.java.shell/sh "notify-send" "ignoring a lockfile error"
                                 (.getMessage exc))
          FileVisitResult/CONTINUE)))))
(in-ns 'boot.user)

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


(require '[thermos-ui.backend.main :refer [create-system]])

(deftask dev
  "Run in development mode - this does live reload, and runs a repl.
   You can connect to the repl from e.g. emacs or by running boot repl -c"
  []
  (comp
   (watch :verbose true
          :debounce 50)
   (less :source-map true)
   ;; (cljs-devtools)
   (reload)
   (cljs-repl)

   (cljs :source-map true
         :optimizations :none
         :compiler-options
         {:parallel-build true
          :preloads ['devtools.preload]

          :external-config
          {:devtools/config {:features-to-install [:formatters :hints :async]
                             :fn-symbol "λ"
                             :print-config-overrides true}}
          })

   (system :sys #'create-system
           :auto true
           :files ["pages.clj" "handler.clj"])
   ))

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
         :optimizations :advanced)
   (aot)
   (pom)
   (uber)
   (jar :file "thermos.jar")
   (sift :include #{#".*\.jar"})
   (target)))


;; (require '[boot.pod :as pod])


;; (deftask run-server []
;;   (let [worker (pod/make-pod (get-env))
;;         start (delay
;;                (pod/with-eval-in worker
;;                  (require '[thermos-ui.backend.main :as app])
;;                  (app/-main)))]
;;     (with-pre-wrap fileset
;;       @start
;;       (info "Server started")
;;       fileset)))


(set-env!
 :source-paths #{"src"}
 :resource-paths #{"resources"}
 :dependencies
 '[[seancorfield/boot-tools-deps "0.4.7" :scope "test"]
   [org.clojure/clojure       "1.9.0" :scope "provided"]
   [com.google.javascript/closure-compiler-unshaded "v20180805" :scope "test"]
   [org.clojure/clojurescript "1.10.439" :scope "test"]
   [org.clojure/test.check    "0.9.0" :scope "test"]
   
   
   [javax.servlet/servlet-api "2.5"    :scope "test"]
   [ring/ring-mock            "0.3.0"  :scope "test"]

   ;; Build tooling dependencies:

   [adzerk/boot-cljs              "2.1.4"  :scope "test"]
   [powerlaces/boot-figreload     "0.5.14"  :scope "test"
    :exclusions [org.clojure/tools.nrepl]]

   [adzerk/boot-cljs-repl   "0.4.0"] ;; latest release
   [cider/piggieback        "0.3.10"  :scope "test"]
   [weasel                  "0.7.0"  :scope "test"]
   [nrepl                   "0.4.5"  :scope "test"]
   
   [deraen/boot-less              "0.6.1"   :scope "test"]
   [adzerk/boot-test              "1.2.0"   :scope "test"]
   ])

(require '[adzerk.boot-cljs              :refer [cljs]]
         '[adzerk.boot-cljs-repl         :refer [cljs-repl-env start-repl cljs-repl]]
         '[powerlaces.boot-figreload     :refer [reload]]
         '[deraen.boot-less              :refer [less]]
         '[adzerk.boot-test :refer :all]

         '[boot-tools-deps.core :refer [deps]]
         )

(deftask testing
  "Profile setup for running tests."
  []
  (set-env! :source-paths #(conj % "test")
            :resource-paths #(conj % "test-resources"))
  identity)

(task-options!
 pom {:project 'thermos
      :version "0.1.0-SNAPSHOT"}
 aot {:namespace #{'thermos-backend.main}}
 jar {:main 'thermos-backend.main})

;; annoyingly, this can't work because we don't have the dependencies at this stage.
;(require '[thermos-backend.main :refer [create-system]])

(deftask dev
  "Run in development mode - this does live reload, and runs a repl.
   You can connect to the repl from e.g. emacs or by running boot repl -c"
  []
  (comp
   (deps :quick-merge true)
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

          :external-config
          {:devtools/config {:features-to-install [:formatters :hints :async]
                             :fn-symbol "λ"
                             :print-config-overrides true}}
          })
   ))

(deftask package
  "Create a runnable jar containing all the thermos stuff."
  []
  (comp
   (deps)
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


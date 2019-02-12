(ns build.pkg
  (:require [clojure.java.io :as io]
            [cljs.build.api :as cljs]
            [less4clj.api :as less]
            [clojure.edn :as edn]
            [badigeon.clean :as clean]
            [badigeon.compile :as compile]
            [badigeon.classpath :as classpath]
            [build.uberjar :as uberjar]))

(defmacro red [& body]
  `(println "\u001B[31m"
            ~@body
            "\u001B[0m"))

(defn build-jar []
  (red "CLEAN TARGET")

  (clean/clean "target")

  (def cljs-builds
    (edn/read-string (slurp "cljs-builds.edn")))

  (doseq [build cljs-builds]
    (red "cljs compile" (:main build))
    (cljs/build
     "src"
     (assoc build
            :preloads nil
            :infer-externs true
            :pretty-print true
            :pseudo-names true)))

  (red "less -> css")
  (less/build
   {:source-paths ["resources"]
    :target-path "target/resources/"
    :compression true})

  (red "clj backend compile")
  (compile/compile
   '[thermos-backend.core]
   {:compile-path "target/classes"
    :compiler-options {:disable-locals-clearing false
                       :elide-meta [:doc :file :line :added]
                       :direct-linking true}
    :classpath (classpath/make-classpath {:aliases [:server]})})

  ;; Finally create uberjar. For whatever reason, there is no good
  ;; library for doing this at the moment. Lots of people seem to have
  ;; made 95% of one, including depstar, and I am hacking the final 5%
  ;; here:
  (red "assemble uberjar")
  (uberjar/create-uberjar
   "target/thermos.jar"
   :classpath (classpath/make-classpath {:aliases [:server :jar]})
   :manifest {:Main-Class 'thermos-backend.core}
   ))

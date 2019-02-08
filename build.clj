;; This is the build script, which will produce a single jar.
;; You can run it with clj -Aserver:client build.clj

(require '[clojure.java.io :as io]
         '[cljs.build.api :as cljs]
         '[less4clj.api :as less]
         '[clojure.edn :as edn]
         '[badigeon.clean :as clean]
         '[badigeon.compile :as compile]
         '[badigeon.classpath :as classpath]
         '[hf.depstar.uberjar :as uberjar])

(defmacro red [& body]
  `(println "\u001B[31m"
            ~@body
            "\u001B[0m"))

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

(in-ns 'hf.depstar.uberjar)

(let [clash-strategy-0 clash-strategy]
  (defn clash-strategy [filename]
    (cond
      (= "META-INF/registryFile.jaiext" filename) :concat-lines
      :else (clash-strategy-0 filename))))

(in-ns 'user)

(let [classpath (classpath/make-classpath {:aliases [:server :build]})]

  (red "assemble uberjar")
  ;; generate a manifest
  (let [manifest-path "target/resources/META-INF/MANIFEST.MF"]
    (io/make-parents manifest-path)
    (spit manifest-path
          "Main-Class: thermos_backend.core$main\n"))
  
  (System/setProperty "java.class.path" classpath)
  (uberjar/run {:dest "target/thermos.jar"}))




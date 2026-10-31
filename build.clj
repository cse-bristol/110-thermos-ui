;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns build
  "Packaging. Run with `clojure -T:build <task>`, inside `nix develop` so
  lessc is available:

    uber        target/thermos.jar, the web application
                (`:debug true` for part-mangled advanced-mode js)
    heat-uber   target/thermos-heat-model.jar
    noder-uber  target/thermos-noder.jar
    css, cljs   just those parts, into target/resources

  Every task takes `:aliases` to add to the basis, e.g. `:aliases [:local-deps]`."
  (:require [clojure.tools.build.api :as b]
            [build.less :as less]
            [build.vendor :as vendor]))

(def class-dir "target/classes")
(def generated "target/resources")

(def ^:private clj-compiler-options
  {:disable-locals-clearing false
   :elide-meta [:doc :file :line :added]
   :direct-linking true})

;; lets java.imageio find the JAI codecs geotools uses
(def ^:private jai-manifest
  {"Specification-Title" "Java Advanced Imaging Image I/O Tools"
   "Specification-Version" "1.1"
   "Specification-Vendor" "Forward Dynamics"
   "Implementation-Title" "com.sun.media.imageio"
   "Implementation-Version" "1.1"
   "Implementation-Vendor" "Forward Dynamics"})

(def ^:private conflict-handlers
  {"META-INF/registryFile.jai" :append
   "META-INF/registryFile.jaiext" :append})

(defn- step [s] (println (str "\033[46;37m" s "\033[0m")))

(defn clean [_]
  (step "clean")
  (b/delete {:path "target"}))

(defn- with-client-loader
  "Call `f` with a class loader for the client classpath, where the cljsjs
  jars are"
  [{:keys [aliases]} f]
  (let [roots (:classpath-roots (b/create-basis {:aliases (into [:client] aliases)}))]
    (with-open [loader (java.net.URLClassLoader.
                        (into-array java.net.URL (map #(.toURL (java.io.File. %)) roots))
                        nil)]
      (f loader))))

(defn css [opts]
  (step "compile less to css")
  (with-client-loader opts vendor/copy-less-imports)
  (less/build "resources" generated [vendor/less-include-dir]))

(defn cljs [{:keys [debug aliases]}]
  (step "compile clojurescript")
  (let [opts (cond-> {:main 'thermos-frontend.editor
                      :output-to (str generated "/public/js/editor.js")
                      :output-dir (str generated "/public/js/editor.out")
                      :asset-path "/js/editor.out"
                      :externs ["externs/leaflet-extra.js"]
                      :optimizations :advanced
                      :infer-externs true
                      :parallel-build true}
               debug (assoc :pretty-print true :pseudo-names true))
        {:keys [exit]}
        (b/process
         (b/java-command
          {:basis (b/create-basis {:aliases (into [:server :client] aliases)})
           :main 'clojure.main
           :main-args ["-m" "cljs.main" "-co" (pr-str opts) "-c"]}))]
    (when-not (zero? exit)
      (throw (ex-info "ClojureScript compilation failed" {})))))

(defn- vendor-js [opts]
  (step "copy vendor js")
  (with-client-loader opts #(vendor/copy-vendor-js generated %)))

(defn- git-rev []
  (let [rev (b/git-process {:git-args "rev-parse HEAD"})]
    (step (str "git rev " rev))
    (spit (str generated "/git-rev.txt") (or rev ""))))

(defn- uberjar [{:keys [aliases main jar-file manifest]} opts]
  (let [basis (b/create-basis {:aliases (into aliases (:aliases opts))})]
    (step "compile clojure")
    (b/compile-clj {:basis basis
                    :ns-compile [main]
                    :class-dir class-dir
                    :compile-opts clj-compiler-options})
    (b/copy-dir {:src-dirs (cond-> ["src" "resources"]
                             (.exists (java.io.File. generated)) (conj generated))
                 :target-dir class-dir})
    (step (str "create " jar-file))
    (b/uber {:basis basis
             :class-dir class-dir
             :uber-file jar-file
             :main main
             :manifest (merge jai-manifest manifest)
             :conflict-handlers conflict-handlers})))

(defn uber [opts]
  (clean nil)
  (css opts)
  (cljs opts)
  (vendor-js opts)
  (git-rev)
  (uberjar {:aliases [:server]
            :main 'thermos-backend.core
            :jar-file "target/thermos.jar"
            ;; hikaricp needs reflective access that is otherwise forbidden
            :manifest {"Add-Opens" "java.base/java.lang"}}
           opts))

(defn heat-uber [opts]
  (clean nil)
  (uberjar {:aliases [:server]
            :main 'thermos-cli.heatmap
            :jar-file "target/thermos-heat-model.jar"}
           opts))

(defn noder-uber [opts]
  (clean nil)
  (uberjar {:aliases [:noder]
            :main 'thermos-cli.noder
            :jar-file "target/thermos-noder.jar"}
           opts))

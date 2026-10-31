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

(defn- npm-ci
  "Install exactly the npm packages in package-lock.json"
  []
  (step "npm ci")
  (let [{:keys [exit]} (b/process {:command-args ["npm" "ci"]})]
    (when-not (zero? exit)
      (throw (ex-info "npm ci failed" {})))))

(defn- ensure-node-modules []
  (when-not (.isDirectory (java.io.File. "node_modules"))
    (npm-ci)))

(defn css [_]
  (ensure-node-modules)
  (step "compile less to css")
  (less/build "resources" generated [vendor/less-include-dir]))

(defn cljs
  "Release build of the editor with shadow-cljs (see shadow-cljs.edn), in a
  JVM with the :server and :client classpath"
  [{:keys [debug aliases]}]
  (ensure-node-modules)
  (step "compile clojurescript")
  (let [{:keys [exit]}
        (b/process
         (b/java-command
          {:basis (b/create-basis {:aliases (into [:server :client] aliases)})
           :main 'clojure.main
           :main-args (cond-> ["-m" "shadow.cljs.devtools.cli" "release" "editor"]
                        debug (conj "--debug"))}))]
    (when-not (zero? exit)
      (throw (ex-info "ClojureScript compilation failed" {})))))

(defn- vendor-js []
  (step "copy vendor js")
  (vendor/copy-vendor-js generated))

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
  (npm-ci)
  (css opts)
  (cljs opts)
  (vendor-js)
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

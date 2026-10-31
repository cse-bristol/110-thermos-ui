;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns build.vendor
  "Javascript and CSS libraries that come out of the cljsjs jars the
  editor uses, so there is only one version of each. Resources are looked
  up in `loader`, by default the current classpath."
  (:require [clojure.java.io :as io]))

(def ^:private js-files
  {"cljsjs/leaflet/production/leaflet.min.inc.js"           "leaflet.js"
   "cljsjs/leaflet-draw/production/leaflet-draw.min.inc.js" "leaflet-draw.js"})

(def ^:private less-imports
  ["cljsjs/leaflet/common/leaflet.inc.css"
   "cljsjs/leaflet-draw/common/leaflet-draw.inc.css"])

(def less-include-dir
  "Where `copy-less-imports` puts the files, for lessc's --include-path"
  "target/less-include")

(defn- default-loader [] (.getContextClassLoader (Thread/currentThread)))

(defn- copy-resource [^ClassLoader loader resource out]
  (io/make-parents out)
  (with-open [in (io/input-stream (or (io/resource resource loader)
                                      (throw (ex-info "Missing vendor resource" {:resource resource}))))]
    (io/copy in out)))

(defn copy-vendor-js
  "Copy the javascript libraries into public/js/vendor under `target-dir`"
  ([target-dir] (copy-vendor-js target-dir (default-loader)))
  ([target-dir loader]
   (doseq [[resource file-name] js-files]
     (copy-resource loader resource (io/file target-dir "public" "js" "vendor" file-name)))))

(defn copy-less-imports
  "Copy the stylesheets our LESS imports from the classpath into
  `less-include-dir`, at their classpath paths"
  ([] (copy-less-imports (default-loader)))
  ([loader]
   (doseq [resource less-imports]
     (copy-resource loader resource (io/file less-include-dir resource)))))

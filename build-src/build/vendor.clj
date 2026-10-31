;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns build.vendor
  "Javascript and CSS libraries from node_modules (package.json), which the
  editor's clojurescript uses too, so there is only one version of each."
  (:require [clojure.java.io :as io]))

(def ^:private js-files
  {"leaflet/dist/leaflet.js"           "leaflet.js"
   "leaflet-draw/dist/leaflet.draw.js" "leaflet-draw.js"})

(def less-include-dir
  "For lessc's --include-path, so LESS can @import stylesheets from npm
  packages, e.g. \"leaflet/dist/leaflet.css\""
  "node_modules")

(defn copy-vendor-js
  "Copy the javascript libraries into public/js/vendor under `target-dir`"
  [target-dir]
  (doseq [[source file-name] js-files]
    (let [in (io/file "node_modules" source)
          out (io/file target-dir "public" "js" "vendor" file-name)]
      (when-not (.exists in)
        (throw (ex-info (str "Missing " in ", has npm install been run?") {:file in})))
      (io/make-parents out)
      (io/copy in out))))

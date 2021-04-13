;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns user
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

;; To get a problem out of one thermos and into another:
;; 1. Run this javascript in the console
;; (function() { eval(document.head.getElementsByTagName('script')[0].text); var blob = new Blob([thermos_preloads], {type:'text/plain'}); var  u = URL.createObjectURL(blob); var a = document.createElement('a'); a.href = u; a.download = window.title + ".edn"; a.dispatchEvent(new MouseEvent("click")); })()
;; 2. Use the function below to load it into a project


(defn read-preload [out-path name project-id map-id]
  (let [preloads (with-open [r (java.io.PushbackReader. (io/reader out-path))]
                   (edn/read r))]
    (thermos-backend.db.projects/save-network!
     "tom.hinton@cse.org.uk" project-id map-id name (:initial-state preloads))))

(comment
  (read-preload
   "/home/hinton/dl/THERMOS - end demand_ ok 200MW, infeasible 300MW.edn"
   "end demand_ ok 200MW, infeasible 300MW.edn"
   2 4
   )
  )

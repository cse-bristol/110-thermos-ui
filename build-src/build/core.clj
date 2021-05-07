;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns build.core
  (:require [clojure.tools.cli :refer [parse-opts]]))

(def opts-for
  {:dev [["-e" "--emacs" "Use cider middleware for emacs"]
         ["-d" "--debug" "Produce part-mangled js to help debug what's wrong when using advanced optimisations"]]
   :pkg [["-d" "--debug" "Produce part-mangled js to help debug what's wrong when using advanced optimisations"]]}
  )

(defn -main [& args]
  (let [args (or args ["dev"])
        [com & args] args
        com (keyword com)
        opts (parse-opts args (opts-for com))]

    (case com
      :pkg (do (require 'build.pkg)
               ((resolve 'build.pkg/build-jar)
                {:debug-optimizations
                 (:debug (:options opts))}
                ))

      :cli-pkg (do (require 'build.pkg)
                   ((resolve 'build.pkg/build-cli-tool)))

      :heat-pkg (do (require 'build.pkg)
                    ((resolve 'build.pkg/build-heatmap-only-tool)))

      :noder-pkg (do (require 'build.pkg)
                     ((resolve 'build.pkg/build-noder)))
      
      :dev (do
             (require 'build.dev)
             ((resolve 'build.dev/start-dev)
              {:nrepl-handler
               (if (:emacs (:options opts))
                 'build.emacs/emacs-middleware
                 'nrepl.server/default-handler)
               
               :debug-optimizations
               (:debug (:options opts))}))
      
      (do (println "Known tasks are pkg and dev")
          (println "e.g. clj -Aclient:server:dev dev")))))


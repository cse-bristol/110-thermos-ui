(ns build.core
  (:require [clojure.tools.cli :refer [parse-opts]]))

(def opts-for
  {:dev [["-c" "--cider" "Use cider middleware"]]
   :pkg []}
  )

(defn -main [& args]
  (let [args (or args ["dev"])
        [com & args] args
        com (keyword com)
        opts (parse-opts args (opts-for com))]
    (case com
      :pkg (do (require 'build.pkg)
                ((resolve 'build.pkg/build-jar)))
      :dev (do (require 'build.dev)
                ((resolve 'build.dev/start-dev) (:options opts)))

      (do (println "Known tasks are pkg and dev")
          (println "e.g. clj -Aclient:server:dev dev")))))


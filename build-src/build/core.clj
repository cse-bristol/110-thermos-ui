(ns build.core)

(defn -main [& args]
  (case (first args)
    "pkg" (do (require 'build.pkg)
              ((resolve 'build.pkg/build-jar)))
    "dev" (do (require 'build.dev)
              ((resolve 'build.dev/start-dev)))

    ;; TODO maybe add some arg to dev to allow this atom repl thing (proto-nrepl)
    ;; TODO rewrite https://github.com/seancorfield/depstar/blob/master/src/hf/depstar/uberjar.clj a bit to do manifest etc

    (do (println "Known tasks are pkg and dev")
        (println "e.g. clj -Aclient:server:dev dev")
        ))
  )


(ns build.core)

(defn -main [& args]
  (case (first args)
    "jar" (do (require 'build.jar)
              ((resolve 'build.jar/build-jar)))
    "dev" (do (require 'build.dev)
              ((resolve 'build.dev/start-dev)))
    

    (do (println "Known tasks are jar and dev")
        (println "e.g. clj -Aclient:server:dev dev")
        ))
  )


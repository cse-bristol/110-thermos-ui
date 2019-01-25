(set-env!
 :source-paths #{"src"}
 :resource-paths #{"resources"}
 :dependencies '[[seancorfield/boot-tools-deps "0.4.7" :scope "test"]]
 )

(require '[boot-tools-deps.core :refer [deps]])

(deftask less []
  (merge-env! :dependencies '[[deraen/boot-less "0.6.2"]])
  (require 'deraen.boot-less)
  (let [less (resolve 'deraen.boot-less/less)]
    (comp (deps :quick-merge true
                :aliases [:client])
          (less))))

(deftask dev-frontend []
  (merge-env! :exclusions  '[org.clojure/clojurescript]
              :dependencies
              '[[org.clojure/clojurescript "1.9.946"]
                [adzerk/boot-cljs "2.1.4"]
                [powerlaces/boot-figreload "0.5.14"]])
  
  (require 'adzerk.boot-cljs 'powerlaces.boot-figreload)
  (let [cljs (resolve 'adzerk.boot-cljs/cljs)
        figreload (resolve 'powerlaces.boot-figreload/reload)]
    (comp (deps :aliases [:client])
          
          (figreload)

          ;; this is where cljs-repl would go, if we turn it on
          
          (cljs :source-map true
                :optimizations :none
                :compiler-options
                {:parallel-build true})
          )))

(deftask dev-backend
  "Start the server-side part"
  []
  (comp
   (deps :aliases [:server])
   (repl :server true)
   ;; we could probably put a (refresh) in here to do auto tools
   ;; namespace refreshing, but it's easier just to do it by hand.
   (with-pass-thru fs
     (require '[mount.core]
              '[clojure.tools.namespace.repl :as tns]
              '[thermos-backend.core])
     
     ((resolve 'mount.core/start)))
   ;; (repl-eval :evaluate
   ;;            '(do
   ;;                (require '[clojure.tools.namespace.repl :as tns]
   ;;                         '[mount.core :as mount])
                  
   ;;                (defn start-server []
   ;;                  (require '[thermos-backend.core])
   ;;                  (mount/start))

   ;;                (start-server)
                  
   ;;                "Server started"
   ;;                ))
   ))

(deftask dev
  "Start incremental compilation for javascript frontend, and serve the backend.

To reload backend code, connect a repl (e.g. boot repl -c) and evaluate (tns/refresh)."
  []
  (comp
   (watch :verbose false :debounce 50)
   (dev-backend)
   (less)
   (dev-frontend)))



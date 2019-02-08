(set-env!
 :source-paths #{"src"}
 :resource-paths #{"resources"}
 :exclusions '[org.clojure/clojurescript org.clojure/tools.reader]
 :dependencies '[[seancorfield/boot-tools-deps "0.4.7" :scope "test"]
                 [deraen/boot-less "0.6.2" :scope "test"]
                 [org.clojure/clojurescript "1.9.946" :scope "test"]
                 [adzerk/boot-cljs "2.1.4" :scope "test"]
                 [powerlaces/boot-figreload "0.5.14" :scope "test"]
                 [org.clojure/tools.reader "1.3.0"]])

(require '[boot-tools-deps.core :refer [load-deps]]
         '[deraen.boot-less :refer [less]]
         '[adzerk.boot-cljs :refer [cljs]]
         '[powerlaces.boot-figreload :as figreload]
         '[boot.core :as boot]
         '[boot.pod :as pod])

(defn- fix-aliases [args]
  (let [aliases     (vec (:aliases args))
        add-aliases #(into aliases %)]
    
    (-> args
        (update :class-path-aliases add-aliases)
        (update :main-aliases add-aliases)
        (update :resolve-aliases add-aliases)
        (dissoc :aliases))))

(defn deps* [args]
  (let [args (fix-aliases args)]
    (fn middleware [next-handler]
      (fn handler [fileset]
        (load-deps args)
        (next-handler fileset)))))

(defmacro with-load-deps [args & body]
  (let [overwrite-boot-deps (:overwrite-boot-deps args)

        outer-args
        (case overwrite-boot-deps
          :in-middleware (assoc args :overwrite-boot-deps true)
          :in-task (assoc args :overwrite-boot-deps false)
          args)

        inner-args
        (case overwrite-boot-deps
          :in-middleware (assoc args :overwrite-boot-deps false)
          :in-task (assoc args :overwrite-boot-deps true)
          args)

        ]
    `(let [env# (boot/get-env)]
       (load-deps ~(fix-aliases outer-args))
       (let [result# (do ~@body)]
         (boot/set-env! env#)
         (comp (deps* ~inner-args)
               result#)))))

(deftask dev
  []
  (comp
   (watch :debounce 50)
   (with-load-deps {:aliases [:server]}
     (comp
      (repl :server true)
      (with-pass-thru fs
        (require '[mount.core]
                 '[clojure.tools.namespace.repl :as tns]
                 '[thermos-backend.core])
        
        ((resolve 'mount.core/start)))))
   (with-load-deps {:aliases [:client] :quick-merge true}
     (comp
      (less :source-map true)
      (figreload/reload)
      (cljs :source-map true
            :optimizations :none
            :compiler-options {:parallel-build true})))))

(deftask build
  []
  (comp
   (with-load-deps {:aliases [:client] :quick-merge true}
     (comp
      (less :compression true)
      (cljs :source-map false
            :optimizations :advanced
            :compiler-options
            {:parallel-build true
             :preloads nil
             :infer-externs true
             :externs ["externs/leaflet-extra.js"]})))

   (with-load-deps {:aliases [:server] :quick-merge true}
     (comp
      (aot :namespace #{'thermos-backend.core})
      (pom :project 'thermos
           :version "4.1.0-SNAPSHOT")
      (deps* {:aliases [:server] :overwrite-boot-deps true})
      (uber)))
   
   (jar :main 'thermos-backend.core
        :file "thermos.jar")
   (sift :include #{#".*\.jar"})
   (target)))


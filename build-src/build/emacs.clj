(ns build.emacs
  (:require [nrepl.server :refer [default-handler]]
            [refactor-nrepl.middleware]
            [cider.piggieback]
            [cider.nrepl :refer [cider-middleware]]))

(def emacs-middleware
  (apply default-handler
         (cons #'cider.piggieback/wrap-cljs-repl
               (cons #'refactor-nrepl.middleware/wrap-refactor
                     (map resolve cider-middleware))
               )
         
))

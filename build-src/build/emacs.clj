(ns build.emacs
  (:require [nrepl.server :refer [default-handler]]
            [refactor-nrepl.middleware]
            [cider.nrepl :refer [cider-middleware]]))

(def emacs-middleware
  (apply default-handler
         (resolve 'refactor-nrepl.middleware/wrap-refactor)
         (map resolve cider-middleware)))

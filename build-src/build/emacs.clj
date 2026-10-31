;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns build.emacs
  (:require [nrepl.server :refer [default-handler]]
            [refactor-nrepl.middleware]
            [cider.nrepl :refer [cider-middleware]]
            [cider.piggieback]
            [shadow.cljs.devtools.server.nrepl]))

(def emacs-middleware
  (apply default-handler
         #'cider.piggieback/wrap-cljs-repl
         #'shadow.cljs.devtools.server.nrepl/middleware
         #'refactor-nrepl.middleware/wrap-refactor
         (map resolve cider-middleware)))

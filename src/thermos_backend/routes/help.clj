;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.routes.help
  (:require [thermos-backend.routes.responses :refer :all]
            [ring.util.response :as response]
            [thermos-backend.pages.help :refer :all]))

(def help-routes
  ["/help" [["" (constantly (response/redirect "./help/index.html"))]
            ["/search" (fn [{{q :q} :params :as req}]
                         (html (help-search q)))]
            ["/changelog" (constantly (html (help-changelog)))]
            [["/" [#".+" :page-name]] (fn [{{p :page-name} :params}]
                                (html (help-page p)))]]])


(ns thermos-backend.pages.core
  (:require [compojure.core :refer :all]
            [compojure.coercions :refer [as-int]]

            [bidi.ring]
            [thermos-backend.routes.help]
            [thermos-backend.routes.landing]
            [thermos-backend.routes.project]
            [thermos-backend.routes.admin]
            [thermos-backend.routes.convert]
            
            [thermos-backend.auth :as auth]
            [thermos-backend.config :refer [config]]
            [thermos-backend.pages.landing-page :refer [landing-page]]
            [thermos-backend.pages.user-settings :refer [settings-page]]
            [thermos-backend.pages.projects :refer [new-project-page
                                                    project-page
                                                    delete-project-page]]
            [thermos-backend.pages.maps :as map-pages]
            [thermos-backend.pages.help :refer [help-page help-search help-changelog]]
            [thermos-backend.pages.admin :as admin]
            [thermos-backend.pages.editor :refer [editor-page]]
            [ring.util.response :as response]
            [ring.util.io :as ring-io]

            [thermos-util.converter :refer [network-problem->geojson]]

            [thermos-backend.db.projects :as projects]
            [thermos-backend.db.maps :as maps]
            [thermos-backend.importer.core :as importer]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [clojure.edn :as edn]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [thermos-backend.solver.core :as solver]
            [cognitect.transit :as transit]
            [thermos-backend.db.users :as users]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [thermos-backend.queue :as queue]
            [thermos-backend.pages.cache-control :as cache-control])
    
  (:import [javax.mail.internet InternetAddress]
           [java.io ByteArrayInputStream]))

(defroutes page-routes
  (GET "/favicon.ico" [] (response/resource-response "/public/favicon.ico"))


  (bidi.ring/make-handler thermos-backend.routes.help/help-routes)
  (bidi.ring/make-handler thermos-backend.routes.admin/admin-routes)
  (bidi.ring/make-handler thermos-backend.routes.landing/landing-routes)
  (bidi.ring/make-handler thermos-backend.routes.project/project-routes)
  (bidi.ring/make-handler thermos-backend.routes.convert/converter-routes)
  )

(ns thermos-ui.backend.store-service.routes
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [thermos-ui.backend.store-service.problems :as p]))

(defonce json-headers {"Content-Type" "text/html"})

(defn routes-list []
  "List routes"
  ;;"I'd like this to be an auto generated list from defroutes, but that's a macro..."
  {"/problem/:org/" {:method "GET" :description "List all problems for :org"}
   "/problem/:org/:name/" {:method "POST" :description "Post a problem"
                           :params {"file" "Problem file in .edn format"}}
   "/problem/:org/:name/:id" {:method "DELETE" :description "Delete specific problem version"}
   })

(defn- json-list-response
  [problem-list]
  (if (> (count problem-list) 0)
    {:status 200
     :headers json-headers
     :body problem-list}
    {:status 404}))

(defroutes all
  (POST "/problem/:org/:name/"
        {{org :org
          name :name
          problem :file} :params :as params}
        (let [problem-file (problem :tempfile)
              stored (p/insert org name problem-file)]
          (if (:file stored)

            {:status 201
             :headers (assoc json-headers
                             "Location"
                             (:id stored)
                             "X-Problem-ID"
                             (:id stored)
                             )}
            {:status 500})))

  (GET "/problem/:org/:name/:id"
       [org name id]
       (if-let [problem (p/get-file org name id)]
         {:status 200
          :headers json-headers
          :body problem}
         {:status 404}))

  ;; (DELETE "/problem/:org/:name/:id"
  ;;         {{org :org
  ;;           name :name
  ;;           id :id} :params :as params}
  ;;         (if (p/delete org name id)
  ;;           {:status 204}
  ;;           {:status 404}))
  )

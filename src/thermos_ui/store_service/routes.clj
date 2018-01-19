(ns thermos-ui.store-service.routes
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [thermos-ui.store-service.store.problems :as p]))

(defonce json-headers {"Content-Type" "text/html"})

(defn routes-list []
  "List routes"
  ;;"I'd like this to be an auto generated list from defroutes, but that's a macro..."
  {"/problem/:org/" {:method "GET" :description "List all problems for :org"}
   "/problem/:org/:name/" {:method "POST" :description "Post a problem"
                           :params {"file" "Problem file in .edn format"}}
   "/problem/:org/:name/:id" {:method "DELETE" :description "Delete specific problem version"}
   })

(defroutes all
  (POST "/problem/:org/:name/"
        {{org :org
          name :name
          problem :file} :params :as params}
        (let [problem-file (problem :tempfile)
              stored (p/store org name problem-file)]
          (if (nil? (:location stored))
            {:status 500}
            {:status 201
             :headers (assoc json-headers
                             "Location:" (str "http://localhost:3449/" (:location stored)))})))
     
  (GET "/problem/:org/"
       {{org :org}
        :params :as params}
       (let [list (p/gather org)]
         (if (> (count list) 0)
           {:status 200
            :headers json-headers
            :body list}
           {:status 404})))

  (DELETE "/problem/:org/:name/:id"
          {{org :org
            name :name
            id :id} :params :as params}
          (if (p/delete org name id)
            {:status 204}
            {:status 404})))

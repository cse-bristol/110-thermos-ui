(ns thermos-ui.store-service.routes
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [thermos-ui.store-service.store.problems :as p]))

(defroutes all
  (POST "/problem/:org/:name/"
        {{org :org
          name :name
          problem :file} :params :as params}
        (let [problem-file (problem :tempfile)
              stored (p/store org name problem-file)
              headers {"Content-Type" "application/octet-stream"}]
          (if (nil? (:location stored))
            {:status 500}
            {:status 201
             :headers (assoc headers
                             "Location:" (:location stored))})))
     
  (GET "/problem/:org/"
       {{org :org}
        :params :as params}
       (let [list (p/gather org)]
         (if (> (count list) 0)
           {:status 200
            :headers  {"Content-Type" "text/html"}
            :body list}
           {:status 404})))

  (DELETE "/problem/:org/:name/:id"
          {{org :org
            name :name
            id :id} :params :as params}
          (if (p/delete org name id)
            {:status 204}
            {:status 404})))

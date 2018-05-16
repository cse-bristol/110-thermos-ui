(ns thermos-ui.backend.problems.routes
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [thermos-ui.backend.problems.db :as db]
            [thermos-ui.backend.queue :as queue]
            ))

(defonce json-headers {"Content-Type" "text/html"})

(defn routes-list []
  "List routes"
  ;;"I'd like this to be an auto generated list from defroutes, but that's a macro..."
  {"/problem/:org" {:method "GET" :description "List all problems for :org"}
   "/problem/:org/:name" {:method "POST" :description "Post a problem"
                          :params {"file" "Problem file in .edn format"}}
   "/problem/:org/:name/:id" {:method "DELETE" :description "Delete specific problem version"}
   })

(defn all [db queue]
  (routes
   (POST "/problem/:org/:name"
         {{org :org
           name :name
           problem :file
           run :run} :params :as params}
         (let [problem-file (problem :tempfile)
               stored (db/insert! db org name problem-file)]
           (if stored
             (let [problem-id (str stored)
                   run-id (when run (queue/put queue :problems [org name stored]))
                   ]
               {:status 201
                :headers (assoc json-headers
                                "Location" (str stored)
                                "X-Problem-ID" (str stored)
                                "X-Run-ID" (str run-id)
                                )}
               )
             
             {:status 500})))

   (GET "/problem/:org/:name/:id"
        [org name id]
        (if-let [problem (db/get-content db org name (Integer/parseInt id))]
          {:status 200
           :headers json-headers
           :body problem}
          {:status 404}))

   (DELETE "/problem/:org/:name"
           [org name]
           (db/delete! db org name)
           {:status 204}))
  
  )

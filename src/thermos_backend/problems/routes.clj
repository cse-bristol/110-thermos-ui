(ns thermos-backend.problems.routes
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [thermos-backend.problems.db :as db]
            [thermos-backend.queue :as queue]

            ))

(defonce json-headers {"Content-Type" "application/json"
                       "Cache-Control" "no-store"})

(defn all [db queue]
  (routes

   ;; TODO make this work better using websockets and LISTEN/NOTIFY in
   ;; postgres, or at least a query / index on the table
   ;; or a join from the problems table
   (GET "/problem/:org/:name/:id/status"
        [org name id]
        (if-let [{job-id :job} (db/get-details db org name (Integer/parseInt id))]
          (let [status (queue/status queue job-id)]
            {:status 200
             :headers json-headers
             :body status})
          {:status 404}))
   
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
               (when run-id (db/set-job-id db org name stored run-id))
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
          {:status 200 :headers json-headers :body problem}
          {:status 404}))

   (DELETE "/problem/:org/:name"
           [org name]
           (db/delete! db org name)
           {:status 204}))
  
  )

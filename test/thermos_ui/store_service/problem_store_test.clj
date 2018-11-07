(ns thermos-store-service.problem_store_test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [ring.mock.request :as mock]
            [thermos-backend.handler :refer :all]
            [thermos-backend.store-service.problems :as problem]
            [environ.core :refer [env]]))

(defonce store-location (env :problem-store))
(defonce problem-file (clojure.string/join "/" [store-location "213123213" "name" "123123132.edn"]))

(defn- delete-org-store [org]
  (let [fname (clojure.string/join "/" [store-location org])
        func (fn [func f]
               (when (.isDirectory f)
                 (doseq [f2 (.listFiles f)]
                   (func func f2)))
               (clojure.java.io/delete-file f))]
    (func func (clojure.java.io/file fname))))

(deftest getting-location-of-problem
  (testing "Can create a new problem"
    (let [response (app
                    (-> (mock/request :post "/api/problem/temp/name/")
                        (assoc :params {:file {:filename "test-problem.edn"
                                               :tempfile problem-file
                                               :content-type nil
                                               :size 27
                                             }})))]
      (is (= (:status response) 201))))
  (delete-org-store "temp"))

(deftest storing-a-problem
   (let [org "org"
         problem-name "name"
         stored-problem (problem/store org problem-name (io/as-file problem-file))]

     (testing "Can store new problem"
       (is (not (nil? stored-problem)))
       (is (not (nil? (:location stored-problem))))
       (is (not (nil? (:id stored-problem)))))

     (testing "Can get existing problem"
       (let [problem (problem/getone org problem-name (:id stored-problem))]
        (is (not (nil? problem)))
        (is (not (nil? (:location problem))))))
     (delete-org-store org)))

(deftest listing-problems-from-store
  (let [org-key "213123213"]
    (testing "Can list all problems for specific organisation"
      (let [org-probs (problem/gather org-key)]
        (is (not (nil? org-probs)))
        (is (= 1 (count org-probs)))
        (is (not (nil? (get org-probs :123123132))))))

    (testing "Can list all problems for org and name"
      (let [name "name"
            org-probs (problem/gather org-key name)]
        (is (not (nil? org-probs)))
        (is (= 1 (count org-probs)))
        (is (not (nil? (get org-probs :123123132))))))))

(deftest listing-problems-from-handler
  (testing "Can list all problems for org"
    (let [response (app (mock/request :get "/api/problem/213123213/"))]
      (is (= (:status response) 200))
      ;;TODO Test contents of response
      ))

  (testing "Can list problems for org and name"
    (let [response (app (mock/request :get "/api/problem/213123213/name/"))]
      (is (= (:status response) 200))
      (is (not (nil? (:body response))))))

  (testing "Test response if no problems found"
    (let [response (app (mock/request :get "/api/problem/unknown/"))]
      (is (= (:status response) 404)))))

(deftest can-delete-a-problem-version
  (let [stored-problem (problem/store "delete" "me" problem-file)]
    (testing "Can delete a problem version"
      (let [response (app (mock/request
                           :delete (str "/api/problem/delete/me/" (:id stored-problem))))]
        (is (not (nil? response)))
        (is (= (:status response) 204))))

    (testing "Get a 404 if file  not found"
      (let [response (app (mock/request
                           :delete (str "/api/problem/blah/blah/blah")))]
        (is (= (:status response) 404))))
  (delete-org-store "delete")))

(deftest can-get-problem-from-handler
  (testing "Correct response when problem exists"
    (let [response (app (mock/request :get "/api/problem/213123213/name/123123132"))]
      (is (= (:status response) 200)))))
    

(defn test-ns-hook []
  "Run the tests in this order"
  (storing-a-problem)
  (getting-location-of-problem)
  (listing-problems-from-store)
  (listing-problems-from-handler)
  (can-delete-a-problem-version))

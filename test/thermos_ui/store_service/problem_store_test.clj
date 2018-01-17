(ns thermos-ui.store-service.problem_store_test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [ring.mock.request :as mock]
            [thermos-ui.handler :refer :all]
            [thermos-ui.store-service.store.problems :as problem]))

(defonce problem-file (io/as-file "data/test-problem.edn"))

(deftest getting-location-of-problem
  (testing "Will return location of existing problem"
    (let [response (app
                    (-> (mock/request :post "/problem/temp/name/")
                        (assoc :params {:file {:filename "test-problem.edn"
                                               :tempfile problem-file
                                               :content-type nil
                                               :size 27
                                             }})))]
      ;;(println response)
      (is (= (:status response) 201)))))

(deftest storing-a-problem
   (let [org "org"
         problem-name "name"
         problem problem-file
         stored-problem (problem/store org problem-name problem)]
     
     (testing "Can store new problem"
       (is (not (nil? stored-problem)))
       (is (not (nil? (:location stored-problem))))
       (is (not (nil? (:id stored-problem)))))

     (testing "Can get existing problem"
       (let [problem (problem/getone org problem-name (:id stored-problem))]
         (is (not (nil? problem)))
         (is (not (nil? (:location problem))))))))

(deftest listing-problems-from-store
  (testing "Can list all problems for specific organisation"
    (let [org-key "213123213"
          org-probs (problem/gather org-key)]
      (is (not (nil? org-probs)))
      (is (= 1 (count org-probs)))
      (is (not (nil? (get org-probs :123123132)))))))

(deftest listing-problems-from-handler
  (testing "Can list all problems for org"
    (let [response (app (mock/request :get "/problem/213123213/"))]
      (is (= (:status response) 200))))

  (testing "Test response if no problems found"
    (let [response (app (mock/request :get "/problem/unknown/"))]
      (is (= (:status response) 404)))))

(deftest can-delete-a-problem-version
  (let [stored-problem (problem/store "delete" "me" problem-file)]
    (testing "Can delete a problem version"
      (let [response (app (mock/request
                           :delete (str "/problem/delete/me/" (:id stored-problem))))]
        (is (not (nil? response)))
        (is (= (:status response) 204))))

    (testing "Get a 404 if file  not found"
      (let [response (app (mock/request :delete
                                        (str "/blah/blah/bla/")))]
            (is (= (:status response 404)))))))

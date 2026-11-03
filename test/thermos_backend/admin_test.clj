;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.admin-test
  (:require [thermos-backend.routes.admin :as admin]
            [clojure.test :as t]))

(t/deftest job-filters
  (t/is (= {:queue nil :state nil :user nil :page 0 :per-page 50}
           (admin/job-filters {})))
  (t/is (= {:queue "problems" :state "failed" :user "a@b.c" :page 2 :per-page 50}
           (admin/job-filters {:queue "problems" :state "failed" :user " a@b.c " :page "2"})))
  (t/testing "nonsense is ignored"
    (t/is (= {:queue nil :state nil :user nil :page 0 :per-page 50}
             (admin/job-filters {:queue "" :state "exploded" :page "-3"})))
    (t/is (zero? (:page (admin/job-filters {:page "lots"}))))))

(def ^:private members
  [{:id "admin@x" :auth "admin"}
   {:id "writer@x" :auth "write"}])

(defn- error [members op user auth & [exists]]
  (admin/member-change-error members op user auth (if (nil? exists) true exists)))

(t/deftest member-changes
  (t/is (nil? (error members "add" "new@x" :read)))
  (t/is (nil? (error members "set" "writer@x" :admin)))
  (t/is (nil? (error members "remove" "writer@x" nil)))
  (t/is (error members "add" "nobody@x" :read false) "users must exist")
  (t/is (error members "add" "new@x" :owner) "roles must be real")
  (t/is (error members "explode" "writer@x" :read))
  (t/is (error members "add" nil :read))

  (t/testing "the last admin stays"
    (t/is (error members "remove" "admin@x" nil))
    (t/is (error members "set" "admin@x" :write))
    (t/is (nil? (error members "set" "admin@x" :admin)))
    (let [two-admins (conj members {:id "boss@x" :auth "admin"})]
      (t/is (nil? (error two-admins "remove" "admin@x" nil))))
    (t/is (nil? (error [] "add" "new@x" :write))
          "an orphan project can get any member")))

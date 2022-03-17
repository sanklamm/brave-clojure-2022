(ns unit.cap05-test
  (:require [chap05 :as sut]
            [clojure.test :as t
             :refer [deftest testing is]]))

(deftest my-comp-test
  (testing "my own comp function: my-comp"
    (is (= ((comp) 1)
           ((sut/my-comp) 1)))
    (is (= ((comp inc) 1)
           ((sut/my-comp inc) 1)))
    (is (= ((comp inc inc) 1)
           ((sut/my-comp inc inc) 1)))
    (is (= ((comp inc inc inc dec *) 2 5)
           ((sut/my-comp inc inc inc dec *) 2 5)))
    (is (= ((comp inc) 1)
           ((sut/my-comp inc) 1)))))


(deftest my-assoc-in-test
  (testing "my own assoc-in function: my-assoc-in"
    (is (= (assoc-in {} [:foo :bar :baz] "bat")
           (sut/my-assoc-in {} [:foo :bar :baz] "bat")))
    (is (= (assoc-in {:this "that"} [:foo :bar :baz] "bat")
           (sut/my-assoc-in {:this "that"} [:foo :bar :baz] "bat")))
    (is (= (assoc-in {} [:foo] "bar")
           (sut/my-assoc-in {} [:foo] "bar")))))

(def my-map {:this "that", :foo {:bar {:baz 1}}})

(deftest my-update-in-test
  (testing "my own update-in function: my-update-in"
    (is (= (update-in {:foo 2} [:foo] inc)
           (sut/my-update-in {:foo 2} [:foo] inc)))
    (is (= (update-in {:foo 2} [:foo] * 3 4)
           (sut/my-update-in {:foo 2} [:foo] * 3 4)))
    (is (= (update-in my-map [:foo :bar :baz] (juxt inc dec odd? even?))
           (sut/my-update-in my-map [:foo :bar :baz] (juxt inc dec odd? even?))))))

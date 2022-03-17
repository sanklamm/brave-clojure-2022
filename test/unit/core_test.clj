(ns core-test
  (:require [core :as sut]
            [clojure.test :as t
             :refer [deftest testing is]]))


(deftest sample
  (testing "1 + 1 = 2"
    (is (= 2 (+ 1 1)))))

;; (deftest failing-test
;;   (testing "1 + 1 = 2"
;;     (is (= 3 (+ 1 1)))))

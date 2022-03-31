(ns unit.chap07-test
 (:require [chap07 :as sut]
           [clojure.test :as t
            :refer [deftest testing is]]))


(deftest infix-test
  (testing "converting infix notation to prefix notation"
    (is (= '(+ 1 (- (* 3 4) 5))
           (sut/unfix '(1 + 3 * 4 - 5))))))

(ns protocols.core-test
  (:require [clojure.test   :refer [deftest is testing]]
            [protocols.core :refer []]))

(deftest a-test
  (testing "fixed"
    (is (= 1 1))))

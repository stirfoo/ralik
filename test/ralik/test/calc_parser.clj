(ns ralik.test.calc-parser
  (:use [parsers.calc.parser])
  (:use [clojure.test]))

(deftest t1
  (testing "calc"
    (is (= (calc "2+2")
           4))
    (is (= (calc "x=3; x+x")
           6))))

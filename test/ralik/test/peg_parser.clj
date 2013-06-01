(ns ralik.test.peg-parser
  (:use [parsers.peg.parser])
  (:use [clojure.test]))

(deftest t1
  (testing "parse of peg.peg by peg parser =)"
    (is (peg (slurp "src/parsers/peg/peg.peg")))))

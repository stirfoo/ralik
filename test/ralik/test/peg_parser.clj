(ns ralik.test.peg-parser
  (:use [parsers.peg.parser])
  (:use [clojure.test]))

(def filename (gensym "peg"))

(deftest t1
  (testing "peg parser"
    (is (peg (slurp "src/parsers/peg/peg.peg")))))

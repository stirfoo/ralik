(ns ralik.test.bencode-parser
  (:use [parsers.bencode.parser])
  (:use [clojure.test]))

(deftest t1
  (testing "integer"
    (is (= (bencode "i1e") [1])))
  (testing "binary string"
    (is (= (bencode "3:xyz") ["xyz"])))
  (testing "list"
    (is (= (bencode "li1e3:xyze") [[1 "xyz"]])))
  (testing "dictionary"
    (is (= (bencode "d1:xi42e1:yi99ee") [{"x" 42 "y" 99}]))))

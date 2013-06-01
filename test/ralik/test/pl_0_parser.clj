(ns ralik.test.pl-0-parser
  (:use [parsers.pl-0.parser])
  (:use [clojure.test]))

(deftest pl-0-output
  (testing "output of parsed and evaluated pl-0 source"
    (is (= (with-out-str
             (pl-0 (slurp "src/parsers/pl_0/ex1.pl0")))
           "1\n4\n9\n16\n25\n36\n49\n64\n81\n100\n"))))

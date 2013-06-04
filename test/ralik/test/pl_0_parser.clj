(ns ralik.test.pl-0-parser
  (:use [parsers.pl-0.parser])
  (:use [clojure.test]))

(deftest pl-0-output
  (testing "output of parsed and evaluated pl-0 source"
    (is (= (with-out-str
             (pl-0 (slurp "src/parsers/pl_0/ex1.pl0")))
           "1\n4\n9\n16\n25\n36\n49\n64\n81\n100\n"))
    (is (= (with-out-str
             (pl-0 (slurp "src/parsers/pl_0/fib.pl0")))
           (str "1\n2\n3\n5\n8\n13\n21\n34\n55\n89\n144\n233\n"
                "377\n610\n987\n1597\n2584\n4181\n6765\n10946\n"
                "17711\n")))))

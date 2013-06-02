(ns ralik.test.json-parser
  (:use [parsers.json.parser])
  (:use [clojure.test]))

(deftest json-test
  (testing "json"
    (is (= (json "[1, 2, 3, 4, 5]")
           [1 2 3 4 5]))
    (is (= (json "[[1, 2], 3, [4, 5]]")
           [[1 2] 3 [4 5]]))
    (is (= (json "[[1, 2], [[[]]], [4, 5]]")
           [[1 2] [[[]]] [4 5]]))
    (is (= (json "[\"1\", \"2\", \"3\", \"4\", \"5\"]")
           ["1" "2" "3" "4" "5"]))
    (is (= (json "
 [1, 22.3,
 -3.4e-3, true, false,
 null, \"foo\",
 [], \"\"]
")
           [1 22.3 -3.4e-3 true false nil "foo" [] ""]))
    (is (= (json "{\"foo\": {}}")
           {:foo {}}))
    (is (= (json "{\"foo\": {\"bar\": []}}")
           {:foo {:bar []}}))
    (is (= (json "{\"foo\": {\"bar\": [
1, 22.3,
 -3.4e-3, true, false,
 null, \"foo\",
 [], \"\"] }}")
           {:foo {:bar [1 22.3 -3.4e-3 true false nil "foo" [] ""]}}))
    (is (= (json "{\"x\": [{}, {}, {}, [\"\\u1d8a\"], [], [{\"y\": [-42]}]]}")
           {:x [{}{}{}["\u1d8a"][][{:y [-42]}]]}))))
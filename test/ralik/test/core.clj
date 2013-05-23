(ns ralik.test.core
  (:use [ralik.core])
  (:import [ralik RalikException])
  (:use [clojure.test]))

(deftest match-test
  (testing "match: \\x, \"foo\", #\"[a-z]\""
    (is (tparse2 "x" \x eoi) "literal character")
    (is (tparse2 "foo" "foo" eoi) "literal string")
    (is (tparse2 "x" "x" eoi) "single character literal string")
    (is (tparse2 "+42" #"[+-]\d+" eoi) "literal regex pattern")))

(deftest wsp-skipper-test
  (testing "wsp-skipper"
    (is (tparse2 "" eoi) "empty input")
    (is (tparse2 " " eoi) "single space")
    (is (tparse2 "	" eoi) "single tab")
    (is (tparse2 "\r" eoi) "single return")
    (is (tparse2 "\n" eoi) "single newline")
    (is (tparse2 "\f" eoi) "single formfeed")
    (is (tparse2 "\f \r\t\f\n\r\r\n\r\r \t\f " eoi) "all whitespace")
    (is (tparse2 "foo " "foo" eoi) "eat trailing whitespace")))

;; g, <g, >g

(deftest g-test
  (testing "g parser"
    (is (tparse2 "" (g eoi)) "empty input")
    (is (tparse2 "xyz" (g \x \y \z) eoi) "sequence")))

(deftest <g-test
  (testing "<g parser"
    (is (= (tparse2 "xyz" (<g 0 \x \y \z)) \x))
    (is (= (tparse2 "xyz" (<g 0 (<g \x \y \z) eoi)) [\x \y \z]))
    (is (= (tparse2 "xyz" (<g 0 (<g 0 \x \y \z) eoi)) \x))
    (is (= (tparse2 "xyz" (<g 0 (<g 1 \x \y \z) eoi)) \y))
    (is (= (tparse2 "xyz" (<g 0 (<g 2 \x \y \z) eoi)) \z))
    (is (= (tparse2 "xyz" (<g 0 (<g [0 1] \x \y \z) eoi)) [\x]))
    (is (= (tparse2 "xyz" (<g 0 (<g [1 2] \x \y \z) eoi)) [\y]))
    (is (= (tparse2 "xyz" (<g 0 (<g [2 3] \x \y \z) eoi)) [\z]))
    (is (= (tparse2 "xyz" (<g 0 (<g [0 2] \x \y \z) eoi)) [\x \y]))
    (is (= (tparse2 "xyz" (<g 0 (<g [1 3] \x \y \z) eoi)) [\y \z]))
    (is (= (tparse2 "xyz" (<g 0 (<g [0 3] \x \y \z) eoi)) [\x \y \z]))))

(deftest >g-test
  (testing ">g parser"
    (is (= (tparse2 "0 1 2" (<g 0 (>g _ _ _ #(vec %&)) eoi))
           [\0 \1 \2]))
    (is (= (tparse2 "0 1 2" (<g 0 (>g 0 _ _ _ #(vec %&)) eoi))
           [\0]))
    (is (= (tparse2 "0 1 2" (<g 0 (>g 1 _ _ _ #(vec %&)) eoi))
           [\1]))
    (is (= (tparse2 "0 1 2" (<g 0 (>g 2 _ _ _ #(vec %&)) eoi))
           [\2]))
    (is (= (tparse2 "0 1 2" (<g 0 (>g [0 1] _ _ _ #(vec %&)) eoi))
           [\0]))
    (is (= (tparse2 "0 1 2" (<g 0 (>g [1 2] _ _ _ #(vec %&)) eoi))
           [\1]))
    (is (= (tparse2 "0 1 2" (<g 0 (>g [2 3] _ _ _ #(vec %&)) eoi))
           [\2]))
    (is (= (tparse2 "0 1 2" (<g 0 (>g [0 2] _ _ _ #(vec %&)) eoi))
           [\0 \1]))
    (is (= (tparse2 "0 1 2" (<g 0 (>g [1 3] _ _ _ #(vec %&)) eoi))
           [\1 \2]))))

;; g*, <g*, >g*

(deftest g*-test
  (testing "g* parser"
    (is (tparse2 "" (g* \x) (g* \y) (g* \z) eoi))
    (is (tparse2 "y" (g* \x) \y eoi))
    (is (tparse2 "xy" (g* \x) \y eoi))
    (is (tparse2 "xxxxxxxxy" (g* \x) \y eoi))))

(deftest <g*-test
  (testing "<g* parser"
    (is (= (tparse2 "" (<g 0 (<g* \x \y \z) eoi))
           []))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g* \x \y \z) eoi))
           [[\x \y \z] [\x \y \z]]))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g* 0 \x \y \z) eoi))
           [\x \x]))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g* 1 \x \y \z) eoi))
           [\y \y]))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g* 2 \x \y \z) eoi))
           [\z \z]))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g* [0 1] \x \y \z) eoi))
           [[\x] [\x]]))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g* [1 2] \x \y \z) eoi))
           [[\y] [\y]]))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g* [2 3] \x \y \z) eoi))
           [[\z] [\z]]))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g* [0 2] \x \y \z) eoi))
           [[\x \y] [\x \y]]))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g* [1 3] \x \y \z) eoi))
           [[\y \z] [\y \z]]))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g* [0 3] \x \y \z) eoi))
           [[\x \y \z] [\x \y \z]]))))

(deftest >g*-test
  (testing ">g* parser"
    (is (= (tparse2 "" (<g 0 (>g* _ #(vec %&)) eoi))
           []))
    (is (= (tparse2 "0 1 2" (<g 0 (>g* _ _ _ #(vec %&)) eoi))
           [[\0 \1 \2]]))
    (is (= (tparse2 "0 1 2" (<g 0 (>g* 0 _ _ _ #(vec %&)) eoi))
           [\0]))
    (is (= (tparse2 "0 1 2" (<g 0 (>g* 1 _ _ _ #(vec %&)) eoi))
           [\1]))
    (is (= (tparse2 "0 1 2" (<g 0 (>g* 2 _ _ _ #(vec %&)) eoi))
           [\2]))
    (is (= (tparse2 "0 1 2" (<g 0 (>g* [0 1] _ _ _ #(vec %&)) eoi))
           [[\0]]))
    (is (= (tparse2 "0 1 2" (<g 0 (>g* [1 2] _ _ _ #(vec %&)) eoi))
           [[\1]]))
    (is (= (tparse2 "0 1 2" (<g 0 (>g* [2 3] _ _ _ #(vec %&)) eoi))
           [[\2]]))
    (is (= (tparse2 "0 1 2" (<g 0 (>g* [0 2] _ _ _ #(vec %&)) eoi))
           [[\0 \1]]))
    (is (= (tparse2 "0 1 2" (<g 0 (>g* [1 3] _ _ _ #(vec %&)) eoi))
           [[\1 \2]]))))

;; g|, <g|, >g|

(deftest g|-test
  (testing "g| parser"
    (is (tparse2 "blue" (g| "green" "yellow" "blue") eoi))
    (is (tparse2 "+42" (g| \+ \-) "42" eoi))
    (is (tparse2 "-42" (g| \+ \-) "42" eoi))
    (is (tparse2 "public interface Gahvah {}"
                 (g| "public" "private") (g| "interface" "class")
                 "Gahvah" \{ \} eoi))
    (is (tparse2 "public class Gahvah {}"
                 (g| "public" "private") (g| "interface" "class")
                 "Gahvah" \{ \} eoi))
    (is (tparse2 "private interface Gahvah {}"
                 (g| "public" "private") (g| "interface" "class")
                 "Gahvah" \{ \} eoi))))

(deftest <g|-test
  (testing "<g| parser"
    (is (= (tparse2 "x" (<g 0 (<g| \x \y \z) eoi))
           \x))))

(deftest >g|-test
  (testing ">g| parser"
    (is (= (tparse2 "1" (<g 0 (>g| \1 \2 \3 #(Integer/parseInt (str %))) eoi))
           1))))

;; g+, <g+, >g+

(deftest g+-test
  (testing "g+ parser"
    (is (tparse2 "xyxyxy" (g+ \x \y) eoi))))

(deftest <g+-test
  (testing "<g+ parser"
    (is (= (tparse2 "" (<g 0 (<g+ \x \y \z) eoi)) ; test for failed match
           nil))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g+ \x \y \z) eoi))
           [[\x \y \z] [\x \y \z]]))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g+ 0 \x \y \z) eoi))
           [\x \x]))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g+ 1 \x \y \z) eoi))
           [\y \y]))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g+ 2 \x \y \z) eoi))
           [\z \z]))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g+ [0 1] \x \y \z) eoi))
           [[\x] [\x]]))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g+ [1 2] \x \y \z) eoi))
           [[\y] [\y]]))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g+ [2 3] \x \y \z) eoi))
           [[\z] [\z]]))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g+ [0 2] \x \y \z) eoi))
           [[\x \y] [\x \y]]))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g+ [1 3] \x \y \z) eoi))
           [[\y \z] [\y \z]]))
    (is (= (tparse2 "xyzxyz" (<g 0 (<g+ [0 3] \x \y \z) eoi))
           [[\x \y \z] [\x \y \z]]))))

(deftest >g+-test
  (testing ">g+ parser"
    (is (= (tparse "x" (<g 0 (>g+ 0 "x" #(Character/toUpperCase %)) eoi))
           \X))
    (is (= (tparse "xxx" (<g 0 (>g+ 0 "x" #(.toUpperCase (apply str %&)))
                             eoi))
           "XXX"))))

;; g?, <g?, >g?

(deftest g?-test
  (testing "g? parser"
    (is (tparse2 "" (g? \x) eoi))
    (is (tparse2 "y" (g? \x) \y eoi))
    (is (tparse2 "xy" (g? \x) \y eoi))
    (is (tparse2 "xxy" \x (g? \x \x) \x \y eoi))
    (is (tparse2 "Hello, Black" "Hello," (g? "Mr.") "Black" eoi))
    (is (tparse2 "Hello , Mr. Black" "Hello" \, (g? "Mr.") "Black" eoi))))

(deftest <g?-test
  (testing "<g? parser"
    (is (= (tparse2 "" (<g 0 (<g? \x) eoi))
           :g?-failed))
    (is (= (tparse2 "" (<g 0 (<g? 0 \x) eoi))
           :g?-failed))
    (is (= (tparse2 "" (<g 0 (<g? [0 1] \x) eoi))
           :g?-failed))
    (is (= (tparse2 "xyz" (<g 0 (<g? \x \y \z) eoi))
           [\x \y \z]))
    (is (= (tparse2 "xz" (<g 0 (<g \x (<g? \y) \z) eoi))
           [\x :g?-failed \z]))))

(deftest >g?-test
  (testing ">g? parser"
    (is (= (tparse "+345" (<g 0 (>g? #"[+-]" {"+" '+
                                             "-" '-
                                             :g?-failed :g?-failed})
                             #"\d+" eoi))
           '+))
    (is (= (tparse "-345" (<g 0 (>g? #"[+-]" {"+" '+
                                             "-" '-
                                             :g?-failed :g?-failed})
                             #"\d+" eoi))
           '-))
    (is (= (tparse "345" (<g 0 (>g? #"[+-]" {"+" '+
                                             "-" '-
                                             :g?-failed :g?-failed})
                             #"\d+" eoi))
           :g?-failed))))

;; g&
;; NOTE: g& is a predicate, so no <g&, or >g& parsers

(deftest g&-test
  (testing "g& parser"
    (is (tparse2 "xyz3_" \x \y \z (g& \3) \3 \_))))

;; g!
;; NOTE: g! is a predicate, so no <g! or >g! parsers

(deftest g!-test
  (testing "g! parser"
    (is (tparse2 "xyz3_" \x \y \z (g! \0) \3 \_))))

;; g-, <g-, >g-

(deftest g--test
  (testing "g- parser"
    (is (tparse2 "a" (g- #"[a-z]" \x)  eoi))
    (is (not (tparse2 "x" (g- #"[a-z]" \x) eoi)))
    (is (tparse2 "sldfkeerw" (g+ (g- _ \q)) eoi) "any character except q")))

(deftest <g--test
  (testing "<g- parser"
    (is (= (tparse2 "x" (<g 0 (<g- _ (g| \a \b \c)) eoi))
           \x))
    (is (= (tparse2 "a" (<g 0 (<g- _ (g| \a \b \c)) eoi))
           nil))))

(deftest >g--test
  (testing ">g- parser"
    (is (= (tparse2 "x" (<g 0 (>g- _ (g| \a \b \c) identity) eoi))
           \x))
    (is (= (tparse2 "a" (<g 0 (>g- _ (g| \a \b \c) identity) eoi))
           nil))))

;; g_, <g_, >g_

(deftest g_-test
  (testing "g_ parser"
    (is (tparse2 "1,1,1,1,1,1" (g_ \1 \,) eoi))
    (is (tparse2 "1,1  ,1,   1,1,1" (g_ \1 \,) eoi))
    (is (tparse2 "x.y.z" (g_ (g| \x \y \z) \.) eoi))))

(deftest <g_-test
  (testing "<g_ parser"
    (is (= (tparse2 "1 + 2234 - 3" (<g 0 (<g_ 0 #"\d+" #"[+-]") eoi))
           ["1" "2234" "3"]))
    (is (= (tparse2 "1 + 2234 - 3" (<g 0 (<g_ 1 #"\d+" #"[+-]") eoi))
           ["+" "-"]))
    (is (= (tparse2 "1 + 2234 - 3" (<g 0 (<g_ #"\d+" #"[+-]") eoi))
           ["1" ["+" "2234"] ["-" "3"]]))))

(deftest >g_-test
  (testing ">g_ parser"
    (is (= (tparse2 "1 + 2234 - 3" (<g 0 (>g_ 0 #"\d+" #"[+-]" vector)
                                       eoi))
           ["1" "2234" "3"]))
    (is (= (tparse2 "1 + 2234 - 3" (<g 0 (>g_ 1 #"\d+" #"[+-]" vector)
                                       eoi))
           ["+" "-"]))
    (is (= (tparse2 "1 + 2234 - 3"
                    (<g 0 (>g_ uint10 (>g #"[+-]" symbol)
                               #(reduce (fn [x [op y]]
                                          (list op x y))
                                        (first %&)
                                        (next %&)))
                        eoi))
           '(- (+ 1 2234) 3)))))

;; g||, <g||, >g||

(deftest g||-test
  (testing "g|| parser"
    (is (tparse2 "123.456" (g|| #"\d+" (g "." #"\d+")) eoi))
    (is (tparse2 "123"  (g|| #"\d+" (g "." #"\d+")) eoi))
    (is (tparse2 ".456"  (g|| #"\d+" (g "." #"\d+")) eoi))))

(deftest <g||-test
  (testing "<g|| parser"
    (is (= (tparse2 "123.456" (<g 0 (<g|| #"\d+" #"\.\d+") eoi))
           ["123" ".456"]))
    (is (= (tparse2 "123" (<g 0 (<g|| #"\d+" #"\.\d+") eoi))
           ["123" :g?-failed]))
    (is (= (tparse2 ".456" (<g 0 (<g|| #"\d+" #"\.\d+") eoi))
           [:g?-failed ".456"]))
    (is (= (tparse2 "123" (<g 0 (<g|| 0 #"\d+" #"\.\d+") eoi))
           "123"))
    (is (= (tparse2 ".456" (<g 0 (<g|| 0 #"\d+" #"\.\d+") eoi))
           :g?-failed))
    (is (= (tparse2 ".456" (<g 0 (<g|| 1 #"\d+" #"\.\d+") eoi))
           ".456"))
    (is (= (tparse2 "123" (<g 0 (<g|| 1 #"\d+" #"\.\d+") eoi))
           :g?-failed))))

(deftest >g||-test
  (testing ">g|| parser"
    (is (= (tparse2 "123.456" (<g 0 (>g|| #"\d+" #"\.\d+" #(vector %1 %2))
                                  eoi))
           ["123" ".456"]))
    (is (= (tparse2 "123" (<g 0 (>g|| #"\d+" #"\.\d+" #(vector %1 %2)) eoi))
           ["123" :g?-failed]))
    (is (= (tparse2 ".456" (<g 0 (>g|| #"\d+" #"\.\d+" #(vector %1 %2)) eoi))
           [:g?-failed ".456"]))
    (is (= (tparse2 "123" (<g 0 (>g|| 0 #"\d+" #"\.\d+" identity) eoi))
           "123"))
    (is (= (tparse2 ".456" (<g 0 (>g|| 0 #"\d+" #"\.\d+" identity) eoi))
           :g?-failed))
    (is (= (tparse2 ".456" (<g 0 (>g|| 1 #"\d+" #"\.\d+" identity) eoi))
           ".456"))
    (is (= (tparse2 "123" (<g 0 (>g|| 1 #"\d+" #"\.\d+" identity) eoi))
           :g?-failed))))

;; rep, >rep, <rep

(deftest rep-test
  (testing "rep parser"
    (is (tparse2 "abcxyz" "abc" (rep 0 \0) "xyz" eoi))
    (is (tparse2 "abcxyz" "abc" (rep {:l 0 :h 5} \0) "xyz" eoi))
    (is (tparse2 "abc0xyz" "abc" (rep {:l 0 :h 5} \0) "xyz" eoi))
    (is (tparse2 "abc00xyz" "abc" (rep {:l 0 :h 5} \0) "xyz" eoi))
    (is (tparse2 "abc000xyz" "abc" (rep {:l 0 :h 5} \0) "xyz" eoi))
    (is (tparse2 "abc0000xyz" "abc" (rep {:l 0 :h 5} \0) "xyz" eoi))
    (is (tparse2 "abc00000xyz" "abc" (rep {:l 0 :h 5} \0) "xyz" eoi))
    (is (tparse2 "01010101" (rep {:l 0 :h 5} \0 \1) eoi) "using & rest args")
    (is (tparse2 "xyxyxy@" (rep {:l 3} "x" "y") "@" eoi) ":h not given")
    (is (tparse2 "@" (rep {:h 3} "x" "y") "@" eoi) ":l not given")
    (is (tparse2 "xy@" (rep {:h 3} "x" "y") "@" eoi) ":l not given")
    (is (tparse2 "xyxy@" (rep {:h 3} "x" "y") "@" eoi) ":l not given")
    (is (tparse2 "xyxyxy@" (rep {:h 3} "x" "y") "@" eoi) ":l not given")
    (is (thrown? RalikException (tparse2 "111" (rep {:l -1 :h 3} \1)))
        "min < 0")
    (is (thrown? RalikException (tparse2 "111" (rep {:l 3 :h 1} \1)))
        "min > max")
    (is (thrown? RalikException (tparse2 "111" (rep "foo" \1)))
        "first arg to rep must be integer or map")
    (is (thrown? RalikException (tparse2 "111" (rep 2.3 \1)))
        "first arg to rep must be integer or map")
    (is (thrown? RalikException (tparse2 "111" (rep #{1 2} \1)))
        "first arg to rep must be integer or map")
    ;; TODO:
    ;; (is (thrown? RalikException (tparse2 "111" (rep {:foo 3} \1)))
    ;;     "only keys :l and :h permitted in first argument to rep")
    ))

(deftest <rep-test
  (testing "<rep parser"
    (is (= (tparse2 "xy" (<g 0 
                             (<rep 0 \x \y \z)
                             (g+ \x \y)
                             eoi))
           []))
    (is (= (tparse2 "xy" (<g [0 2] 
                             (<rep 0 \x \y \z)
                             (<g+ \x \y)
                             eoi))
           [[] [[\x \y]]]))
    (is (= (tparse2 "xyzxyzxy" (<g 0 
                                   (<rep {:l 1 :h 5} \x \y \z)
                                   (g+ \x \y)
                                   eoi))
           [[\x \y \z] [\x \y \z]]))
    (is (= (tparse2 "xyzxyzxy" (<g [0 2] 
                                   (<rep {:l 1 :h 5} \x \y \z)
                                   (<g+ \x \y)
                                   eoi))
           [[[\x \y \z] [\x \y \z]] [[\x \y]]])
        "Check backtrack. On <rep's third iteration it will match \\x and \\y,
       but fail on \\z. Both \\x and \\y will advance *cur-pos*. <rep should
       reset *cur-pos* to the last \\x for subsequent matches.")))

;; prm, <prm, >prm

(deftest prm-test
  (testing "prm parser"
    (is (tparse2 "0" (prm \0 \1) eoi))
    (is (tparse2 "1" (prm \0 \1) eoi))
    (is (tparse2 "010100001" (prm \0 \1) eoi))))

(deftest <prm-test
  (testing "<prm parser"
    (is (= (tparse2 "00001111" (<g 0 (<prm \0 \1) eoi))
           [\0 \0 \0 \0 \1 \1 \1 \1]))
    (is (= (tparse2 "0" (<g 0 (<prm \0 \1) eoi))
           [\0]))
    (is (= (tparse2 "1" (<g 0 (<prm \0 \1) eoi))
           [\1]))
    (is (= (tparse2 "" (<g 0 (<prm \0 \1) eoi))
           nil))
    (is (= (tparse2 "x" (<g 0 (<prm \0 \1) eoi))
           nil))
    (is (= (tparse2 "xyzxyzxyq"
                    (<g [0 2]
                        (<prm (<g \y \z) \x)
                        (<g \y \q)
                        eoi))
           [[\x [\y \z] \x [\y \z] \x] [\y \q]])
        "Check backtrack.")))

;; case-, >case-, <case-

(deftest case--test
  (testing "case- parser"
    (is (tparse2 "FoO" (case- "foo") eoi) "ignore case")
    (is (tparse2 "foo FOO FoO fOO"
                 (case- "FOO"
                        (case+ "FOO"
                               (case- "foo"
                                      (case+ "fOO")))) eoi)
        "nested case-/+")))

;; case+, <case+, >case+

(deftest case+-test
  (testing "case+ parser"
    (is (tparse2 "foo" (case+ "foo") eoi) "match case")
    (is (tparse2 "Foo" (case+ "Foo") eoi) "match case")))

(deftest nested-case+--test
  (testing "nesting case- and case+"
    (is (tparse2 "foo FOO FoO fOO"
                 (case- "FOO"
                        (case+ "FOO"
                               (case- "foo"
                                      (case+ "fOO")))) eoi)
        "nested case-/+")))

;; skip-, <skip-, >skip-

(deftest skip--test
  (testing "skip- parser"
    (is (tparse2 "\n" (skip- "\n") eoi) "skip- to match \\n")
    (is (tparse2 "\r" (skip- "\r") eoi) "skip- to match \\r")
    (is (tparse2 "\n\f" (skip- "\n\f") eoi) "skip- to match \\n\\f")
    (is (tparse2 "
" (skip- "
") eoi) "skip- to match a \\n")))

;; skip+, <skip+, >skip+
;; TODO: ...


(deftest nested-skip-+-test
  (testing "nested skip- and skip+ parsers"
    (is (tparse2 " foo ba r"
                 ;; skip off to match the leading space
                 (skip- " foo"
                        ;; skip back on to eat the leading space
                        (skip+ "ba"
                               ;; skip back off to match the leading space
                               (skip- " r"))) eoi)
        "nesed skip+/-")))

;; <lex, >lex
;; NOTED: The intent of lex is to return a string so there is no lex parser

(deftest <lex-test
  (testing "Return-the-matched-text operator: <lex"
    (is (= (tparse2 "/* foo */" (<lex "/*" (g+ (g- _ "*/")) "*/"))
           "/* foo */"))
    (is (= (tparse2 "/* foo */" (<lex 0 "/*" (g+ (g- _ "*/")) "*/"))
           "/*"))
    (is (= (tparse2 "/* foo */" (<lex 1 "/*" (g+ (g- _ "*/")) "*/"))
           " foo "))
    (is (= (tparse2 "/* foo */" (<lex 2 "/*" (g+ (g- _ "*/")) "*/"))
           "*/"))
    (is (= (tparse2 "/* foo */" (<lex [0 1] "/*" (g+ (g- _ "*/")) "*/"))
           "/*"))
    (is (= (tparse2 "/* foo */" (<lex [1 2] "/*" (g+ (g- _ "*/")) "*/"))
           " foo "))
    (is (= (tparse2 "/* foo */" (<lex [2 3] "/*" (g+ (g- _ "*/")) "*/"))
           "*/"))
    (is (= (tparse2 "/* foo */" (<lex [0 2] "/*" (g+ (g- _ "*/")) "*/"))
           "/* foo "))
    (is (= (tparse2
            "/* foo */" (<lex [1 3] "/*" (g+ (g- _ "*/")) "*/"))
           " foo */"))))

;; kw, <kw, >kw

(deftest <kw-test
  (testing "<kw parser"
    (is (= (tparse2 "float"
                    (<g 0
                        (<kw :float)
                        eoi))
           "float")
        "match float return \"float\"")
    (is (= (tparse2 "FLOAT"
                    (<g 0
                        (case- (<kw :float))
                        eoi))
           "FLOAT")
        "match float return the actual string matched \"FLOAT\"")
    (is (= (tparse2 "float" (<g 0 (<kw "float") eoi)) "float")
        "match with a string")
    (is (= (tparse2 "float" (<g 0 (<kw :float) eoi)) "float")
        "match with a keyword")
    (is (= (tparse2 "float" (<g 0 (<kw float) eoi)) "float")
        "match with a symbol")))

;; kws, <kws, >kws

(deftest <kws-test
  (testing "kws parser"
    (is (= (tparse2 "float" (<g 0 (<kws int char float long) eoi))
           "float"))
    (is (= (tparse2 "float" (<g 0 (<kws :int :char :float :long) eoi))
           "float"))
    (is (= (tparse2 "float" (<g 0 (<kws "int" "char" "float" "long") eoi))
           "float"))))


;; [X] match
;; [X] wsp-skipper
;; [X]  g
;; [X] <g
;; [X] >g
;; [X]  g*
;; [X] <g*
;; [X] >g*
;; [X]  g|
;; [X] <g|
;; [X] >g|
;; [X]  g+
;; [X] <g+
;; [X] >g+
;; [X]  g?
;; [X] <g?
;; [X] >g?
;; [X]  g&
;;     <g&   N/A
;;     >g&   N/A
;; [X]  g!
;;     <g!   N/A
;;     >g!   N/A
;; [X]  g-
;; [X] <g-
;; [X] >g-
;; [X]  g_
;; [X] <g_
;; [X] >g_
;; [X]  g||
;; [ ] <g||
;; [ ] >g||
;; [X]  rep
;; [X] <rep
;; [ ] >rep
;; [X]  prm
;; [X] <prm
;; [ ] >prm
;;      lex   N/A
;; [X] <lex
;; [ ] >lex
;; [ ]  kw
;; [X] <kw
;; [ ] >kw
;; [ ]  kws
;; [X] <kws
;; [ ] >kws
;; [X]  case-
;; [ ] <case-
;; [ ] >case-
;; [X]  case+
;; [ ] <case+
;; [ ] >case+
;; [X]  skip-
;; [ ] <skip-
;; [ ] >skip-
;; [ ]  skip+
;; [ ] <skip+
;; [ ] >skip+

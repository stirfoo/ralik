(ns ralik.test.core
  (:use [ralik.core])
  (:import [ralik RalikException])
  (:use [clojure.test]))

(testing "\\x, \"foo\", #\"[a-z]\", and _ matchers"
  (is (parse "x" \x eoi) "literal character")
  (is (parse "foo" "foo" eoi) "literal string")
  (is (parse "+42" #"[+-]\d+" eoi) "literal regex pattern")
  (is (parse "!@#" _ _ _ eoi) "any-character matcher"))

(testing "+case and -case parsers"
  (is (parse "foo" (+case "foo") eoi) "match case")
  (is (parse "Foo" (+case "Foo") eoi) "match case")
  (is (parse "FoO" (-case "foo") eoi) "ignore case")
  (is (parse "foo FOO FoO fOO"
             (-case "FOO"
                    (+case "FOO"
                           (-case "foo"
                                  (+case "fOO")))) eoi)
      "nested +/-case"))

(testing "wsp-skipper"
  (is (parse "" eoi) "empty input")
  (is (parse " " eoi) "single space")
  (is (parse "	" eoi) "single tab")
  (is (parse "\r" eoi) "single return")
  (is (parse "\n" eoi) "single newline")
  (is (parse "\f" eoi) "single formfeed")
  (is (parse "\f \r\t\f\n\r\r\n\r\r \t\f " eoi) "all whitespace")
  (is (parse "foo " "foo" eoi) "eat trailing whitespace"))

(testing "+skip and -skip parsers"
  (is (parse "\n" (-skip "\n") eoi) "-skip to match \\n")
  (is (parse "\r" (-skip "\r") eoi) "-skip to match \\r")
  (is (parse "\n\f" (-skip "\n\f") eoi) "-skip to match \\n\\f")
  (is (parse "
" (-skip "
") eoi) "-skip to match a \\n")
  (is (parse " foo ba r"
             ;; skip off to match the leading space
             (-skip " foo"
                    ;; skip back on to eat the leading space
                    (+skip "ba"
                           ;; skip back off to match the leading space
                           (-skip " r"))) eoi)
      "nesed +/-skip"))

(testing "g parser"
  (is (parse "" (g eoi)) "empty input")
  (is (parse "xyz" (g \x \y \z) eoi) "sequence"))

(testing "g+ parser"
  (is (parse "xyxyxy" (g+ \x \y) eoi)))

(testing "g* parser"
  (is (parse "" (g* \x) (g* \y) (g* \z) eoi))
  (is (parse "y" (g* \x) \y eoi))
  (is (parse "xy" (g* \x) \y eoi))
  (is (parse "xxxxxxxxy" (g* \x) \y eoi)))

(testing "g? parser"
  (is (parse "" (g? \x) eoi))
  (is (parse "y" (g? \x) \y eoi))
  (is (parse "xy" (g? \x) \y eoi))
  (is (parse "xxy" \x (g? \x \x) \x \y eoi))
  (is (parse "Hello, Black" "Hello," (g? "Mr.") "Black" eoi))
  (is (parse "Hello , Mr. Black" "Hello" \, (g? "Mr.") "Black" eoi)))

(testing "g| parser"
  (is (parse "blue" (g| "green" "yellow" "blue") eoi))
  (is (parse "+42" (g| \+ \-) "42" eoi))
  (is (parse "-42" (g| \+ \-) "42" eoi))
  (is (parse "public interface Gahvah {}"
             (g| "public" "private") (g| "interface" "class")
             "Gahvah" \{ \} eoi))
  (is (parse "public class Gahvah {}"
             (g| "public" "private") (g| "interface" "class")
             "Gahvah" \{ \} eoi))
  (is (parse "private interface Gahvah {}"
             (g| "public" "private") (g| "interface" "class")
             "Gahvah" \{ \} eoi)))

(testing "g& parser"
  (is (parse "xyz3_" \x \y \z (g& \3) \3 \_)))

(testing "g! parser"
  (is (parse "xyz3_" \x \y \z (g! \0) \3 \_)))

(testing "prm parser"
  (is (parse "0" (prm \0 \1) eoi))
  (is (parse "1" (prm \0 \1) eoi))
  (is (parse "010100001" (prm \0 \1) eoi)))

(testing "rep parser"
  (is (parse "abcxyz" "abc" (rep 0 0 \0) "xyz" eoi))
  (is (parse "abcxyz" "abc" (rep 0 5 \0) "xyz" eoi))
  (is (parse "abc0xyz" "abc" (rep 0 5 \0) "xyz" eoi))
  (is (parse "abc00xyz" "abc" (rep 0 5 \0) "xyz" eoi))
  (is (parse "abc000xyz" "abc" (rep 0 5 \0) "xyz" eoi))
  (is (parse "abc0000xyz" "abc" (rep 0 5 \0) "xyz" eoi))
  (is (parse "abc00000xyz" "abc" (rep 0 5 \0) "xyz" eoi))
  (is (parse "01010101" (rep 0 5 \0 \1) eoi) "using & rest args")
  (is (thrown? RalikException (parse "111" (rep -1 3 \1)))
      "min < 0")
  (is (thrown? RalikException (parse "111" (rep 3 1 \1)))
      "min > max"))

(testing "g_ parser"
  (is (parse "1,1,1,1,1,1" (g_ \1 \,) eoi))
  (is (parse "1,1  ,1,   1,1,1" (g_ \1 \,) eoi))
  (is (parse "x.y.z" (g_ (g| \x \y \z) \.) eoi)))

(testing "g- parser"
  (is (parse "sldfkeerw" (g+ (g- _ \q)) eoi) "any character except q"))

(testing "Return-the-matched-text operator: <lex"
  (is (= (parse "/* foo */" (<lex "/*" (g+ (g- _ "*/")) "*/"))
         "/* foo */"))
  (is (= (parse "/* foo */" (<lex 0 "/*" (g+ (g- _ "*/")) "*/"))
         "/*"))
  (is (= (parse "/* foo */" (<lex 1 "/*" (g+ (g- _ "*/")) "*/"))
         " foo "))
  (is (= (parse "/* foo */" (<lex 2 "/*" (g+ (g- _ "*/")) "*/"))
         "*/"))
  (is (= (parse "/* foo */" (<lex [0 1] "/*" (g+ (g- _ "*/")) "*/"))
         "/*"))
  (is (= (parse "/* foo */" (<lex [1 2] "/*" (g+ (g- _ "*/")) "*/"))
         " foo "))
  (is (= (parse "/* foo */" (<lex [2 3] "/*" (g+ (g- _ "*/")) "*/"))
         "*/"))
  (is (= (parse "/* foo */" (<lex [0 2] "/*" (g+ (g- _ "*/")) "*/"))
         "/* foo "))
  (is (= (parse
          "/* foo */" (<lex [1 3] "/*" (g+ (g- _ "*/")) "*/"))
         " foo */"))
  ;; XXX: will not catch this one even though RalikException is thrown
  ;; (is (thrown? RalikException (parse "xyz" (<lex -1 \x \y \z)))
  ;;     "index < 0")
  )

(testing "<g parser"
  (is (= (parse "xyz" (<g 0 \x \y \z)) \x))
  (is (= (parse "xyz" (<g 0
                          (<g \x \y \z) ; this form produces the result
                          eoi))
         [\x \y \z]))
  (is (= (parse "xyz" (<g 0 (<g 0 \x \y \z) eoi)) \x))
  (is (= (parse "xyz" (<g 0 (<g 1 \x \y \z) eoi)) \y))
  (is (= (parse "xyz" (<g 0 (<g 2 \x \y \z) eoi)) \z))
  (is (= (parse "xyz" (<g 0 (<g [0 1] \x \y \z) eoi)) [\x]))
  (is (= (parse "xyz" (<g 0 (<g [1 2] \x \y \z) eoi)) [\y]))
  (is (= (parse "xyz" (<g 0 (<g [2 3] \x \y \z) eoi)) [\z]))
  (is (= (parse "xyz" (<g 0 (<g [0 2] \x \y \z) eoi)) [\x \y]))
  (is (= (parse "xyz" (<g 0 (<g [1 3] \x \y \z) eoi)) [\y \z]))
  (is (= (parse "xyz" (<g 0 (<g [0 3] \x \y \z) eoi)) [\x \y \z])))

(testing "<g* parser"
  (is (= (parse "" (<g 0 (<g* \x \y \z) eoi))
         []))
  (is (= (parse "xyzxyz" (<g 0 (<g* \x \y \z) eoi))
         [[\x \y \z] [\x \y \z]]))
  (is (= (parse "xyzxyz" (<g 0 (<g* 0 \x \y \z) eoi))
         [\x \x]))
  (is (= (parse "xyzxyz" (<g 0 (<g* 1 \x \y \z) eoi))
         [\y \y]))
  (is (= (parse "xyzxyz" (<g 0 (<g* 2 \x \y \z) eoi))
         [\z \z]))
  (is (= (parse "xyzxyz" (<g 0 (<g* [0 1] \x \y \z) eoi))
         [[\x] [\x]]))
  (is (= (parse "xyzxyz" (<g 0 (<g* [1 2] \x \y \z) eoi))
         [[\y] [\y]]))
  (is (= (parse "xyzxyz" (<g 0 (<g* [2 3] \x \y \z) eoi))
         [[\z] [\z]]))
  (is (= (parse "xyzxyz" (<g 0 (<g* [0 2] \x \y \z) eoi))
         [[\x \y] [\x \y]]))
  (is (= (parse "xyzxyz" (<g 0 (<g* [1 3] \x \y \z) eoi))
         [[\y \z] [\y \z]]))
  (is (= (parse "xyzxyz" (<g 0 (<g* [0 3] \x \y \z) eoi))
         [[\x \y \z] [\x \y \z]])))

(testing "<g+ parser"
  (is (= (parse2 "" (<g 0 (<g+ \x \y \z) eoi)) ; test for failed match
         nil))
  (is (= (parse "xyzxyz" (<g 0 (<g+ \x \y \z) eoi))
         [[\x \y \z] [\x \y \z]]))
  (is (= (parse "xyzxyz" (<g 0 (<g+ 0 \x \y \z) eoi))
         [\x \x]))
  (is (= (parse "xyzxyz" (<g 0 (<g+ 1 \x \y \z) eoi))
         [\y \y]))
  (is (= (parse "xyzxyz" (<g 0 (<g+ 2 \x \y \z) eoi))
         [\z \z]))
  (is (= (parse "xyzxyz" (<g 0 (<g+ [0 1] \x \y \z) eoi))
         [[\x] [\x]]))
  (is (= (parse "xyzxyz" (<g 0 (<g+ [1 2] \x \y \z) eoi))
         [[\y] [\y]]))
  (is (= (parse "xyzxyz" (<g 0 (<g+ [2 3] \x \y \z) eoi))
         [[\z] [\z]]))
  (is (= (parse "xyzxyz" (<g 0 (<g+ [0 2] \x \y \z) eoi))
         [[\x \y] [\x \y]]))
  (is (= (parse "xyzxyz" (<g 0 (<g+ [1 3] \x \y \z) eoi))
         [[\y \z] [\y \z]]))
  (is (= (parse "xyzxyz" (<g 0 (<g+ [0 3] \x \y \z) eoi))
         [[\x \y \z] [\x \y \z]])))

(testing "<g? parser"
  (is (= (parse "" (<g 0 (<g? \x) eoi))
         :g?-failed))
  (is (= (parse "" (<g 0 (<g? 0 \x) eoi))
         :g?-failed))
  (is (= (parse "" (<g 0 (<g? [0 1] \x) eoi))
         :g?-failed))
  (is (= (parse "xyz" (<g 0 (<g? \x \y \z) eoi))
         [\x \y \z]))
  (is (= (parse "xz" (<g 0 (<g \x (<g? \y) \z) eoi))
         [\x :g?-failed \z])))

(testing "<g| parser"
  (is (= (parse "x" (<g 0 (<g| \x \y \z) eoi))
         \x)))

(testing "<g- parser"
  (is (= (parse "x" (<g 0 (<g- _ (g| \a \b \c)) eoi))
         \x))
  (is (= (parse2 "a" (<g 0 (<g- _ (g| \a \b \c)) eoi))
         nil)))

(testing "<g_ parser"
  (is (= (parse "1 + 2234 - 3" (<g 0 (<g_ 0 #"\d+" #"[+-]") eoi))
         ["1" "2234" "3"]))
  (is (= (parse "1 + 2234 - 3" (<g 0 (<g_ 1 #"\d+" #"[+-]") eoi))
         ["+" "-"]))
  (is (= (parse "1 + 2234 - 3" (<g 0 (<g_ #"\d+" #"[+-]") eoi))
         ["1" ["+" "2234"] ["-" "3"]])))

(testing "<prm parser"
  (is (= (parse "00001111" (<g 0 (<prm \0 \1) eoi))
         [\0 \0 \0 \0 \1 \1 \1 \1]))
  (is (= (parse "0" (<g 0 (<prm \0 \1) eoi))
         [\0]))
  (is (= (parse "1" (<g 0 (<prm \0 \1) eoi))
         [\1]))
  (is (= (parse2 "" (<g 0 (<prm \0 \1) eoi))
         nil))
  (is (= (parse2 "x" (<g 0 (<prm \0 \1) eoi))
         nil))
  (is (= (parse "xyzxyzxyq"
                (<g [0 2]
                    (<prm (<g \y \z) \x)
                    (<g \y \q)
                    eoi))
         [[\x [\y \z] \x [\y \z] \x] [\y \q]])
      "Check backtrack."))

(testing "<rep parser"
  (is (= (parse "xy" (<g 0 
                         (<rep 0 0 \x \y \z)
                         (g+ \x \y)
                         eoi))
         []))
  (is (= (parse "xy" (<g [0 2] 
                         (<rep 0 0 \x \y \z)
                         (<g+ \x \y)
                         eoi))
         [[] [[\x \y]]]))
  (is (= (parse "xyzxyzxy" (<g 0 
                               (<rep 1 5 \x \y \z)
                               (g+ \x \y)
                               eoi))
         [[\x \y \z] [\x \y \z]]))
  (is (= (parse "xyzxyzxy" (<g [0 2] 
                               (<rep 1 5 \x \y \z)
                               (<g+ \x \y)
                               eoi))
         [[[\x \y \z] [\x \y \z]] [[\x \y]]])
      "Check backtrack. On <rep's third iteration it will match \\x and \\y,
       but fail on \\z. Both \\x and \\y will advance *cur-pos*. <rep should
       reset *cur-pos* to the last \\x for subsequent matches."))

(testing "<kw parser"
  (is (= (parse "float"
                (<g 0
                    (<kw :float)
                    eoi))
         "float")
      "match float return \"float\"")
  (is (= (parse "FLOAT"
                (<g 0
                    (-case (<kw :float))
                    eoi))
         "FLOAT")
      "match float return the actual string matched \"FLOAT\"")
  (is (= (parse "float" (<g 0 (<kw "float") eoi)) "float")
      "match with a string")
  (is (= (parse "float" (<g 0 (<kw :float) eoi)) "float")
      "match with a keyword")
  (is (= (parse "float" (<g 0 (<kw float) eoi)) "float")
      "match with a symbol"))

(testing "kws parser"
  (is (= (parse "float" (<g 0 (<kws int char float long) eoi))
         "float")))

(testing ">g parser"
  (is (= (parse "0 1 2" (<g 0 (>g _ _ _ #(vec %&)) eoi))
         [\0 \1 \2]))
  (is (= (parse "0 1 2" (<g 0 (>g 0 _ _ _ #(vec %&)) eoi))
         [\0]))
  (is (= (parse "0 1 2" (<g 0 (>g 1 _ _ _ #(vec %&)) eoi))
         [\1]))
  (is (= (parse "0 1 2" (<g 0 (>g 2 _ _ _ #(vec %&)) eoi))
         [\2]))
  (is (= (parse "0 1 2" (<g 0 (>g [0 1] _ _ _ #(vec %&)) eoi))
         [\0]))
  (is (= (parse "0 1 2" (<g 0 (>g [1 2] _ _ _ #(vec %&)) eoi))
         [\1]))
  (is (= (parse "0 1 2" (<g 0 (>g [2 3] _ _ _ #(vec %&)) eoi))
         [\2]))
  (is (= (parse "0 1 2" (<g 0 (>g [0 2] _ _ _ #(vec %&)) eoi))
         [\0 \1]))
  (is (= (parse "0 1 2" (<g 0 (>g [1 3] _ _ _ #(vec %&)) eoi))
         [\1 \2])))

(testing ">g* parser"
  (is (= (parse "" (<g 0 (>g* _ #(vec %&)) eoi))
         []))
  (is (= (parse "0 1 2" (<g 0 (>g* _ _ _ #(vec %&)) eoi))
         [[\0 \1 \2]]))
  (is (= (parse "0 1 2" (<g 0 (>g* 0 _ _ _ #(vec %&)) eoi))
         [\0]))
  (is (= (parse "0 1 2" (<g 0 (>g* 1 _ _ _ #(vec %&)) eoi))
         [\1]))
  (is (= (parse "0 1 2" (<g 0 (>g* 2 _ _ _ #(vec %&)) eoi))
         [\2]))
  (is (= (parse "0 1 2" (<g 0 (>g* [0 1] _ _ _ #(vec %&)) eoi))
         [[\0]]))
  (is (= (parse "0 1 2" (<g 0 (>g* [1 2] _ _ _ #(vec %&)) eoi))
         [[\1]]))
  (is (= (parse "0 1 2" (<g 0 (>g* [2 3] _ _ _ #(vec %&)) eoi))
         [[\2]]))
  (is (= (parse "0 1 2" (<g 0 (>g* [0 2] _ _ _ #(vec %&)) eoi))
         [[\0 \1]]))
  (is (= (parse "0 1 2" (<g 0 (>g* [1 3] _ _ _ #(vec %&)) eoi))
         [[\1 \2]])))
(ns ralik.test.core
  (:use [ralik.core])
  (:use [clojure.test]))

(comment
  Parsers
  -------
  g g+ g* g? g| g& g!
  g< g> g_ g-
  lex kw kws
  +skip -skip +case -case
  wsp blank eoi eol _)

(testing "Skipper (wsp-skipper)"
  (is (parse "" eoi) "empty input")
  (is (parse " " eoi) "single space")
  (is (parse "	" eoi) "single tab")
  (is (parse "\r" eoi) "single return")
  (is (parse "\n" eoi) "single newline")
  (is (parse "\f" eoi) "single formfeed")
  (is (parse "\f \r\t\f\n\r\r\n\r\r \t\f " eoi) "all whitespace")
  (is (parse "foo " "foo" eoi) "eat trailing whitespace"))

(testing "Skip state (+skip, -skip)"
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

(testing "Case state (+case, -case)"
  (is (parse "foo" (+case "foo") eoi) "match case")
  (is (parse "Foo" (+case "Foo") eoi) "match case")
  (is (parse "FoO" (-case "foo") eoi) "ignore case")
  (is (parse "foo FOO FoO fOO"
             (-case "FOO"
                    (+case "FOO"
                           (-case "foo"
                                  (+case "fOO")))) eoi)
      "nested +/-case"))

(testing
    "Group operator (g). parse uses an implicit g after the text to match.
So pretty much any test will test g."
  (is (parse "" (g eoi)) "empty input")
  (is (parse "xyz" (g \x \y \z) eoi) "empty input"))

(testing "One or more operator (g+)"
  (is (parse "xyxyxy" (g+ \x \y) eoi)))

(testing "Zero or more operator (g*)"
  (is (parse "y" (g* \x) \y eoi))
  (is (parse "xy" (g* \x) \y eoi))
  (is (parse "xxxxxxxxy" (g* \x) \y eoi)))

(testing "Zero or one operator (g?)"
  (is (parse "y" (g? \x) \y eoi))
  (is (parse "xy" (g? \x) \y eoi))
  (is (parse "xxy" \x (g? \x \x) \x \y eoi))
  (is (parse "Hello, Black" "Hello," (g? "Mr.") "Black" eoi))
  (is (parse "Hello , Mr. Black" "Hello" \, (g? "Mr.") "Black" eoi)))

(testing "Alternate operator (g|)"
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

(testing "Positive look ahead operator (g&)"
  (is (parse "xyz3_" \x \y \z (g& \3) \3 \_)))

(testing "Negative look ahead operator (g!)"
  (is (parse "xyz3_" \x \y \z (g! \0) \3 \_)))

(testing "Match-at-least-one-of-these-in-any-order operator (g<)"
  (is (parse "0" (g< \0 \1) eoi))
  (is (parse "1" (g< \0 \1) eoi))
  (is (parse "010100001" (g< \0 \1) eoi)))

(testing "Match-N-to-M-times-inclusive operator (g>)"
  (is (parse "abcxyz" "abc" (g> 0 5 \0) "xyz" eoi))
  (is (parse "abc0xyz" "abc" (g> 0 5 \0) "xyz" eoi))
  (is (parse "abc00xyz" "abc" (g> 0 5 \0) "xyz" eoi))
  (is (parse "abc000xyz" "abc" (g> 0 5 \0) "xyz" eoi))
  (is (parse "abc0000xyz" "abc" (g> 0 5 \0) "xyz" eoi))
  (is (parse "abc00000xyz" "abc" (g> 0 5 \0) "xyz" eoi))
  (is (parse "01010101" (g> 0 5 \0 \1) eoi) "using & rest args"))

(testing "Match-this-separated-by-that operator (g_)"
  (is (parse "1,1,1,1,1,1" (g_ \1 \,) eoi))
  (is (parse "1,1  ,1,   1,1,1" (g_ \1 \,) eoi))
  (is (parse "x.y.z" (g_ (g| \x \y \z) \.) eoi)))

;; TODO: better test/use for this
(testing "Match-this-but-not-that operator (g-)"
  (is (parse "blue seafood" (g- "blue" "bluefin") "seafood" eoi)))

(testing "Return-the-matched-text operator (lex)"
  (is (let [res (atom nil)]
        (parse "lambda" (reset! res (lex "lambda")) eoi)
        (= @res "lambda"))))

(testing "Match-only-this-keyword operator (kw)"
  (is (parse "nil" (kw :nil :nil-kw) eoi) "match nil but return :nil-kw")
  (is (parse "false" (kw :false :false) eoi) "match false but return :false"))

(testing "Match-one-of-these-keywords operator (kws)"
  (is (parse "
float foo = 2.044;
double bar = 234.30230002;
int baaz = 42;
long cycles = 233422234234234234234234;"
             (g+ (kws :int :long :float :double)
                 #"[a-z]+" \= #"\d+(\.\d*)?" \;)
             eoi)
      "using regex for identifiers and numbers"))

(testing "Atomic parsers (wsp, blank, eoi (already tested), eol, _)"
  (is (parse "     " (-skip (g+ wsp)) eoi))
  (is (parse "     " (-skip (g+ blank)) eoi))
  (is (parse "1. Ohio
2. Russia
3. Hawaii
4. Germany
5. Anarctica" (-skip (g_ (g #"\d" \. blank #"[a-zA-Z]+") eol)) eoi))
  (is (parse "_ will match any character" (g+ _) eoi)))

(is (= (let [r (atom nil)]
         (parse "a b c d e, f, foo(h i) j bOiNg! bOiNg! bOiNg! 11010101"
                (reset!
                 r (lex (+skip (g+ \a)                         ; a
                               (g* (g| \b \c \d))              ; b c d
                               (g! eoi)                        ; not yet!
                               (g_ (g| (kw foo) #"[a-z]") \,)  ; e, f, foo
                               (g \( (g& \h) \h (g& \i) \i \)) ; (h i)
                               \j                              ; j
                               (g> 1 3 (+case (kw bOiNg!)))    ; bOiNg! * 3
                               (g< \0 \1)                      ; 11010101
                               )))
                eoi
                @r))
       "a b c d e, f, foo(h i) j bOiNg! bOiNg! bOiNg! 11010101"))


(is (= (parse "(1, 2, 3234, 4, 5229384)"
              (<2g \((>g_ (lex #"[0-9]+") \,
                          #(map (fn [x] (Integer/parseInt x))
                                %&)) \)))
       [1 2 3234 4 5229384])
    "test <2g and >g_")
;;; expression.clj
;;;
;;; Sunday, April  8 2012

(ns ^{:doc "Test Java expression parser"}
  ralik.parsers.java.expression
  (:use ralik.core)
  (:use [clojure.test :only [is]]))

;; ?: is the node (ternary e1 e2 e3)

(def bop->node
  {"=" 'j-ass
   "*=" 'j-mul-ass
   "/=" 'j-div-ass
   "%=" 'j-mod-ass
   "|=" 'j-bit-or-ass
   "^=" 'j-bit-xor-ass
   "&=" 'j-bit-and-ass
   "+=" 'j-add-ass
   "-=" 'j-sub-ass
   "<<=" 'j-sls-ass
   ">>=" 'j-srs-ass
   ">>>=" 'j-urs-ass
   "||" 'j-or
   "&&" 'j-and
   "|" 'j-bit-or
   "^" 'j-bit-xor
   "&" 'j-bit-and
   "==" 'j-eq
   "!=" 'j-neq
   ">" 'j-gt
   ">=" 'j-ge
   "<" 'j-lt
   "<=" 'j-le
   "instanceof" 'j-instof
   "<<" 'j-sls
   ">>" 'j-srs
   ">>>" 'j-urs
   "+" 'j-add
   "-" 'j-sub
   "*" 'j-mul
   "/" 'j-div
   "%" 'j-mod})

(def uop->node
  { "+" 'j-uadd
    "-" 'j-usub
    "!" 'j-not
    "~" 'j-bit-not})

(def preop->node
  {"++" 'j-pre-inc
   "--" 'j-pre-dec})

(def postop->node
  {"++" 'j-post-inc
   "--" 'j-post-dec})

(defgrammar java-expression
  "Parse Java expression, return an AST." 
  [:print-err? true
   :trace? false]
  ;;
  (start
   (<1g (expression) eoi))
  ;; 
  (expression
   (assignment-expression))
  ;; 
  (assignment-expression
   (g| (assignment)
       (conditional-expression)))
  ;; x = y, x += y, etc., right-associative
  (assignment
   (>g (left-hand-side)
       (assignment-operator)
       (assignment-expression)
       #(list (bop->node %2) %1 %3)))
  ;;
  (left-hand-side
   (g| (qualified-name)
       ;; (field-access)
       ;; (array-access)
       ))
  ;; foo or foo.bar.baaz
  (qualified-name
   (>g_ (identifier) \.
        #(symbol (apply str (interpose \. %&)))))
  ;; 
  (assignment-operator
    (g| "=" "*=" "/=" "%=" "+=" "-=" "<<=" ">>>=" ">>=" "&=" "^=" "|="))
  ;; ?:, right-associative
  (conditional-expression
   (>g (conditional-or-expression)
       (g? (<g \? (expression) \: (conditional-expression)))
       #(if (= %2 :g?-failed)
          %1
          (list 'ternary %1 (%2 1) (%2 3)))))
  ;; a || b -- left-associative
  (conditional-or-expression
   (>g_ (conditional-and-expression) "||"
        #(reduce (fn [x y]
                   (list (bop->node "||") x y))
                 %&)))
  ;; a && b -- left-associative
  (conditional-and-expression
   (>g_ (inclusive-or-expression) "&&"
        #(reduce (fn [x y]
                   (list (bop->node "&&") x y))
                 %&)))
  ;; x | y -- left-associative
  (inclusive-or-expression
   (>g_ (exclusive-or-expression) \|
        #(reduce (fn [x y]
                   (list (bop->node "|") x y))
                 %&)))
  ;; x ^ y -- left-associative
  (exclusive-or-expression
   (>g_ (and-expression) \^
        #(reduce (fn [x y]
                   (list (bop->node "^") x y))
                 %&)))
  ;; x & y -- left-associative
  (and-expression
   (>g_ (equality-expression) \&
        #(reduce (fn [x y]
                   (list (bop->node "&") x y))
                 %&)))
  ;; x == y, x != y -- left-associative
  (equality-expression
   (>g (relational-expression)
       (<g* (g| "==" "!=") (relational-expression))
       #(if (empty? %2)
          %1
          (reduce (fn [x [op y]]
                    (list (bop->node op) x y))
                  %1 (partition 2 %2)))))
  ;; x <= y, x < y, x >= y, x > y, x instanceof y -- left-associative
  (relational-expression
   (>g (shift-expression)
       (<g* (g| (<g (g| "<=" \< ">=" \>) (shift-expression))
                (<g (kw :instanceof "instanceof") (identifier))))
       #(if (empty? %2)
          %1
          (reduce (fn [x [op y]]
                    ;; str for \< and \>
                    (list (bop->node (str op)) x y))
                  %1 %2))))
  ;; x << y, x >>> y, x >> y -- left-associative
  (shift-expression
   (>g (additive-expression)
       (<g* (g| "<<" ">>>" ">>") (additive-expression))
       #(if (empty? %2)
          %1
          (reduce (fn [x [op y]]
                    (list (bop->node op) x y))
                  %1 (partition 2 %2)))))
  ;; x + y, x - y -- left-associative
  (additive-expression
   (>g (multiplicative-expression)
       (<g* (g| \+ \-) (multiplicative-expression))
       #(if (empty? %2)
          %1
          (reduce (fn [x [op y]]
                    (list (bop->node (str op)) x y))
                  %1 (partition 2 %2)))))
  ;; x * y, x / y, x % y -- left-associative
  (multiplicative-expression
   (>g (unary-expression)
       (<g* (g| \* \/ \%) (unary-expression))
       #(if (empty? %2)
          %1
          (reduce (fn [x [op y]]
                    (list (bop->node (str op)) x y))
                  %1 (partition 2 %2)))))
  ;; ++x, --x, +x, -x, x++, x--, ~x, !x -- right-associative
  (unary-expression
   (g| (pre-increment-expression)
       (pre-decrement-expression)
       (>g (g| \+ \-) (unary-expression)
           #(list (uop->node (str %1)) %2))
       (unary-expression-not-plus-minus)))
  ;; ++x -- right-associative
  (pre-increment-expression
   (>2g "++" (unary-expression)
        #(list (preop->node "++") %)))
  ;; --x -- right-associative
  (pre-decrement-expression
   (>2g "--" (unary-expression)
        #(list (preop->node "--") %)))
  ;; x++, x++, ~x, !x -- right-associative
  (unary-expression-not-plus-minus
   (g| (postfix-expression)
       (>g (g| \~ \!) (unary-expression)
           #(list (uop->node (str %1)) %2))
       ;; (cast-expression)
       ))
  ;; x++, x-- -- right-associative
  (postfix-expression
   (>g (g| (primary)
           (qualified-name))
       (g? (g| "++" "--"))
       #(if (= %2 :g?-failed)
          %1
          (list (postop->node %2) %1))))
  ;; 
  (primary
   (g| (primary-no-new-array)
       ;; (array-creation-expression)
       ))
  ;;
  (primary-no-new-array
   (g| (literal)
       (<2g \( (expression) \))
       ;; (g (java-type) \. (kw :class))
       ;; (g (kw :void) \. (kw :class))
       ;; (g (class-name) \. (kw :this))
       ;; (class-instance-creation-expression)
       ;; (field-access)
       ;; (method-invocation)
       ;; (array-access)
       ))
  ;; 
  (literal
   (>lex #"[0-9]+" Integer/parseInt))
  ;; 
  (identifier
   (>lex #"[a-z]+" symbol)))

(defn test-it
  []
  ;; primary
  (is (java-expression "x"))
  ;; unary-expression
  (is (java-expression "x++"))
  (is (java-expression "x--"))
  (is (java-expression "!x"))
  (is (java-expression "~x"))
  (is (java-expression "-x"))
  (is (java-expression "+x"))
  (is (java-expression "--x"))
  (is (java-expression "++x"))
  ;; multiplicative-expression
  (is (java-expression "x * y"))
  (is (java-expression "x / y"))
  (is (java-expression "x % y"))
  (is (java-expression "a * b / c % d"))
  ;; additive-expression
  (is (java-expression "x + y"))
  (is (java-expression "x - y"))
  (is (java-expression "a + b - c"))
  ;; shift-expression
  (is (java-expression "x << y"))
  (is (java-expression "x >>> y"))
  (is (java-expression "x >> y"))
  ;; relational-expression
  (is (java-expression "x <= y"))
  (is (java-expression "x < y"))
  (is (java-expression "x >= y"))
  (is (java-expression "x > y"))
  (is (java-expression "x instanceof y"))
  ;; equality-expression
  (is (java-expression "x == y"))
  (is (java-expression "x != y"))
  ;; and-expression
  (is (java-expression "x & y"))
  ;; inclusive-or-expression
  (is (java-expression "x ^ y"))
  ;; exclusive-or-expression
  (is (java-expression "x | y"))
  ;; conditional-and-expression
  (is (java-expression "x && y"))
  ;; conditional-or-expression
  (is (java-expression "x || y"))
  ;; conditional-expression
  (is (java-expression "x ? y : z"))
  ;; assignment
  (is (java-expression "a = b"))
  (is (java-expression "a = b = c = d"))
  (is (java-expression "a = b *= c"))
  (is (java-expression "a = b /= c"))
  (is (java-expression "a = b %= c"))
  (is (java-expression "a = b += c"))
  (is (java-expression "a = b -= c"))
  (is (java-expression "a = b <<= c"))
  (is (java-expression "a = b >>>= c"))
  (is (java-expression "a = b >>= c"))
  (is (java-expression "a = b &= c"))
  (is (java-expression "a = b ^= c"))
  (is (not (java-expression "q = a ==  c = b")))
  (is (java-expression      "q = a == (c = b)"))
)

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
   (let [ret (atom nil)]
     (g (reset! ret (expression)) eoi @ret)))
  ;; 
  (expression
   (assignment-expression))
  ;; 
  (assignment-expression
   (g| (assignment)
       (conditional-expression)))
  ;; x = y, x += y, etc., right-associative
  (assignment
   (let [lhs (atom nil)
         op (atom nil)]
     (g (reset! lhs (left-hand-side))
        (reset! op (assignment-operator))
        (awhen (assignment-expression)
          #(list @op @lhs %)))))
  ;;
  (left-hand-side
   (g| (qualified-name)
       ;; (field-access)
       ;; (array-access)
       ))
  ;; foo or foo.bar.baaz
  (qualified-name
   (let [ids (atom [])]
     (g (g_ (awhen (identifier)
              #(swap! ids conj %))
            \.)
        (symbol (apply str (map str (interpose \. @ids)))))))
  ;; 
  (assignment-operator
   (bop->node
    (lex (g| \= "*=" "/=" "%=" "+=" "-=" "<<=" ">>>=" ">>=" "&=" "^=" "|="))))
  ;; ?:, right-associative
  (conditional-expression
   (let [o1 (atom nil)
         o2 (atom nil)
         o3 (atom nil)]
     (g (reset! o1 (conditional-or-expression))
        (g? \? (reset! o2 (expression))
            \: (reset! o3 (conditional-expression)))
        (if @o3
          (list 'ternary @o1 @o2 @o3)
          @o1))))
  ;; a || b -- left-associative
  (conditional-or-expression
   (let [es (atom [])]
     (g (g_ (awhen (conditional-and-expression)
              #(swap! es conj %))
            "||")
        (reduce #(list (bop->node "||") %1 %2)
                @es))))
  ;; a && b -- left-associative
  (conditional-and-expression
   (let [es (atom [])]
     (g (g_ (awhen (inclusive-or-expression)
              #(swap! es conj %))
            "&&")
        (reduce #(list (bop->node "&&") %1 %2)
                @es))))
  ;; x | y -- left-associative
  (inclusive-or-expression
   (let [es (atom [])]
     (g (g_ (awhen (exclusive-or-expression)
              #(swap! es conj %))
            \|)
        (reduce #(list (bop->node "|") %1 %2)
                @es))))
  ;; x ^ y -- left-associative
  (exclusive-or-expression
   (let [es (atom [])]
     (g (g_ (awhen (and-expression)
              #(swap! es conj %))
            \^)
        (reduce #(list (bop->node "^") %1 %2)
                @es))))
  ;; x & y -- left-associative
  (and-expression
   (let [es (atom [])]
     (g (g_ (awhen (equality-expression)
              #(swap! es conj %))
            \&)
        (reduce #(list (bop->node "&") %1 %2)
                @es))))
  ;; x == y, x != y -- left-associative
  (equality-expression
   (let [es (atom nil)
         op (atom nil)]
     (g (reset! es (relational-expression))
        (g* (reset! op (lex (g| "==" "!=")))
            (awhen (relational-expression)
              #(swap! es (fn [old-es] (list (bop->node @op) old-es %))))))
     @es))
  ;; x <= y, x < y, x >= y, x > y, x instanceof y -- left-associative
  (relational-expression
   (let [es (atom nil)
         op (atom nil)]
     (g (reset! es (shift-expression))
        (g* (g| (g (reset! op (lex (g| "<=" \< ">=" \>)))
                   (awhen (shift-expression)
                     #(swap! es (fn [old-es]
                                  (list (bop->node @op) old-es %)))))
                (g (reset! op (kw :instanceof "instanceof"))
                   (awhen (identifier)
                     #(swap! es (fn [old-es]
                                  (list (bop->node @op) old-es %))))
                   ;; (reference-type)
                   ))))
     @es))
  ;; x << y, x >>> y, x >> y -- left-associative
  (shift-expression
   (let [es (atom nil)
         op (atom nil)]
     (g (reset! es (additive-expression))
        (g* (reset! op (lex (g| "<<" ">>>" ">>")))
            (awhen (additive-expression)
              #(swap! es (fn [old-es] (list (bop->node @op) old-es %))))))
     @es))
  ;; x + y, x - y -- left-associative
  (additive-expression
   (let [es (atom nil)
         op (atom nil)]
     (g (reset! es (multiplicative-expression))
        (g* (reset! op (lex (g| \+ \-)))
            (awhen (multiplicative-expression)
              #(swap! es (fn [old-es] (list (bop->node @op) old-es %))))))
     @es))
  ;; x * y, x / y, x % y -- left-associative
  (multiplicative-expression
   (let [es (atom nil)
         op (atom nil)]
     (g (reset! es (unary-expression))
        (g* (reset! op (lex (g| \* \/ \%)))
            (awhen (unary-expression)
              #(swap! es (fn [old-es] (list (bop->node @op) old-es %))))))
     @es))
  ;; ++x, --x, +x, -x, x++, x--, ~x, !x -- right-associative
  (unary-expression
   (let [op (atom nil)]
     (g| (pre-increment-expression)
         (pre-decrement-expression)
         (g (reset! op (lex (g| \+ \-)))
            (awhen (unary-expression)
              #(list (uop->node @op) %)))
         (unary-expression-not-plus-minus))))
  ;; ++x -- right-associative
  (pre-increment-expression
   (g "++" (awhen (unary-expression)
             #(list (preop->node "++") %))))
  ;; --x -- right-associative
  (pre-decrement-expression
   (g "--" (awhen (unary-expression)
             #(list (preop->node "--") %))))
  ;; ++x, --x, ~x, !x -- right-associative
  (unary-expression-not-plus-minus
   (let [op (atom nil)]
     (g| (postfix-expression)
         (g (reset! op (lex (g| \~ \!)))
            (awhen (unary-expression)
              #(list (uop->node @op) %)))
         ;; (cast-expression)
         )))
  ;; x++, x-- -- right-associative
  (postfix-expression
   (let [o1 (atom nil)
         op (atom nil)]
     (g (reset! o1 (g| (primary)
                       (qualified-name)
                       ))
        (g? (reset! op (lex (g| "++" "--"))))
        (if @op
          (list (postop->node @op) @o1)
          @o1))))
  ;; 
  (primary
   (g| (primary-no-new-array)
       ;; (array-creation-expression)
       ))
  ;;
  (primary-no-new-array
   (let [e (atom nil)]
     (g| (literal)
         ;; (g (java-type) \. (kw :class))
         ;; (g (kw :void) \. (kw :class))
         ;; (g (class-name) \. (kw :this))
         (g \( (reset! e (expression)) \) @e)
         ;; (class-instance-creation-expression)
         ;; (field-access)
         ;; (method-invocation)
         ;; (array-access)
         )))
  (literal
   (awhen (lex (g #"[0-9]+"))
     #(Integer/parseInt %)))
  (identifier
   (awhen (lex (g #"[a-z]+"))
     #(symbol %))))

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

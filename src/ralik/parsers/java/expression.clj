;;; expression.clj
;;;
;;; Sunday, April  8 2012

(ns ^{:doc "Test Java expression parser"}
  ralik.parsers.java.expression
  (:use ralik.core)
  (:use [clojure.test :only [is]])
  (:require [clojure.contrib.string :as string]))

;; match a character that could be in a reserved word
(def-atomic-parser :kw-term (match #"[a-z]"))

(def *reserved-words*
  #{"abstract" "assert" "boolean" "break" "byte" "case" "catch" "char"
    "class" "const" "continue" "default" "do" "double" "else" "enum"
    "extends" "false" "final" "finally" "float" "for" "goto" "if"
    "implements" "import" "instanceof" "int" "interface" "long" "native"
    "new" "null" "package" "private" "protected" "public" "return" "short"
    "static" "strictfp" "super" "switch" "synchronized" "this" "throws"
    "throw" "transient" "try" "true" "void" "volatile" "while"})

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

(declare java-integer-string-to-number)

(defgrammar java-expression
  "Parse Java expression, return an AST." 
  [:print-err? true
   :trace? false
   :profile? false]
  ;;
  (start
   (<g 1 (expression) eoi))
  ;; 
  (expression
   (>g 0 (conditional-expression)
       (<g? 0 (assignment-operator) (expression))
       #(if (= %2 :g?-failed)
          %1
          (let [[op y] %2]
            (list (bop->node op) %1 y)))))
  ;; foo or foo.bar.baaz
  (qualified-name
   (>g_ (identifier) \.
        #(symbol (apply str (interpose \. %&)))))
  ;; 
  (assignment-operator
   (g| "=" "*=" "/=" "%=" "+=" "-=" "<<=" ">>>=" ">>=" "&=" "^=" "|="))
  ;; ?:, right-associative
  (conditional-expression
   (>g 0 (conditional-or-expression)
       (g? (<g 0 \? (expression) \: (conditional-expression)))
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
   (>g 0 (relational-expression)
       (<g* (g| "==" "!=") (relational-expression))
       #(if (empty? %2)
          %1
          (reduce (fn [x [op y]]
                    (list (bop->node op) x y))
                  %1 (partition 2 %2)))))
  ;; x <= y, x < y, x >= y, x > y, x instanceof y -- left-associative
  (relational-expression
   (>g 0 (shift-expression)
       (<g* (g| (<g 0 (g| "<=" \< ">=" \>) (shift-expression))
                (<g 0 (kw :instanceof "instanceof") (identifier))))
       #(if (empty? %2)
          %1
          (reduce (fn [x [op y]]
                    ;; str for \< and \>
                    (list (bop->node (str op)) x y))
                  %1 %2))))
  ;; x << y, x >>> y, x >> y -- left-associative
  (shift-expression
   (>g 0 (additive-expression)
       (<g* (g| "<<" ">>>" ">>") (additive-expression))
       #(if (empty? %2)
          %1
          (reduce (fn [x [op y]]
                    (list (bop->node op) x y))
                  %1 (partition 2 %2)))))
  ;; x + y, x - y -- left-associative
  (additive-expression
   (>g 0 (multiplicative-expression)
       (<g* (g| \+ \-) (multiplicative-expression))
       #(if (empty? %2)
          %1
          (reduce (fn [x [op y]]
                    (list (bop->node (str op)) x y))
                  %1 (partition 2 %2)))))
  ;; x * y, x / y, x % y -- left-associative
  (multiplicative-expression
   (>g 0 (unary-expression)
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
       (>g 0 (g| \+ \-) (unary-expression)
           #(list (uop->node (str %1)) %2))
       (unary-expression-not-plus-minus)))
  ;; ++x -- right-associative
  (pre-increment-expression
   (>g 2 "++" (unary-expression)
       #(list (preop->node "++") %)))
  ;; --x -- right-associative
  (pre-decrement-expression
   (>g 2 "--" (unary-expression)
       #(list (preop->node "--") %)))
  ;; x++, x++, ~x, !x -- right-associative
  (unary-expression-not-plus-minus
   (g| (postfix-expression)
       (>g 0 (g| \~ \!) (unary-expression)
           #(list (uop->node (str %1)) %2))
       ;; (cast-expression)
       ))
  ;; x++, x-- -- right-associative
  (postfix-expression
   (>g 0 (g| (primary)
             (qualified-name))
       (g? (g| "++" "--"))
       #(if (= %2 :g?-failed)
          %1
          (list (postop->node %2) %1))))
  ;; 
  (primary
   (g| (<g 2 \( (expression) \))
       (g (kw :this) (g* \. (identifier)) (g? (identifier-suffix)))
       (g (kw :super) (super-suffix))
       (literal)
       ;; (g (kw :new) (creator))
       (<g 1 (qualified-name) (g? (identifier-suffix)))
       (g (primitive-type) (g* \[ \]) \. (kw :class))
       (g (kw :void) \. (kw :class))))
  ;;
  (primitive-type
   (kws :boolean :char :byte :short :int :long :float :double))
  ;; 
  (identifier-suffix
   (g| (g (g+ \[ \]) \. (kw :class))
       (g (g+ \[ (expression) \]))
       (arguments)
       (g \. (g| (kw :class)
                 ;; (explicit-generic-invocation)
                 (kw :this)
                 ;; (g (kw :super) (arguments))
                 ;; (g (kw :new) (inner-creator))
                 ))))
  ;;
  (super-suffix
   (g| (arguments)
       (g \. (identifier) (g? (arguments)))))
  ;;
  (arguments
   (<g 2 \( (g? (expression-list)) \)))
  ;;
  (expression-list
   (g_ (expression) \,))
  ;;
  (literal
   (g| (floating-point-literal)
       (integer-literal)
       (character-literal)
       (string-literal)
       (boolean-literal)
       (null-literal)))
  ;;
  (integer-literal
   (g| (>lex #"0[xX][0-9a-fA-F]++(?![.dDfF])[lL]?"
             #(java-integer-string-to-number % 16))
       (>lex #"0[0-7]+(?![.dDfF])[lL]?"
             #(java-integer-string-to-number % 8))
       (>lex #"(0|[1-9][0-9]*)(?![.dDfF])[lL]?"
             #(java-integer-string-to-number % 10))))
  ;;
  (floating-point-literal
   (g| (decimal-floating-point-literal)
       (hexadecimal-floating-point-literal)))
  ;;
  (decimal-floating-point-literal
   (>lex #"(?x)((\d+\.\d*|\.\d+)([eE][+-]?\d+)?[fFdD]?)
             | (\d+([eE][+-]?\d+)?[fFdD])
             | (\d+([eE][+-]?\d+)[fFdD]?)"
         #(if (#{\f \F} (last %))
            (Float/parseFloat %)
            (Double/parseDouble %))))
  ;; 
  (hexadecimal-floating-point-literal
   (>lex #"(?x)0[xX]
               ([0-9a-fA-F]*\.[0-9a-fA-F]+ | [0-9a-fA-F]+\.?)
               [pP]
               [+-]?[0-9]+
               [fFdD]?"
         #(if (#{\f \F} (last %))
            (Float/parseFloat %)
            (Double/parseDouble %))))
  ;;
  (boolean-literal
   (g| (kw :true :java-true)
       (kw :false :java-false)))
  ;; TODO: >2lex
  ;;       (>2lex \' (character) \' #(first %))
  (character-literal
   (skip)
   (-skip (>g 2 \' (lex (g| (escape-sequence)
                            (g (g! (g| \' \\ "\n" "\r")) _)))
              \'
              #(first %))))
  ;; TODO: <2lex
  ;;       (<2lex \" (characters) \")
  ;;       But don't want to skip after the opening " so perform a preskip,
  ;;       then turn skipping off. But then lex turns skipping off *again*.
  (string-literal
   (skip)
   (-skip (<g 2 \" (lex (g* (g| (escape-sequence)
                               (g (g! (g| \\ \" "\n" "\r")) _))))
               \")))
   ;; 
   (unicode-escape
    (awhen (lex (g \\ \u (g> 4 4 #"[0-9a-fA-F]")))
      #(let [c (char (Integer/parseInt (subs % 2) 16))]
         (if-not (#{\newline \return} c) ; hard line terminators not allowed
           c))))  
   ;;
   (escape-sequence
    (g| (g \\ (g| \b \t \n \f \r \" \' \\))
        (unicode-escape)
        (octal-escape)))
   ;;
   (octal-escape
    (g| #"\\[0-3][0-7][0-7]"
        #"\\[0-7][0-7]"
        #"\\[0-7]"))
   ;;
   (null-literal
    (kw :null 'null))
   ;;
   (identifier
    (>lex (>lex _ #(Character/isJavaIdentifierStart (first %)))
          (g* (>lex _ #(Character/isJavaIdentifierPart (first %))))
          #(when-not (*reserved-words* %)
             (symbol %)))))

(defn java-integer-string-to-number
  "Given a java literal integer and a radix, return a number.
Radix must be one of 8, 10, or 16. The string s can have an optional [lL]
suffix. Hexadecimal numbers must have an \"0x\" prefix. Return the correct
size: Integer, Long, or BigInteger."
  [s radix]
  {:pre [(#{8 10 16} radix)]
   :post [(number? %)]}
  (letfn [(get-num [ss]
            (if (some #{\l \L} ss)
              (try
                (Long/parseLong (string/butlast 1 ss) radix)
                ;; too big for long
                (catch NumberFormatException e
                  (BigInteger. (string/butlast 1 ss) radix)))
              (try
                (Integer/parseInt ss radix)
                ;; too big for int
                (catch NumberFormatException e
                  (try
                    (Long/parseLong ss radix)
                    ;; too big for long
                    (catch NumberFormatException e
                      (BigInteger. ss radix)))))))]
    (if (= radix 16)
      (get-num (string/drop 2 s))
      (get-num s))))

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

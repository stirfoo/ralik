;;; java.clj
;;;
;;; Tuesday, April  3 2012

(ns ^{:doc "Java parser
See: The Java Language Specification (Java SE 7 Edition)"}
    ralik.parsers.java.java
    (:use ralik.core)
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

(declare make-skipper)
(declare java-integer-string-to-number)

(defgrammar java
  "Parse Java source code"
  [:start-rule compilation-unit
   :skipper (make-skipper)
   :print-err? true
   :memoize? false
   :match-case? true
   :trace? true
   :profile? false]
  ;;
  (identifier
   (if-let [id (lex (awhen (lex _) #(Character/isJavaIdentifierStart
                                     (first %)))
                    (g* (awhen (lex _) #(Character/isJavaIdentifierPart
                                         (first %)))))]
     (and (not (*reserved-words* id))
          id)))
  ;; 
  (qualified-identifier
   (g_ (identifier) \.))
  ;;
  (qualified-identifier-list
   (g_ (qualified-identifier) \,))
  ;;
  (compilation-unit
   (g (g? (g? (annotations)) (kw :package) (qualified-identifier) \;)
      (g* (import-declaration))
      (g* (type-declaration))
      eoi))
  ;;
  (import-declaration
   (g (kw :import) (g? (kw :static)) (qualified-identifier) (g? \. \*) \;))
  ;;
  (type-declaration
   (g| \;
       (class-or-interface-declaration)))
  ;;
  (class-or-interface-declaration
   (g (g* (modifier)) (g| (class-declaration) (interface-declaration))))
  ;;
  (class-declaration
   (g| (normal-class-declaration)
       (enum-declaration)))
  ;;
  (interface-declaration
   (g| (normal-interface-declaration)
       (annotation-type-declaration)))
  ;;
  (normal-class-declaration
   (g (kw :class) (identifier) (g? (type-parameters))
      (g? (kw :extends) (java-type))
      (g? (kw :implements) (type-list))
      (class-body)))
  ;; 
  (enum-declaration
   (g (kw :enum) (identifier) (g? (kw :implements) (type-list)) (enum-body)))
  ;;
  (normal-interface-declaration
   (g (kw :interface) (identifier) (g? (type-parameters))
      (g? (kw :extends) (type-list))
      (interface-body)))
  ;;
  (annotation-type-declaration
   (g \@ (kw :interface) (identifier) (annotation-type-body)))
  ;;
  (java-type
   (g| (g (reference-type) (g* \[ \]))
       (g (basic-type) (g* \[ \]))))
  ;;
  (reference-type
   (g (identifier) (g? (type-arguments)) (g* \. (identifier)
                                             (g? (type-arguments)))))
  ;; XXX: identical to type-parameters
  (type-arguments
   (g \< (g_ (type-argument) \,) \>))
  ;;
  (type-argument
   (g| (reference-type)
       (g \? (g? (kws :extends :super) (reference-type)))))
  ;;
  (basic-type
   (kws :byte :short :char :int :long :float :double :boolean))
  ;;
  (type-parameters
   (g \< (g_ (type-parameter) \,) \>))
  ;;
  (type-parameter
   (g (identifier) (g? (kw :extends) (bound))))
  ;;
  (bound
   (g (reference-type) (g* \& (reference-type))))
  ;;
  (non-wildcard-type-arguments
   (g \< (type-list) \>))
  ;;
  (type-list
   (g_ (reference-type) \,))
  ;;
  (modifier
   (g| (annotation)
       (kws :public :protected :private :static :abstract :final :native
            :synchronized :transient :volatile :strictfp)))
  ;;
  (annotations
   (g+ (annotation)))
  ;;
  (annotation
   (g \@ (qualified-identifier)
      (g? \( (g| \)
                 (g (g? (annotation-element))
                    \))))))
  
  ;;
  (annotation-element
   (g| (element-value-pairs)
       (element-value)))
  ;;
  (element-value-pairs
   (g_ (element-value-pair) \,))
  ;;
  (element-value-pair
   (g (identifier) \= (element-value)))
  ;;
  (element-value
   (g| (annotation)
       (expression-1)
       (element-value-array-initializer)))
  ;;
  (element-value-array-initializer
   (g \{ (g| \}
             ;; ???: can the comma stand alone?
             (g (g? (element-values)) (g? \,)
                \}))))
  ;;
  (element-values
   (g_ (element-value) \,))
  ;;
  (class-body
   (g \{ (g| \}
             (g (g* (class-body-declaration))
                \}))))
  ;;
  (class-body-declaration
   (g| \;
       (g (g* (modifier)) (member-decl))
       (g (g? (kw :static)) (block))))
  ;;
  (member-decl
   (g| (method-or-field-decl)
       (g (kw :void) (identifier) (void-method-declarator-rest))
       (g (identifier) (constructor-declarator-rest))
       (generic-method-or-constructor-decl)
       (class-declaration)
       (interface-declaration)))
  ;;
  (method-or-field-decl
   (g (java-type) (identifier) (method-or-field-rest)))
  ;;
  (method-or-field-rest
   (g| (g (variable-declarators-rest) \;)
       (method-declarator-rest)))
  ;;
  (method-declarator-rest
   (g (formal-parameters) (g* \[ \])
      (g? (kw :throws) (qualified-identifier-list)) (g| (block)
                                                        \;)))
  ;;
  (void-method-declarator-rest
   (g (formal-parameters)
      (g? (kw :throws) (qualified-identifier-list)) (g| (block)
                                                        \;)))
  ;;
  (constructor-declarator-rest
   (g (formal-parameters)
      (g? (kw :throws) (qualified-identifier-list)) (block)))
  ;;
  (generic-method-or-constructor-decl
   (g (type-parameters) (generic-method-or-constructor-rest)))
  ;;
  (generic-method-or-constructor-rest
   (g| (g (g| (java-type)
              (kw :void))
          (identifier) (method-declarator-rest))
       (g (identifier) (constructor-declarator-rest))))
  ;;
  (interface-body
   (g \{ (g| \}
             (g (g* (interface-body-declaration))
                \}))))
  ;;
  (interface-body-declaration
   (g| \;
       (g (g* (modifier)) (interface-member-decl))))
  ;;
  (interface-member-decl
   (g| (interface-method-or-field-decl)
       (g (kw :void) (identifier) (void-interface-method-declarator-rest))
       (interface-generic-method-decl)
       (class-declaration)
       (interface-declaration)))
  ;;
  (interface-method-or-field-decl
   (g (java-type) (identifier) (interface-method-or-field-rest)))
  ;;
  (interface-method-or-field-rest
   (g| (constant-declarators-rest)
       (interface-method-declarator-rest)))
  ;;
  (constant-declarators-rest
   (g (constant-declarator-rest) (g* \, (constant-declarator))))
  ;;
  (constant-declarator-rest
   (g (g* \[ \]) \= (variable-initializer)))
  ;;
  (constant-declarator
   (g (identifier) (constant-declarator-rest)))
  ;;
  (interface-method-declarator-rest
   (g (formal-parameters) (g* \[ \])
      (g? (kw :throws) (qualified-identifier-list)) \;))
  ;;
  (void-interface-method-declarator-rest
   (g (formal-parameters)
      (g? (kw :throws) (qualified-identifier-list)) \;))
  ;;
  (interface-generic-method-decl
   (g (type-parameters) (g| (java-type) (kw :void)) (identifier)
      (interface-method-declarator-rest)))
  ;;
  (formal-parameters
   (g \( (g| \)
             (g (g? (formal-parameter-decls))
                \)))))
  ;;
  (formal-parameter-decls
   (g (g* (variable-modifier)) (java-type) (formal-parameter-decls-rest)))
  ;;
  (variable-modifier
   (g| (kw :final)
       (annotation)))
  ;;
  (formal-parameter-decls-rest
   (g| (g (variable-declarator-id) (g? \, (formal-parameter-decls)))
       (g "..." (variable-declarator-id))))
  ;;
  (variable-declarator-id
   (g (identifier) (g* \[ \])))
  ;;
  (variable-declarators
   (g_ (variable-declarator) \,))
  ;;
  (variable-declarator
   (g (identifier) (variable-declarator-rest)))
  ;;
  (variable-declarator-rest
   (g (g* \[ \]) (g? \= (variable-initializer))))
  ;;
  (variable-initializer
   (g| (array-initializer)
       (expression)))
  ;;
  (array-initializer
   (g \{ (g| \}
             (g (g? (g_ (variable-initializer) \,) (g? \,))
                \}))))
  ;;
  (variable-declarators-rest
   (g (variable-declarator-rest) (g* \, (variable-declarator))))
  ;;
  (block
   (g \{ (g| \}
             (g (block-statements)
                \}))))
  ;;
  (block-statements
   (g* (block-statement)))
  ;;
  (block-statement
   (g| (local-variable-declaration-statement)
       (class-or-interface-declaration)
       (g (g? (identifier) \:) (statement))))
  ;;
  (local-variable-declaration-statement
   (g (g* (variable-modifier)) (java-type) (variable-declarators) \;))
  ;;
  (statement
   (g| \;
       (block)
       (g (kw :assert) (expression) (g? \: (expression)) \;) ; ;
       (g (kw :if) (par-expression) (statement) (g? (kw :else) (statement)))
       (g (kw :while) (par-expression) (statement))
       (g (kw :do) (statement) (kw :while) (par-expression) \;) ; ;
       (g (kw :synchronized) (par-expression) (block))
       (g (kw :return) (g? (expression)) \;)          ; ;
       (g (kw :throw) (expression) \;)                ; ;
       (g (kw :break) (g? (identifier)) \;)           ; ;
       (g (kw :continue) (g? (identifier)) \;)        ; ;
       (g (kw :try) (block) (g| (catches)
                                (g (g? catches) (kw :finally) (block))))
       (g (kw :switch) (par-expression)
          \{ (switch-block-statement-groups) \})
       (g (kw :for) \( (for-control) \) (statement))
       (g (statement-expression) \;)    ; ;
       (g (identifier) \: (statement))))
  ;;
  (statement-expression
   (expression))
  ;;
  (catches
   (g+ (catch-clause)))
  ;;
  (catch-clause
   (g (kw :catch)
      \( (g* (variable-modifier)) (java-type) (identifier) \)
      (block)))
  ;;
  (switch-block-statement-groups
   (g* (switch-block-statement-group)))
  ;;
  (switch-block-statement-group
   (g (switch-labels) (block-statements)))
  ;;
  (switch-labels
   (g+ (switch-label)))
  ;;
  (switch-label
   (g| (g (kw :case) (expression) \:)
       (g (kw :case) (enum-constant-name) \:)
       (g (kw :default) \:)))
  ;;
  (enum-constant-name
   (identifier))
  ;;
  (for-control
   (g| (for-var-control)
       (g (g? (for-init)) \; (g? (expression)) \; (g? (for-update)))))
  ;;
  (for-var-control
   (g (g* (variable-modifier)) (java-type) (variable-declarator-id)
      (for-var-control-rest)))
  ;;
  (for-var-control-rest
   (g| (g (for-variable-declarators-rest) \;
          (g? (expression)) \;
          (g? (for-update)))
       (g \: (expression))))
  ;;
  (for-variable-declarators-rest
   (g (g? \= (variable-initializer)) (g* \, (variable-declarator))))
  ;;
  (for-init
   (g_ (statement-expression) \,))
  ;;
  (for-update
   (g_ (statement-expression) \,))
  ;;
  (expression
   (g (expression-1) (g? (assignment-operator) (expression-1))))
  
  ;; assignment-expression
  ;; ternary-expression
  ;; logical-or-expression
  ;; logical-and-expression
  ;; bitwise-inclusive-or-expression
  ;; bitwise-exclusive-or-expression
  ;; bitwise-and-expression
  (equality-expression
   (g (relational-expression) (g? (g| "==" "!=") (relational-expression))))
  ;; 
  (relational-expression
   (g (shift-expression) (g? (g| "<=" \< ">=" \> (kw :instanceof))
                             (shift-expression))))
  ;; 
  (shift-expression
   (g (additive-expression) (g? (g| "<<" ">>>" ">>") (additive-expression))))
  ;; 
  (additive-expression
   (g (multiplicative-expression) (g* (g| \+ \-)
                                      (multiplicative-expression))))
  ;; 
  (multiplicative-expression
   (g (unary-expression) (g* (g| \* \/ \%) (unary-expression))))
  ;; 
  (unary-expression
   (g| (g (g| "++" "--" \+ \-) (unary-expression))
        (postfix-expression)))
  ;; 
  (postfix-expression
   (g (primary) (g* (selector)) (g? (g| "++" "--"))))
  ;; (assignment-operator
  ;;  (g| "=" "+=" "-=" "*=" "/=" "&=" "|=" "^=" "%=" "<<=" ">>>=" ">>="))
  ;; ;;
  ;; (expression-1
  ;;  (g (expression-2) (g? (expression-1-rest))))
  ;; ;; 
  ;; (expression-1-rest
  ;;  (g \? (expression) \: (expression-1)))
  ;; ;;
  ;; (expression-2
  ;;  (g (expression-3) (g? (expression-2-rest))))
  ;; ;;
  ;; (expression-2-rest
  ;;  (g| (g (kw :instanceof) (java-type))
  ;;      (g* (infix-op) (expression-3))))
  ;; ;;
  ;; (infix-op
  ;;  (g| "||" "&&" \| \^ \& "==" "!=" "<<" "<=" \< ">>>" ">>" ">=" \> \+ \-
  ;;      \* \/ \%))
  ;; ;;
  ;; (expression-3
  ;;  (g| (g (prefix-op) (expression-3))
  ;;      ;; cast expression (p483)
  ;;      (g \( (java-type) \) (expression-3))
  ;;      (g (primary) (g* (selector)) (g* (postfix-op)))))
  ;; ;;
  ;; (prefix-op
  ;;  (g| "++" "--" \! \~ \+ \-))
  ;; ;;
  ;; (postfix-op
  ;;  (g| "++" "--"))
  ;;
  (primary
   (g| (literal)
       (par-expression)
       (g (kw :this) (g? (arguments)))
       (g (kw :super) (super-suffix))
       (g (kw :new) (creator))
       (g (non-wildcard-type-arguments)
          (g| (explicit-generic-invocation-suffix)
              (g (kw :this) (arguments))))
       (g (g_ (identifier) \.) (g? (identifier-suffix)))
       (g (basic-type) (g* \[ \]) \. (kw :class))
       (g (kw :void) \. (kw :class))))
  ;;
  (literal
   (g| (floating-point-literal)
       (integer-literal)
       (character-literal)
       (string-literal)
       (boolean-literal)
       (null-literal)))
  ;;
  (par-expression
   (g \( (expression) \)))
  ;;
  (arguments
   (g \( (g| \)
             (g (g? (g_ (expression) \,))
                \)))))
  ;;
  (super-suffix
   (g| (arguments)
       (g \. (identifier) (g? (arguments)))))
  ;;
  (explicit-generic-invocation-suffix
   (g| (g (kw :super) (super-suffix))
       (g (identifier) (arguments))))
  ;;
  (creator
   (g| (g (non-wildcard-type-arguments) (created-name) (class-creator-rest))
       (g (created-name) (g| (class-creator-rest)
                             (array-creator-rest)))))
  ;;
  (created-name
   (g_ (g (g| (basic-type) (identifier)) (g? (type-arguments)))
       \.))
  ;;
  (class-creator-rest
   (g (arguments) (g? (class-body))))
  ;;
  (array-creator-rest
   (g \[
      (g| (g \] (g* \[ \]) (array-initializer))
          (g (expression) \] (g* \[ (expression) \]) (g* \[ \])))))
  ;;
  (identifier-suffix
   (g| (g \[ (g| (g (g* \[ \]) \. (kw :class))
                 (expression))
          \])
       (arguments)
       (g \. (g| (kw :class)
                 (explicit-generic-invocation)
                 (kw :this)
                 (g (kw :super) (arguments))
                 (g (kw :new) (g? (non-wildcard-type-arguments))
                    (inner-creator))))))
  ;;
  (explicit-generic-invocation
   (g (non-wildcard-type-arguments) (explicit-generic-invocation-suffix)))
  ;;
  (inner-creator
   (g (identifier) (class-creator-rest)))
  ;;
  (selector
   (g| (g \. (g| (g (identifier) (g? (arguments)))
                 (explicit-generic-invocation)
                 (kw :this)
                 (g (kw :super) (super-suffix))
                 (g (kw :new) (g? (non-wildcard-type-arguments))
                    (inner-creator))))
       (g \[ (expression) \])))
  ;;
  (enum-body
   (g \{ (g| \}
             (g (g? (enum-constants)) (g? \,) (g? (enum-body-declarations))
                \}))))
  ;;
  (enum-constants
   (g_ (enum-constant) \,))
  ;;
  (enum-constant
   (g (g? (annotations)) (identifier) (g? (arguments)) (g? (class-body))))
  ;;
  (enum-body-declarations
   (g \; (g* (class-body-declaration))))
  ;;
  (annotation-type-body
   (g \{ (g| \}
             (g (g? (annotation-type-element-declarations))
                \}))))
  ;;
  (annotation-type-element-declarations
   (g+ (annotation-type-element-declaration)))
  ;;
  (annotation-type-element-declaration
   (g (g* (modifier)) (annotation-type-element-rest)))
  ;;
  (annotation-type-element-rest
   (g| (g (java-type) (identifier) (annotation-method-or-constant-rest) \;)
       (class-declaration)
       (interface-declaration)
       (enum-declaration)
       (annotation-type-declaration)))
  ;;
  (annotation-method-or-constant-rest
   (g| (annotation-method-rest)
       (constant-declarators-rest)))
  ;;
  (annotation-method-rest
   (g \( \) (g? \[ \]) (g? (kw :default) (element-value))))
  ;;
  (unicode-escape
   (awhen (lex (g \\ \u (g> 4 4 #"[0-9a-fA-F]")))
     #(let [c (char (Integer/parseInt (subs % 2) 16))]
        (if-not (#{\newline \return} c) ; hard line terminators not allowed
          c))))
  ;;
  (literal
   (g| (floating-point-literal)
       (integer-literal)
       (boolean-literal)
       (character-literal)
       (string-literal)
       (null-literal)))
  ;;
  (integer-literal
   (g| (awhen (lex #"0[xX][0-9a-fA-F]++(?![.dDfF])[lL]?")
         #(java-integer-string-to-number % 16))
       (awhen (lex #"0[0-7]+(?![.dDfF])[lL]?")
         #(java-integer-string-to-number % 8))
       (awhen (lex #"(0|[1-9][0-9]*)(?![.dDfF])[lL]?")
         #(java-integer-string-to-number % 10))))
  ;;
  (floating-point-literal
   (g| (decimal-floating-point-literal)
       (hexadecimal-floating-point-literal)))
  ;;
  (decimal-floating-point-literal
   (awhen (lex #"(?x)((\d+\.\d*|\.\d+)([eE][+-]?\d+)?[fFdD]?)
                     | (\d+([eE][+-]?\d+)?[fFdD])
                     | (\d+([eE][+-]?\d+)[fFdD]?)")
     #(if (#{\f \F} (last %))
        (Float/parseFloat %)
        (Double/parseDouble %))))
  ;; 
  (hexadecimal-floating-point-literal
   (awhen (lex #"(?x)0[xX]
                     ([0-9a-fA-F]*\.[0-9a-fA-F]+ | [0-9a-fA-F]+\.?)
                     [pP]
                     [+-]?[0-9]+
                     [fFdD]?")
     #(if (#{\f \F} (last %))
        (Float/parseFloat %)
        (Double/parseDouble %))))
  ;;
  (boolean-literal
   (g| (kw :true :java-true)
       (kw :false :java-false)))
  ;;
  (character-literal
   (g \' (-skip (g| (escape-sequence)
                    (g (g! (g| \' \\ "\n" "\r")) _))
                \')))
  ;;
  (string-literal
   (g \" (-skip (g* (g| (escape-sequence)
                        (g (g! (g| \\ \" "\n" "\r")) _)))
                \")))
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
   (kw :null :java-null)))

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

(defn make-skipper
  "Skip all whitespace, single-line comments, and multi-line comments"
  []
  (let [mem (atom {})]
    (fn []
      (if-let [e (find @mem *cur-pos*)]
        (set! *cur-pos* (val e))
        (let [mark *cur-pos*
              sl-comment-re #"(?m)//.*?$"
              ml-comment-re #"(?s)/\*.*?\*/"]
          (while (and (< *cur-pos* *end-pos*)
                      (or
                       ;; whitespace
                       (and
                        (some #{(nth *text-to-parse* *cur-pos*)}
                              [\space \newline \return \tab \formfeed])
                        (set! *cur-pos* (inc *cur-pos*)))
                       ;; single-line comment
                       (let [m (re-matcher sl-comment-re
                                           (subs *text-to-parse* *cur-pos*))]
                         (when (.lookingAt m)
                           (set! *cur-pos* (+ *cur-pos* (.end m)))))
                       ;; multi-line comment
                       (let [m (re-matcher ml-comment-re
                                           (subs *text-to-parse* *cur-pos*))]
                         (when (.lookingAt m)
                           (set! *cur-pos* (+ *cur-pos* (.end m))))))))
          (swap! mem assoc mark *cur-pos*)))
      true)))
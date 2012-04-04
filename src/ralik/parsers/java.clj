;;; java.clj
;;;
;;; Tuesday, April  3 2012

(ns ^{:doc "Java parser. See: The Java Language Specification, 3rd Edition"}
    ralik.parsers.java
    (:use ralik.core)
    (:require [clojure.contrib.string :as string]))

;; match a character that could be in any keyword
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
   :print-err? false
   :match-case? true
   :trace? false
   :profile? false]
  ;; 
  (compilation-unit
   (g (g? (primary)) eoi))
  ;;
  (primary
   (g| (literal)
       (g (g_ (identifier) \.))         ; (g? (identifier-suffix)))
       ))
  ;;
  (literal
   (g| (floating-point-literal)
       (integer-literal)
       (boolean-literal)
       (character-literal)
       (string-literal)
       (null-literal)))
  ;; let java check each character, then make sure it's not a reserved word
  (identifier
   (if-let [id (lex (awhen (lex _) #(Character/isJavaIdentifierStart
                                     (first %)))
                    (g+ (awhen (lex _) #(Character/isJavaIdentifierPart
                                         (first %)))))]
     (and (not (*reserved-words* id))
          id)))
  ;; 1, 0xff, 0277
  (integer-literal
   (g| (awhen (lex #"0[xX][0-9a-fA-F]++(?![.dDfF])[lL]?")
         #(java-integer-string-to-number % 16))
       (awhen (lex #"0[0-7]+(?![.dDfF])[lL]?")
         #(java-integer-string-to-number % 8))
       (awhen (lex #"(0|[1-9][0-9]+)(?![.dDfF])[lL]?")
         #(java-integer-string-to-number % 10))))
  ;; 
  (floating-point-literal
   (g| (decimal-floating-point-literal)
       (hexadecimal-floating-point-literal)))
  ;; 2., .2, 3.0e-9, 3f, 22D
  (decimal-floating-point-literal
   (awhen (lex #"(?x)((\d+\.\d*|\.\d+)([eE][+-]?\d+)?[fFdD]?)
                     | (\d+([eE][+-]?\d+)?[fFdD])
                     | (\d+([eE][+-]?\d+)[fFdD]?)")
     #(if (#{\f \F} (last %))
        (Float/parseFloat %)
        (Double/parseDouble %))))
  ;; 0x0p0, 0xFP9, 0xa.p23, 0x.aP5, 0xa.fP2, 0x1.a6666666666p1
  (hexadecimal-floating-point-literal
   (awhen (lex #"(?x)0[xX]
                     ([0-9a-fA-F]*\.[0-9a-fA-F]+ | [0-9a-fA-F]+\.?)
                     [pP]
                     [+-]?[0-9]+
                     [fFdD]?")
     #(if (#{\f \F \d \D} (last %))
        (Float/parseFloat %)
        (Double/parseDouble %))))
  ;; true, false
  (boolean-literal
   (g| (kw :true :java-true)
       (kw :false :java-false)))
  ;; 'x', '\u03bb' '\t'
  (character-literal
   (g \' (-skip (g| (escape-sequence)
                    (g (g! (g| \' \\ "\n" "\r")) _))
                \')))
  ;; "", "foo", "\u03bb", "\r\n\t"
  (string-literal
   (g \" (-skip (g* (g| (escape-sequence)
                        (g (g! (g| \\ \" "\n" "\r")) _)))
                \")))
  ;; null
  (null-literal
   (kw :null :java-null))
  ;;
  (escape-sequence
   (g| (g \\ (g| \b \t \n \f \r \" \' \\))
       (unicode-escape)
       (octal-escape)))
  ;;
  (unicode-escape
   (awhen (lex (g \\ \u (g> 4 4 #"[0-9a-fA-F]")))
     #(let [c (char (Integer/parseInt (subs % 2) 16))]
        (if-not (#{\newline \return} c) ; hard line terminators not allowed
          c))))
  ;;
  (octal-escape
   (g| #"\\[0-3][0-7][0-7]"
       #"\\[0-7][0-7]"
       #"\\[0-7]")))

(defn java-integer-string-to-number
  "Given a java literal integer and a radix, return a number.
Radix must be one of 8, 10, or 16. The string s can have an optional [lL]
suffix. Hexadecimal numbers must have an \"0x\" prefix. Return the correct
size: Integer, Long, or BigInt"
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
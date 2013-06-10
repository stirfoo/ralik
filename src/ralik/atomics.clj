;;; atomics.clj
;;;
;;; Sunday, June  2 2013

(ns ^{:doc "Collection of atomic parsers"}
  ralik.atomics
  (:use ralik.core))

(defatomic uint10
  "Match an unsigned decimal integer. Return an integer."
  (or (>lex #"\d+" Integer/parseInt)
      (adv-err-pos "expected unsigned decimal integer")))

(defatomic sint10
  "Match a decimal integer with optional leading +/-.
No space allowed in the token. Return an integer."
  (or (>g #"[+-]?\d+"
          #(if (= (first %) \+)
             (Integer/parseInt (subs % 1))
             (Integer/parseInt %)))
      (adv-err-pos "expected optionally signed decimal integer")))

(defatomic uint16
  "Match an unsigned hexadecimal number with optional 0[xX] prefix.
Return an integer."
  (or (>g #"(0[xX])?[0-9a-fA-F]+"
          #(if (some #{\x \X} %)
             (Integer/parseInt (subs % 2) 16)
             (Integer/parseInt % 16)))
      (adv-err-pos "expected hexadecimal number")))

(defatomic c-ident
  "Match and return a C/C++ identifier as a string"
  (match #"[a-zA-Z_][a-zA-Z0-9_]*"))

(defatomic c-comment
  "Match a C comment, return success or failure"
  (g "/*" (g* (g! "*/") <_) "*/"))

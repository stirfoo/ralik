;;; atomics.clj
;;;
;;; Sunday, June  2 2013

(ns ^{:doc "Collection of atomic parsers"}
  ralik.atomics
  (:use ralik.core))

;; Moved the majority of them here to keep from cluttering up the core
;; namespace.

(defatomic eoi
  "Return true if at the end of input. A skip is performed first."
  (skip)
  (or (= *cur-pos* *end-pos*)
      (adv-err-pos "expected end of input")))

(defatomic wsp
  "Match a single whitespace character"
  (or (match #"[ \n\t\r\f\v]")
      (adv-err-pos "expected whitespace character")))

(defatomic wsp*
  "Match zero or more whitespace characters"
  (match #"[ \n\t\r\f\v]*"))

(defatomic wsp+
  "Match one or more whitespace characters"
  (or (match #"[ \n\t\r\f\v]+")
      (adv-err-pos "expected whitespace character")))

(defatomic blank
  "Match a single space or tab."
  (or (match #"[ \t]")
      (adv-err-pos "expected space or tab")))

(defatomic blank*
  "Match zero or more spaces or tabs"
  (match #"[ \t]*"))

(defatomic blank+
  "Match one or more spaces or tabs"
  (or (match #"[ \t]+")
      (adv-err-pos "expected space or tab")))

(defatomic eol
  "Match a single end of line terminator \r\n, \r, or \n"
  (or (match #"\r?\n|\r")
      (adv-err-pos "expected end of line terminator or end of input")))

(defatomic eol*
  "Match a zero or more line terminator \r\n, \r, or \n"
  (match #"(\r?\n|\r)*"))

(defatomic eol+
  "Match one or more end of line terminators \r\n, \r or \n"
  (or (match #"(\r?\n|\r)+")
      (adv-err-pos "expected end of line terminator or end of input")))

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

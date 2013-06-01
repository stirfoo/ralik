;;; parser.clj
;;;
;;; Sunday, April  8 2012

(ns ^{:doc "Define a template grammar for experimentation."}
  parsers.foo.parser
  (:use ralik.core)
  (:import [ralik RalikException]))

(defgrammar foo
  "Grammar for experimentation.

Matches the string foo and nothing more.
On a successful parse, return the string matched, else return nil.

All keys are given with default values except :start-rule."
  [:start-rule Start
   :skipper nil
   :match-case? false
   :print-err? false
   :memoize? false
   :trace? false
   :profile? false
   :inherit? false
   :ppfn identity]
  (Start
   (<g 0 (Statement) eoi))
  (Statement
   (<g 0 "foo")))


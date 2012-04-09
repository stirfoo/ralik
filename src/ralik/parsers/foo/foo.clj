;;; foo.clj
;;;
;;; Sunday, April  8 2012

(ns ^{:doc "Define a grammar for experimentation."}
  ralik.parsers.foo.foo
  (:use ralik.core))

(defgrammar foo
  "Grammar for experimentation"
  [:print-err? true
   :memoize? false
   :trace? false
   :profile? false]
  (start
   (g "foo" eoi)))
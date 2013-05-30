;;; RalikException.clj
;;;
;;; Tuesday, May 28 2013

(ns ^{:doc "Parse error exception.
Dont' throw this exception dircectly, call ralik.core/parse-error instead.
The exception will be thrown and caught to produce the parse error message."}
  ralik.ParserException
  (:gen-class :extends java.lang.Exception))

;;; RalikException.clj
;;;
;;; Friday, April 13 2012

(ns ^{:doc "Thrown by the core.
This occurs for instance when invalid args are passed to a parser"}
  ralik.RalikException
  (:gen-class :extends java.lang.Exception))

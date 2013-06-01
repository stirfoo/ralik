;;; parser.clj
;;;
;;; Sunday, April  8 2012

(ns ^{:doc ""}
  parsers.kludge.parser
  (:use ralik.core)
  (:use [clojure.pprint :only [pprint]])
  (:import [ralik RalikException]))

(deftype KConstant [value])

(defprotocol Mutable
  (--> [this x])
  (<-- [this]))

(deftype KSymbol
    [name ^{:volatile-mutable true} value file-pos]
  Mutable
  (--> [this x] (set! value x))
  (<-- [this] value))

(defmethod print-method KSymbol [x w]
  (let [v (<-- x)]
    (.write w (str "#<KSymbol "
                   (.name x)
                   " " (if (nil? v) "nil" v)
                   " " (.file-pos x) ">"))))

(deftype KVar
    [^{:volatile-mutable true} value]
  Mutable
  (--> [this x] (set! value x))
  (<-- [this] value))

(def reserved-word? #{})

(defgrammar kludge
  ""
  [:start-rule Program
   :print-err? true
   :ppfn pprint]
  (Program
   (<g 0 (Block) eoi))
  (Block (<g (<g? (ConstDecl))
              (<g? (VarDecl))))
  (ConstDecl (>g 1 (kw :const) (<g_ 0 (>g (Ident) "=" uint10
                                          #(do
                                             (--> %1 (KConstant. %3))
                                             %1))
                                    ",")
                 ";"
                 #(list* '$const-decl %&)))
  (VarDecl (>g 1 (kw :var) (<g_ 0 (>g (Ident)
                                      #(do
                                         (--> % (KVar. nil))
                                         %))
                                ",")
               ";"
               #(list* '$var-decl %&)))
  (Ident (>lex #"[a-zA-Z][a-zA-Z0-9_]*"
               #(when-not (reserved-word? (.toLowerCase %))
                  (KSymbol. % nil (- *cur-pos* (count %)))))))


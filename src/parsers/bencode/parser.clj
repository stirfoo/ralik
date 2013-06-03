;;; parser.clj
;;;
;;; Sunday, April  13 2012

(ns ^{:doc "Define a Bencoded string parser.
http://en.wikipedia.org/wiki/Bencode"}
  parsers.bencode.parser
  (:use ralik.core)
  (:use [ralik.atomics :only [eoi uint10]])
  (:import [ralik RalikException]))

(defgrammar bencode
  "Parse a Bencoded string.
Return a vector containing zero or more Bencoded items:
  * integer
  * vector
  * string
  * map

Partial PEG. The number of characters following the : in a BBytestring is
determined by the integer preceding the :. A PEG has no syntax to describe
this.
  Start       <- BValue* !.
  BValue      <- BInteger / BBytestring / BList / BDictionary
  Binteger    <- 'i' '-'? [0-9]+ 'e'
  BBytestring <- [0-9]+ ':' .*
  BList       <- 'l' BValue* 'e'
  BDictionary <- 'd' (BValue BValue) 'e'"
  [:start-rule Start
   :skipper nil
   :trace? false
   :print-err? true]
  (Start (<g 0 (<g* 0 (BValue)) eoi))
  (BValue (<g| (BInteger) (BBytestring) (BList) (BDictionary)))
  (BInteger (<g 1 "i" (>lex #"-?\d+" Integer/parseInt) "e"))
  (BBytestring (>g 0 uint10 ":" #(<lex (rep % <_))))
  (BList (<g 1 "l" (<g* 0 (BValue)) "e"))
  (BDictionary (>g 1 "d" (<g* (BValue) (BValue)) "e" #(into {} %&))))
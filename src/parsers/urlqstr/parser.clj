;;; parser.clj
;;;
;;; Wednesday, June 5 2013

(ns ^{:doc "Define a URL query string parser as described here:
http://en.wikipedia.org/wiki/Query_string"}
  parsers.urlqstr.parser
  (:use ralik.core)
  (:import [ralik RalikException ParserException]))

(defn find-illegal-char
  "Before parsing, check for illegal characters in the query string"
  []
  (let [m (re-matcher #"[^a-zA-Z0-9.~_+=&%-]" *text-to-parse*)]
    (when (.find m)
      (parse-error (.start m) "illegal character in query string"
                   :tag "urlqstr"))))

(defgrammar urlqstr
  "Parse a URL query string.
Missing values will be returned as \"\":
  x=y&z  => [[\"x\" \"y\"] [\"z\" \"\"]]
If the value is missing, the = cannot be present.
Return a vector of zero or more [key value] pairs."
  [:init-fn find-illegal-char
   :start-rule QueryStr
   :skipper nil
   :print-err? true]
  (QueryStr (<g 0 (>g? (<g_ 0 (QPair) (g| "&" ";"))
                       #(if (= (first %&) :empty)
                          []
                          (vec %&)))
                eoi))
  (QPair (<g (QStr) (>g? 1 "=" (QStr)
                         #(if (= % :empty)
                            ""
                            %))))
  (QStr (>g+ (QChar) str))
  (QChar (<g| (>g "+" (constantly " "))
              #"[a-zA-Z0-9.~_-]"
              (>g #"%[a-fA-F0-9]{2,2}"
                  #(char (Integer/parseInt (subs % 1) 16))))))
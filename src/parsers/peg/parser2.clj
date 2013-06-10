;;; parser2.clj
;;;
;;; Saturday, June  1 2013

(ns ^{:doc "Define a PEG parser as defined in Bryan Ford's
Parsing Expression Grammars:
A Recognition-Based Syntactic Foundation

This parser differs from parsers.peg.parser in that a skipper is used and
terminals are not separate rules. It parses in about half the time."}
    parsers.peg.parser2
    (:use ralik.core)
    (:import [ralik RalikException])
    (:use [clojure.pprint :only [pprint]]))

(defgrammar peg-skipper
  "Skip whitespace and # style comments"
  [:start-rule Skip
   :skipper nil                         ; prevent infinite loop
   :inherit? true]
  (Skip (g* (g| #"[ \n\r\t\f\v]+"
                #"(?m)#.*$"))))

(defgrammar peg
  "Parse a PEG, returning a vector of ralik rules."
  [:start-rule Grammar
   :skipper peg-skipper
   :print-err? true]
  ;; Hierarchical syntax
  (Grammar (<g 0 (<g+ (Definition)) eoi))
  (Definition (>g (Identifier) "<-" (Expression)
                  #(let [[id _ e] %&]
                     (if-not (seq? e) ; rule body must have a top-level parser
                       (list (symbol id) (list 'g e))
                       (list (symbol id) e)))))
  (Expression (>g (Sequence) (<g* 1 "/" (Sequence))
                  #(let [[h & [t]] %&]
                     (if (empty? t)
                       h
                       (list* 'g| h t)))))
  (Sequence (>g* (Prefix)
                 #(if (empty? %&)
                    (list 'g "")        ; handle empty sequence
                    (let [[p & ps] %&]
                      (if ps            ; group two or more expressions
                        (list* 'g p ps)
                        p)))))
  (Prefix (>g #"[&!]?" (Suffix)
              #(let [[op p] %&]
                 (if (empty? op)
                   p
                   (if (and (seq? p)    ; unnest g parsers
                            (= (first p) 'g))
                     (list* (symbol (str "g" op)) (next p))
                     (list (symbol (str "g" op)) p))))))
  (Suffix (>g (Primary) #"[?*+]?"
              #(let [[p op] %&]
                 (if (empty? op)
                   p
                   (if (and (seq? p)    ; unnest g parsers
                            (= (first p) 'g))
                     (list* (symbol (str "g" op)) (next p))
                     (list (symbol (str "g" op)) p))))))
  (Primary (<g| (>g 0 (Identifier) (g! "<-") list)
                (<g 1 "(" (Expression) ")")
                (Literal)
                (Class)
                (>g "." (constantly '<_))))
  
  ;; Lexical syntax
  (Identifier (>g #"[a-zA-Z_][a-zA-Z0-9_]*" symbol))
  (Literal (>g| (<g 1 "'" (<skip- (<g* 1 (g! "'") (Char :lit))) "'")
                (<g 1 \" (<skip- (<g* 1 (g! \") (Char :lit))) \")
                #(symbol (str "\"" (apply str %&) "\""))))
  (Class (>g 1 "[" (<skip- (<g* 1 (g! "]") (Range))) "]"
             #(symbol (str "#\"[" (apply str %&) "]\""))))
  (Range (<g| (>g (Char) "-" (g! "]") (Char)
                  #(let [[c1 _ _ c2] %&]
                     (str c1 \- c2)))
              (>g (Char) str)))
  (Char
   [& for-lit]
   (<g| (>g 1 "\\" #"[nrft'\"\[\]\\]" {"n" "\\n"
                                       "r" "\\r"
                                       "t" "\\t"
                                       "f" "\\f"
                                       "'" "'"
                                       "\"" "\\\""
                                       "[" (if (empty? for-lit) "\\[" "[")
                                       "]" (if (empty? for-lit) "\\]" "]")
                                       "\\" "\\\\"})
        (>g 1 "\\" #"[0-2][0-7][0-7]" #(format "\\u%04x"
                                               (Integer/parseInt % 8)))
        (>g 1 "\\" #"[0-7][0-7]?" #(format "\\u%04x"
                                           (Integer/parseInt % 8)))
        (>g 1 (g! "\\") <_ #(if (= % \")
                              ;;  for ["] 
                              "\\\""
                              (str %))))))

(defn peg->ralik
  "Read and parse in-file, a PEG, then write a ralik grammar to out-file.

out-file WILL BE OVERWRITTEN WITHOUT NOTICE (of course if out-file is *out* it
will pretty print at the repl) . If the PEG is correct, and fully defined, the
ralik parser should be able to syntax check any input against it. grammar-name
and start-rule can be a string, keyword or, quoted symbol.  Rule names are not
munged to keep them from colliding with clojure names.

A ns form will be written to out-file with grammar-name as the namespace.

Return nil."
  [in-file out-file grammar-name start-rule]
  (when-let [rules (peg (slurp in-file))]
    (let [named-sym (symbol (name grammar-name))]
      (spit out-file
            (with-out-str
              (pprint `(~(symbol "ns") ~named-sym
                        (:use ralik.core)
                        (:import [~(symbol "ralik")
                                  ~(symbol "RalikException")])))
              (println)
              (pprint (list* 'defgrammar
                             named-sym
                             ;; TODO: full path to infile
                             (str "Auto-generated grammar/parser from "
                                  in-file)
                             [:start-rule (symbol (name start-rule))
                              :skipper nil
                              :match-case? true
                              :print-err? true
                              :trace? false
                              :profile? false
                              :memoize? false
                              :ppfn 'identity]
                             rules)))))))

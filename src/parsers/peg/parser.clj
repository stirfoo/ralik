;;; peg.clj
;;;
;;; Friday, April  6 2012

(ns ^{:doc "Define a PEG parser as defined in Bryan Ford's
Parsing Expression Grammars:
A Recognition-Based Syntactic Foundation"}
    parsers.peg.parser
    (:use ralik.core)
    (:import [ralik RalikException])
    (:use [clojure.pprint :only [pprint]]))

(defgrammar peg
  "Parse a PEG, returning a vector of ralik rules.
Forward and Collector/Selector parsers, and semantic actions are employed to
return a useful value. This is done the hard way because this grammar follows
Ford's original PEG description almost exactly. The grammar handles
whitespace instead of using a ralik skipper. Only basic ralik parsers are
used. Also, a PEG has only rudimentary regular expression support. ralik uses
clojure regular expressions which are great for matching tokens.

The rhs of a rule may be empty: Foo <-. Actually, according to the grammar, a
Sequence may contain zero or more Prefix expressions. This makes a Sequence
optional and permits something like: Foo <- / 'x' / / 'y'. In these cases the
parser will insert an empty string. This will consume no input and return the
empty string as a match."
  [:start-rule Grammar
   :skipper nil                         ; the grammar handles the whitespace
   :profile? false
   :trace? false
   :memoize? true
   :print-err? true
   :ppfn identity]
  ;; Hierarchical syntax
  (Grammar (<g 1 (Spacing) (<g+ 0 (Definition)) (EndOfFile)))
  (Definition (>g (Identifier) (LEFTARROW) (Expression)
                  #(let [[id _ e] %&]
                     (if-not (seq? e) ; rule body must have a top-level parser
                       (list (symbol id) (list 'g e))
                       (list (symbol id) e)))))
  (Expression (>g (Sequence) (<g* 1 (SLASH) (Sequence))
                  #(let [[h & [t]] %&]
                     (if (empty? t)
                       h
                       (list* 'g| h t)))))
  (Sequence (>g* 0 (Prefix)
                 #(if (empty? %&)
                    ""                  ; handle empty sequence
                    (let [[p & ps] %&]
                      (if ps            ; group two or more expressions
                        (list* 'g p ps)
                        p)))))
  (Prefix (>g (<g? (<g| (AND) (NOT))) (Suffix)
              #(let [[op p] %&]
                 (if (= op :g?-failed)
                   p
                   (if (and (seq? p)    ; unnest g parsers
                            (= (first p) 'g))
                     (list* (symbol (str "g" (first op))) (next p))
                     (list (symbol (str "g" (first op))) p))))))
  (Suffix (>g (Primary) (<g? (<g| (QUESTION) (STAR) (PLUS)))
              #(let [[p op] %&]
                 (if (= op :g?-failed)
                   p
                   (if (and (seq? p)    ; unnest g parsers
                            (= (first p) 'g))
                     (list* (symbol (str "g" (first op))) (next p))
                     (list (symbol (str "g" (first op))) p))))))
  (Primary (<g| (>g 0 (Identifier) (g! (LEFTARROW)) list)
                (<g 1 (OPEN) (Expression) (CLOSE))
                (Literal)
                (Class)
                (DOT)))
  
  ;; Lexical syntax
  (Identifier (>g [0 2] (IdentStart) (<g* 0 (IdentCont)) (Spacing)
                  #(let [[x & [y]] %&]
                     (symbol (apply str x y)))))
  (IdentStart (<g 0 #"[a-zA-Z_]"))
  (IdentCont (<g| (IdentStart)
                  #"[0-9]"))
  (Literal (>g| (<g 1 "'" (<g* 1 (g! "'") (Char :lit)) "'" (Spacing))
                (<g 1 \" (<g* 1 (g! \") (Char :lit)) \" (Spacing))
                #(symbol (str "\"" (apply str %&) "\""))))
  (Class (>g 1 "[" (<g* 1 (g! "]") (Range)) "]" (Spacing)
             #(symbol (str "#\"[" (apply str %&) "]\""))))
  (Range (<g| (>g (Char) "-" (g! "]") (Char)
                  #(let [[c1 _ _ c2] %&]
                     (str c1 \- c2)))
              (>g 0 (Char) str)))
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
        (>g 1 (g! "\\") _ #(if (= % \")
                             ;;  for ["] 
                             "\\\""
                             (str %)))))

  (LEFTARROW (g "<-" (Spacing)))
  (SLASH (g "/" (Spacing)))
  (AND (<g 0 "&" (Spacing)))
  (NOT (<g 0 "!" (Spacing)))
  (QUESTION (<g 0 "?" (Spacing)))
  (STAR (<g 0 "*" (Spacing)))
  (PLUS (<g 0 "+" (Spacing)))
  (OPEN (g "(" (Spacing)))
  (CLOSE (g ")" (Spacing)))
  (DOT (>g "." (Spacing) (constantly '_)))

  (Spacing (g* (g| (Space) (Comment))))
  (Comment (g "#" (g* (g! (EndOfLine)) _)))
  (Space (g| " " "\t" (EndOfLine)))
  (EndOfLine (g| "\r\n" "\n" "\r"))
  (EndOfFile (g! _)))

(defn peg->ralik
  "Read and parse in-file, a PEG, then write a ralik grammar to out-file.

out-file WILL BE OVERWRITTEN WITHOUT NOTICE. If the PEG is correct, and fully
defined, the ralik parser should be able to syntax check any input against
it. grammar-name and start-rule can be a string, keyword or, quoted symbol.
Rule names are not munged to keep them from colliding with clojure names.

Return nil."
  [in-file out-file grammar-name start-rule]
  (when-let [rules (peg (slurp in-file))]
    (spit out-file
          (with-out-str
            (pprint '(ns "NAMESPACE"
                       (:use ralik.core)
                       (:import [ralik RalikException])))
            (println)
            (pprint (list* 'defgrammar
                           (symbol (name grammar-name))
                           ;; TODO: full path to infile
                           (str "Auto-generated grammar from " in-file)
                           [:start-rule (symbol (name start-rule))
                            :skipper nil
                            :match-case? true
                            :print-err? true
                            :trace? false
                            :profile? false
                            :memoize? false
                            :ppfn 'identity]
                           rules))))))

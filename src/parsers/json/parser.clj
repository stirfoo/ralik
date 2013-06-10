;;; parser.clj
;;;
;;; Sunday, April  8 2012
;;;
;;; TODO: 1. not handling control characters in strings
;;;       2. numbers are limited to Java Long or Double
;;;       3. string keys with spaces are somewhat handled

(ns ^{:doc "Define a JSON parser.
Example usage
  user> (require '[parsers.json.parser :as jp])
  nil
  user> (jp/json \"[1, 2, 3]\")
  [1 2 3]"}
  parsers.json.parser
  (:use ralik.core)
  (:use [clojure.walk :only [postwalk-replace]])
  (:import [ralik RalikException]))

(defgrammar json
  ;; This doc string is required
  "Grammar for JSON as described at json.org.
String keys are converted to keywords unless they contain inner spaces.
On successful parse, return a clojure vector or a clojure map.
NOTE: If you want to read JSON about a zillion times faster, use
      clojure.contrib.json. =)"
  ;; All of the key args are optional, but the vector is not.
  [:start-rule start               ; The rule to call first. Default is start.
   ;; This is the default skipper. If :skipper was omitted it would still be
   ;; used. Set :skipper to nil to disable skipping. Each time a string,
   ;; character, or regular expression is used in the grammar a skip is
   ;; performed prior to matching with it.
   :skipper wsp-skipper  
   ;; strings, characters and keywords are matched case-sensitively. Regular
   ;; expression ignore this flag. It's set true to match JSON true, false,
   ;; and null, not TRUE, FalSE, or nuLL. The default is false.
   :match-case? true     
   ;; If the parse fails, print a message showing where the parse failed but
   ;; not why. The default is false.
   :print-err? true      
   ;; these are default values
   :trace? false                  ; print a trace of the parse if true (buggy)
   :memoize? false                ; create a packrat parser (sort of) if true
   :profile? false                ; print rule profile information if true
   ;; Post Parse Function
   ;; Call this function with the result of a successful parse.
   ;; Its default value is the clojure function identity.
   :ppfn #(postwalk-replace {:true true :false false :null nil} %)]
  ;; 
  (start
   ;; return the result of the nth (0) form
   (<g 0
       ;; this form
       (<g| (jobject)                   ; try the rule jobject
            (jarray))                   ; then the rule jarray
       ;; Match the end of the input string using the atomic parser eoi.
       ;; This ensures that all the input was parsed.
       eoi))
  ;;
  (jobject
   ;; match a { followed by ...
   (g "{"
      ;; one of the two alternates
      (<g|
       ;; a }. {} is not a parsing expression so it gets evaluated
       ;; as is and returned as the result of the g parser.
       ;; A g parser simply expands into: (and form1 form2 form3).
       ;; This can be seen with C-cm in emacs.
       (g "}" {})
       ;; pass the result of the nth (0th) form to the fn in the tail
       (>g 0
           ;; Parse 1 or more k:v separated by a commas.
           ;; The 0 means return a vector of the matched values.
           ;; A 1 would mean return a vector of the separators.
           ;; Omitting the parameter would result in:
           ;; [value [sep value] [sep value] [sep value]
           ;; which works well with reduce.
           (<g_ 0
                ;; pass the result of all forms (as a vector) to the fn in the
                ;; tail
                (>g (jstring) ":" (jvalue) ; k : v
                    ;; return the single vector [k v]
                    #(let [k (.trim %1)
                           v %3]
                       (vector (if (some #{\space \tab} k)
                                 k
                                 (keyword k))
                               v)))
                ;; separator
                ",")
           ;; this is the 2nd form in the >g parser
           "}"
           ;; this fn will only be called if the previous } matched
           #(into {} %&)))))
  ;; 
  (jarray
   ;; match a [ followed by ...
   (g "["
      (g|
       ;; a ]. Again, [] is not a parsing expression so it's evaluated as is
       ;; and returned as the result of the g parser.
       (g "]" [])
       ;; OR a comma separated list of jvalueS.
       ;; The 0 means return the result of the nth parser but only AFTER
       ;; all the forms match.
       (<g 0 (<g_ 0 (jvalue) ",") "]"))))
  ;;
  (jvalue
   (<g| (jstring)
        (jnumber)
        (jobject)
        (jarray)
        ;; Match one of the keywords using its string name. Pass the matched
        ;; string to keyword and return that. Can't return false or nil as
        ;; these are the only two values used to indicate a parse failure. The
        ;; defgrammar keyarg :ppfn (Post Parse Function) handles these
        ;; returned keywords.
        (>kws :true :false :null keyword)))
  ;; 
  (hex-char
   ;; pass the text matched by the regexpr to the fn
   (>lex #"[0-9a-fA-F]{4,4}" #(char (Integer/parseInt % 16))))
  ;; 
  (esc-char
   ;; return the character result of the first successful alternate
   (<g|
    ;; return the character " \\ or /
    (>g #"[\"\\/]" first)
    ;; OR return one of the characters
    (>g #"[bfnrt]" {"b" \backspace      ; a map is a function =)
                    "f" \formfeed
                    "n" \newline
                    "r" \return
                    "t" \tab})
    ;; OR return the character result of hex-char
    (g "u" (hex-char))))
  ;; 
  (jstring
   ;; The 1 means return the result of the 2nd form (the -skip form) but only
   ;; AFTER all the form in the <g parser succeed.
   (<g 1
       ;; match the leading quote
       "\""
       ;; disable skipping while parsing between " and "
       (skip-
        ;; Match all of the forms zero or more times and pass the result of
        ;; all the successful matches of the 2nd form (the <g| form) to the fn
        ;; in the tail of the form. g*, <g*, and >g* always returns a possibly
        ;; empty vector.
        (>g* 1
             ;; peek ahead and fail if the next character (after skipping) is
             ;; a ".
             (g! "\"")
             (<g|
              ;; The !cut! operator prevents any backtracking.  Once the
              ;; \\ is read the parser is committed to this alternate. It will
              ;; not try the <_. In this case esc-char must match else the
              ;; parse will fail.
              (<g 2 "\\" !cut! (esc-char))
              ;; match any single character unless we're at the end of the
              ;; input
              <_)
             ;; Apply str to the vector of the characters accumulated by the
             ;; previous form.
             #(apply str %&)))
       ;; match the trailing quote
       "\""))
  ;; 
  (jnumber
   ;; Pass the result of the matched text to the fn. Again, only if the regexp
   ;; matches.
   (>lex #"-?(0|([1-9][0-9]*))(\.[0-9]+)?([Ee][+-]?[0-9]+)?"
         #(if (some #{\. \e \E} %)
            (Double/parseDouble %)
            (Long/parseLong %)))))

;; The same grammar, no comments or superfluous key args
(defgrammar json ""
  [:start-rule Start
   :match-case? true
   :print-err? true
   :ppfn #(postwalk-replace {:true true :false false :null nil} %)]
  (Start
   (<g 0 (<g| (Jobject) (Jarray)) eoi))
  (Jobject
   (g "{" (<g| (<g 1 "}" {})
               (>g 0 (<g_ 0 (>g (Jstring) ":" (Jvalue)
                                #(let [k (.trim %1)
                                       v %3]
                                   (vector (if (some #{\space \tab} k)
                                             k
                                             (keyword k))
                                           v)))
                          ",")
                   "}"
                   #(into {} %&)))))
  (Jarray
   (<g 1 "[" (<g| (<g 1 "]" [])
                  (<g 0 (<g_ 0 (Jvalue) ",") "]"))))
  (Jvalue
   (<g| (Jstring)
        (Jnumber)
        (Jobject)
        (Jarray)
        (>kws :true :false :null keyword)))
  (HexChar
   (>lex #"[0-9a-fA-F]{4,4}" #(char (Integer/parseInt % 16))))
  (EscChar
   (<g| (>g #"[\"\\/]" first)
        (>g #"[bfnrt]" {"b" \backspace "f" \formfeed "n" \newline
                        "r" \return "t" \tab})
        (<g 1 "u" (HexChar))))
  (Jstring
   (<g 1 "\"" (<skip- (>g* 1 (g! "\"")
                           (<g| (<g 2 "\\" !cut! (EscChar))
                                <_)
                           #(apply str %&)))
       "\""))
  (Jnumber
   (>lex #"-?(0|([1-9][0-9]*))(\.[0-9]+)?([Ee][+-]?[0-9]+)?"
         #(if (some #{\. \e \E} %)
            (Double/parseDouble %)
            (Long/parseLong %)))))

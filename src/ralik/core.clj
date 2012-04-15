(ns ^{:doc "
A Parsing Expression Grammar parsing parser to parse PEG parser... parser?

Most parse operators return nil or false on failure and some arbitrary value
equal to boolean true on success. The exceptions are lex, kw, and g?.

Character-level parsers
------------------------------------------------------------------------------
These clojure literals can be used directly within a ralik operator.
\\x      - match a literal character
          (g+ \\x) => match one or more x's
\\uXXXX  - match a literal character given a unicode code point
          (g+ \\u0233) match one or more ȳ's
\"foo\"   - match a literal string
#\"foo\"  - match a regular expression
          A PEG has syntax for character classes [A-Za-z]. This is the intent
          of this literal parser. It can be abused though. For instance:
          #\"(?s).+\" will match the entire input. One of the downsides to
          using complex regexps to match is there is no way to tell where the
          match failed.

Functional Parsers and Helpers, actually macros. The first group matches PEG's
standard operators (), +, *, ?, /, &, and !, respectively. / is reserved by
Clojure so | will do.

------------------------------------------------------------------------------
op     args        comment
---    ----------  -----------------------------------------------------------
g      & forms     match all parser arguments
g+     & forms     match one or more
g*     & forms     match zero or more
                   This parser always succeeds.
g?     & forms     match zero or one
                   This parser always succeeds but does backtrack.
g|     & forms     match the first parser that returns non-nil.
                   The parser backtracks before trying the next alternate
                   parser.
g&     & forms     fail if no match
                   The parser backtracks regardless of a match.
g!     & forms     fail if match
                   The parser backtracks regardless of a match.
---    ----------  -----------------------------------------------------------
g-     true-form   match the first form but not the second
       false-form  (g- \"bar\" \"foo\")
                   is the same as
                   (g (g! \"foo\") \"bar\")
g_     form        match delimited text
       separator   For matching lists of items interspersed with a separator.
                   (g_ #\"[0-9]+\" \\,) will match: 1,32,753,0,23423423
prm    & forms     permutation parser
rep     min         match [min, max] times
       max         Behaves as the suffix {n,m} in a regular expression except
       & forms     neither min nor max is optional.
kw     kword       match the given keyword and return the optional value
       & value
kws    & kwords    match one of the given keywords
lex    & forms     return the matched text
                   Return all of the text matched by the given parser(s) or
                   nil if no match.
---    ----------  -----------------------------------------------------------
+skip  & forms     forms will be parsed with skipping enabled
-skip  & forms     forms will be parsed with skipping disabled
+case  & forms     forms will be parsed with case sensitivity enabled
-case  & forms     forms will be parsed with case sensitivity disabled
                  
Atomic parsers
------------------------------------------------------------------------------
These parsers were defined with def-atomic-parser. Their names are keys in the
map *atomic-parsers*. Their values are vectors with the contents:
  [(gensym) code]
When a defgrammar is expanded the rules are searched for atomic parsers. When
one is found a local fn is created in the expanded letfn with the rest of the
rules. The name of the fn is the (gensym) from above. The body of the fn is
the code from above. Each instance of an atomic parser is expanded into a call
to its associated gensym'd name.

wsp       match a single space, tab, newline, or cursor return
blank     match a single space or tab
eoi       match the end of the input
eol       match the end of a line
_         match a single character
          This will always match unless the current position is at *end-pos*.
          (g+ _) will match all input, one character at a time.

:kw-term  This is a special case. See: kw doc

Utility Functions or Macros
------------------------------------------------------------------------------
def-atomic-parser    create an entry into *atomic-parsers* (see: eoi)
parse                for testing parsers against strings
offset->line-number  to aid error reporting
spep                 simple parse error printer
awhen                anaphoric-like when
aif                  anaphoric-like if
parse-avg-time       homeless man's profile tool
defgrammar           somewhat friendly grammar creation

Dynamic Globals
------------------------------------------------------------------------------
These symbols must be bound in a binding form prior to calling any parsing
functions. They have no default values.

*text-to-parse*     bind to the input string
*cur-pos*           bind to the point in *text-to-parse* where parsing
                    should begin, generally 0
*end-pos*           bind to the length of the input string
*err-pos*           bind to 0
*err-msg*           bind to whatever you like, it will probably get
                    modified
*skipper*           bind to a function to perform skipping prior to matching a
                    character, string, or regular expression. Atomic parsers
                    should call this fn as needed.
*skip?*             bind to true to enable skipping, false to disable
*match-char-case?*  bind to false if \"FoO\" is to match \"foo\"
*char=*             bind to char= or char-case=

Globals
------------------------------------------------------------------------------
*atomic-parsers* a symbol->code map
                 See: the doc for def-atomic-parser"}
  ralik.core
  (:import [ralik RalikException]))

;; ------------
;; Dynamic Vars
;; ------------

(def ^{:doc "The string being parsed. Initially unbound."}
  *text-to-parse*)
(def ^{:doc "The current parse position. Initially unbound."}
  *cur-pos*)
(def ^{:doc "The character count of *text-to-parse*. Initially unbound."}
  *end-pos*)
(def ^{:doc "The function to perform skipping. Initially unbound."}
  *skipper*)
(def ^{:doc "Skip flag. If true *skipper* is called prior to matching.
Initially unbound."}
  *skip?*)
(def ^{:doc "Character case flag. If true, the case of characters and strings
must match the input. Initially unbound."}
  *match-char-case?*)
(def ^{:doc "The function to compare two characters for equality. Should be
bound to one of: char= or char-case= prior to calling any parsers.
Initially unbound."}
  *char=*)
(def ^{:doc "The greatest char pos in *text-to-parse* where a parser failed.
*cur-pos* cannot be used as it's constantly being reset when the parser
backtracks. If a parse operator fails, it check if *cur-pos* > *err-pos*. If
so, *err-pos* is set the value of *cur-pos*. Initially unbound."}
  *err-pos*)
(def ^{:doc "A very short string giving a (possibly erroneous) hint as to why
the operator failed. Initially unbound."}
  *err-msg*)
(def ^{:doc "The map to use when a grammar's :memoize? key is true.
Each key will be:
  ['rule-name position-before-rule-is-executed]
Each associated value will be:
  [parse-result position-after-rule-is-executed
The result of the rule is cached regardless if it succeeds or not. Initially
unbound."}
  *grammar-rule-cache*)

;; -------
;; Globals
;; -------

(def ^{:doc "Symbol -> [fn-name parser-code] map
See: def-atomic-parser"}
  *atomic-parsers* (atom {}))

;; -----------
;; odds & ends
;; -----------

(defn skip
  "If *skip?* is true call *skipper*"
  []
  (when *skip?* (*skipper*)))

(defmacro adv-err-pos
  "Set *error-pos*, and *err-msg* to a message which should be a short string
hinting as to why the parser failed. Return nil."
  [message]
  `(do (set! *err-pos* (max *err-pos* *cur-pos*))
       (set! *err-msg* ~message)
       nil))

(defn re-pattern?
  "Is x a java.util.regex.Pattern?"
  [x]
  (= (type x) java.util.regex.Pattern))

;; --------------
;; Atomic Parsers
;; --------------

(defmacro def-atomic-parser 
  "Define a named parser that can be used without ().
name can be an (unquoted) symbol or a keyword. It can be used as if it were
defined with CL symbol-macrolet (No (foo) syntax required). name will simply
be replaced by a fn call where the name of the fn is gensym'd at the time
def-atomic-parser is called. body can be any code. The result of the last form
will be returned as the result of the parser.

Atomic parser are only accessable within a ralik operator. ralik operators can
be used in body. Atomic parsers can include other atomic parsers in their
body. Some built-in atomic parsers are eoi, _, eol, wsp, and blank."
  [name & body]
  (if (> (count body) 1)
    `(swap! *atomic-parsers* assoc '~name [(gensym) '(do ~@body)])
    `(swap! *atomic-parsers* assoc '~name [(gensym) '~(first body)])))

(def-atomic-parser :kw-term
  (match #"[A-Za-z0-9_]"))

(def-atomic-parser eoi
  (skip)
  (= *cur-pos* *end-pos*))

;; match and return any character as long as not at the end of input
(def-atomic-parser _
  (skip)
  (or (and (< *cur-pos* *end-pos*)
           (set! *cur-pos* (inc *cur-pos*))
           (.charAt *text-to-parse* (dec *cur-pos*)))
      (adv-err-pos "unexpected end of input")))

(def-atomic-parser wsp
  (skip)
  (or (and (< *cur-pos* *end-pos*)
           (let [c (nth *text-to-parse* *cur-pos*)]
             (some #{c} [\space \tab \newline \return]))
           (set! *cur-pos* (inc *cur-pos*)))
      (adv-err-pos "expected space, tab, newline, or cursor return")))

(def-atomic-parser blank
  (skip)
  (or (and (< *cur-pos* *end-pos*)
           (let [c (nth *text-to-parse* *cur-pos*)]
             (some #{c} [\space \tab]))
           (set! *cur-pos* (inc *cur-pos*)))
      (adv-err-pos "expected space or tab")))

(def-atomic-parser eol
  (skip)
  (or (and (< *cur-pos* *end-pos*)
           (or (and (= (nth *text-to-parse* *cur-pos*) ; dos
                       \return)
                    (and (< (inc *cur-pos*) *end-pos*)
                         (= (nth *text-to-parse* (inc *cur-pos*))
                            \newline))
                    (set! *cur-pos* (+ *cur-pos* 2)))
               (and (= (nth *text-to-parse* *cur-pos*) ; unix
                       \newline)
                    (set! *cur-pos* (inc *cur-pos*)))
               (and (= (nth *text-to-parse* *cur-pos*) ; mac
                       \return)
                    (set! *cur-pos* (inc *cur-pos*)))))
      (adv-err-pos "expected end of line")))

;; Java Integer subset, return an Integer

;; (def-atomic-parser uint8
;;   (>lex #"0[0-7]+" #(Integer/parseInt % 8)))

;; (def-atomic-parser sint8
;;   (>lex #"[+-]?0[0-7]+"
;;         #(-> (if (= (.charAt % 0)
;;                     \+)
;;                (subs % 1)
;;                %)
;;              (Integer/parseInt 8))))

;; (def-atomic-parser uint10
;;   (>lex #"0|[1-9][0-9]*" Integer/parseInt))

;; (def-atomic-parser sint10
;;   (>lex #"[+-]?(0|[1-9][0-9]*)"
;;         #(-> (if (= (.charAt % 0)
;;                     \+)
;;                (subs % 1) %)
;;              Integer/parseInt)))

;; (def-atomic-parser uint16
;;   (>lex #"0x[a-zA-Z0-9][a-fA-F0-9]*"
;;         #(Integer/parseInt (subs % 2) 16)))

;; (def-atomic-parser sint16
;;   (>lex #"[+-]?0x[a-zA-Z0-9][a-fA-F0-9]*"
;;         #(-> (cond
;;               (= (.charAt % 0) \+) (subs % 3)
;;               (= (.charAt % 0) \-) (str \- (subs % 3))
;;               :else (subs % 2))
;;              (Integer/parseInt 16))))

;; (def-atomic-parser uint
;;   (g| uint8 uint16 uint10))

;; (def-atomic-parser sint
;;   (g| sint8 sint16 sint10))

;; ;; Java floating point subset, return a Double

;; (def-atomic-parser ufloat
;;   (>lex #"(?x)((\d+\.\d*|\.\d+)([eE][+-]?\d+)?)
;;              | \d+[eE][+-]?\d+"
;;         #(Double/parseDouble %)))

;; (def-atomic-parser sfloat
;;   (>lex #"(?x)[+-]?(((\d+\.\d*|\.\d+)([eE][+-]?\d+)?)
;;                     | \d+[eE][+-]?\d+)"
;;         #(-> (if (= (.charAt % 0)
;;                     \+)
;;                (subs % 1)
;;                %)
;;              Double/parseDouble)))

;; ;; integer or float, sign not allowed
;; (def-atomic-parser unum
;;   (g| ufloat uint16 uint8 uint10))

;; ;; integer or float, optional [+-] allowed
;; (def-atomic-parser snum
;;   (g| sfloat sint16 sint8 sint10))

;; ;; C/C++ identifier, return a string
;; (def-atomic-parser cident
;;   (lex #"[a-zA-Z_][a-zA-Z0-9_]*"))

;; ;; Java Identifier, return a string
;; (def-atomic-parser jident
;;   (lex (>g 0 _ Character/isJavaIdentifierStart)
;;         (g* (>g 0 _ Character/isJavaIdentifierPart))))

;; ;; Java string literal
;; (def-atomic-parser jstr
;;   (skip)
;;   (-skip
;;    (<g 1
;;        \"
;;        (lex (g* (g| (g| (g \\ (g| \b \t \n \f \r \" \' \\))
;;                         (awhen (lex (g \\ \u (rep 4 4 #"[0-9a-fA-F]")))
;;                           #(let [c (char (Integer/parseInt (subs % 2) 16))]
;;                              (if-not (#{\newline \return} c)
;;                                c)))
;;                         (g| #"\\[0-3][0-7][0-7]"
;;                             #"\\[0-7][0-7]"
;;                             #"\\[0-7]"))
;;                     (g (g! (g| \\ \" "\n" "\r")) _))))
;;        \")))

;; ------------------
;; Low-Level Matchers
;; ------------------

(def ^{:doc "Function to compare two characters for equality, ignoring case."}
  char= (fn [^Character c1 ^Character c2]
          (= (.toLowerCase (str c1))
             (.toLowerCase (str c2)))))

(def ^{:doc "Function to compare two characters for equality, using case."}
  char-case= =)

(defn match-char
  "Return the char matched by x.
A pre-skip is performed."
  [x]
  (skip)
  (or (and (< *cur-pos* *end-pos*)
           (*char=* (.charAt *text-to-parse* *cur-pos*) x)
           (set! *cur-pos* (inc *cur-pos*))
           ;; return the character matched
           (.charAt *text-to-parse* (dec *cur-pos*)))
      (adv-err-pos (str "expected character " x))))

(defn match-string
  "Return the string matched by x.
A pre-skip is performed."
  [x]
  (skip)  
  (let [pos *cur-pos*]
    (or (loop [s (seq x)]
          (if s
            (and (< *cur-pos* *end-pos*)
                 (*char=* (.charAt *text-to-parse* *cur-pos*)
                          (first s))
                 (set! *cur-pos* (inc *cur-pos*))
                 (recur (next s)))
            ;; return the string matched
            (subs *text-to-parse* pos *cur-pos*)))
        (do (set! *cur-pos* pos)
            (adv-err-pos (str "expected string " x))))))

(defn match-regexp
  "Return the string matched by x.
A pre-skip is performed. +case and -case have no affect on this matcher. Use
the appropriate match flag(s) instead."
  [x]
  (skip)
  ;; using <= so (parse "" (g| (g? \x) #"x?") eoi) will correctly succeed
  (if (<= *cur-pos* *end-pos*)
    (let [m (re-matcher x (subs *text-to-parse* *cur-pos*))]
      (if (.lookingAt m)
        (do (set! *cur-pos* (+ *cur-pos* (.end m)))
            ;; return the matched text
            (.group m))
        (adv-err-pos (str "expected re match " (.toString x)))))))

(defmacro match
  "Perform character level matching. Return non-nil on a match. X should be
one of:
  * literal character
  * literal string
  * java.util.regex.Pattern instance
  * a symbol or keyword found in *atomic-parsers*"
  [x]
  (cond
   (char? x) `(match-char ~x)
   (string? x) `(match-string ~x)
   (re-pattern? x) `(match-regexp ~x)
   ;; if x is found in *atomic-parsers*, insert a call to its gensym'd name
   ;; that was created in def-atomic-parser, in its place.
   (or (symbol? x) (keyword? x))
   (if-let [[name _] (x @*atomic-parsers*)]
     `(~name)
     (throw (RalikException. (str "match: symbol not found in"
                                  " *atomic-parsers*: " x))))
   :else (throw (RalikException. (str "ralik.core/match: unknown form: " x)))))

;; ----------------
;; Form Translation
;; ----------------

;; '(\x \y)
;; Given that example, translate-form will return:
;; ((match \x) (match \y))
;; The calling macro should splice that into its body.

(def ^{:doc "Any op that calls translate-form or maybe-backtrack must be in
             this set"}
  *opset* '#{g g* g+ g? g| g& g! g- g_ prm rep +skip -skip
             +case -case})

(defn- translate-form
  "Walk form, wrapping strings, characters, and regexps in a match macro. Also
look up keywords and symbols in *atomic-parsers*. If found, return the
associated code. This should only be used internally if creating new core
parsers. form must be a list.

in-parser? will be true if the first element of form is in *opset*.
This is how action code is mixed with parsing code without having to implement
an explicit (now-in-action-code ...) fn or macro.

Return a list that the caller should splice into its body."
  [form in-parser?]
  (if in-parser?
    (map #(cond
           ;; 
           (list? %)
           (if (= (first %)
                  'match)
             %
             (translate-form % (#{(first %)} *opset*)))
           ;; 
           (or (char? %)
               (string? %)
               (re-pattern? %))
           (list 'match %)
           ;; 
           (and (or (keyword? %)
                    (symbol? %))
                (% @*atomic-parsers*))
           (list 'match %)
           ;; 
           :else %)
         form)
    (map #(if (list? %)
            (translate-form % (#{(first %)} *opset*))
            %)
         form)))

;; -------------
;; Basic Parsers
;; -------------

(defmacro maybe-backtrack
  "Wrap form in code that backtracks *cur-pos* if form fails to match."
  [form]
  (let [tforms (translate-form form true)]
    `(let [old-pos# *cur-pos*]
       (or ~(if (> (count tforms) 1)
              `(and ~@tforms)
              (first tforms))
           (do (set! *cur-pos* old-pos#)
               nil)))))

(defmacro +case
  "Forms are parsed with case sensitivity enabled. \"FoO\" will not match
\"foo\". Character and string matchers are affected. Regular expressions are
not. Return the result of the last form in forms."
  [form & forms]
  (let [tforms (translate-form (cons form forms) true)]
    `(binding [*match-char-case?* true
               *char=* char-case=]
       ~(if (> (count tforms) 1)
          `(and ~@tforms)
          (first tforms)))))

(defmacro -case
  "Forms are parsed with case sensitivity disabled. \"FoO\" will match \"foo\".
Character and string matchers are affected. Regular expressions are not.
Return the result of the last form in forms."
  [form & forms]
  (let [tforms (translate-form (cons form forms) true)]
    `(binding [*match-char-case?* false
               *char=* char=]
       ~(if (> (count tforms) 1)
          `(and ~@tforms)
          (first tforms)))))

(defmacro +skip
  "Enable skipping while parsing with forms. Return the result of the last
form in forms."
  [form & forms]
  (let [tforms (translate-form (cons form forms) true)]
    `(binding [*skip?* true]
       ~(if (> (count tforms) 1)
          `(and ~@tforms)
          (first tforms)))))

(defmacro -skip
  "Disable skipping while parsing with forms. Return the result of the last
form in forms."
  [form & forms]
  (let [tforms (translate-form (cons form forms) true)]
    `(binding [*skip?* false]
       ~(if (> (count tforms) 1)
          `(and ~@tforms)
          (first tforms)))))

(defmacro g
  "Return a non-nil value if forms matches. This macro performs the duty of the
PEG group operator (). Return the result of the last form in forms or nil as
soon as a form in forms returns nil.

Example:
  ;; This will fail when \\y fails to match \\q
  (parse \"xqz\" (g \\x \\y \\z))"
  [form & forms]
  (let [tforms (translate-form (cons form forms) true)]
    (if (> (count tforms) 1)
      `(and ~@tforms)
      `(do ~(first tforms)))))

(defmacro g*
  "Match forms zero or more times. This operator always returns a non-nil
value. *cur-pos* is advanced each time forms succeeds. If any form in forms
fails, the parser backtracks to the end position of the last successful parse
of forms."
  [form & forms]
  `(loop []
     (if (maybe-backtrack ~(cons form forms))
       (recur)
       true)))

(defmacro g+
  "Return a non-nil value if forms matches at least once."
  [form & forms]
  `(letfn [(f# [] (g ~@(translate-form (cons form forms) true)))]
     (when (f#)
       (g* (f#)))))

(defmacro g?
  "Match forms zero or one time. This operator always succeeds but the return
value will show if forms actually matched or not.
  * Return the result of forms if forms succeeded.
  * Return :g?-failed if forms did not succeed."
  [form & forms]
  ;; Has to backtrack because non-nil is always returned. if translate-form
  ;; was used instead, an enclosing parser that may backtrack has no way of
  ;; knowing if this failed. All parsers only know nil or false as failure,
  ;; and anything else as success
  `(if-let [res# (maybe-backtrack ~(cons form forms))]
     res#
     :g?-failed))

(defmacro g|
  "Return a non-nil value when the first alternate matches.
Example to match \\x, or \\y, or a digit followed by \\i:
  (g| \\x                    ; 1st alternate
      \\y                    ; 2nd alternate
      (g #\"\\d\" \\i))         ; 3rd alternate, grouped"
  [form & forms]
  (let [old-pos (gensym)]
    ;; Save the position so the parser can backtrack when/if an alternate
    ;; fails to match. I could use maybe-backtrack here but it would expand to
    ;; much more superfluous code than this. Each alternate would get its own
    ;; old-pos. Bleah! Also, old-pos# will not work here for reasons I do not
    ;; grok. Hence the gensym.
    `(let [~old-pos *cur-pos*]
       (or ~@(map (fn [cur-form]
		    `(or
		      ;; this alternate succeeded
		      ~@(translate-form (list cur-form) true)
		      ;; this alternate failed so backtrack
		      (do (set! *cur-pos* ~old-pos)
			  nil)))
		  (cons form forms))))))

(defmacro g&
  "Return a non-nil value if forms match. This parser peeks ahead, matching
forms, but does not advance *cur-pos*."
  [form & forms]
  (let [tforms (translate-form (cons form forms) true)]
    `(binding [*cur-pos* *cur-pos*]
       ~(if (> (count tforms) 1)
          `(and ~@tforms)
          `(do ~(first tforms))))))

(defmacro g!
  "Return a non-nil value if forms do not match. This parser peeks ahead,
  matching forms, but does not advance *cur-pos*."
  [form & forms]
  `(not (g& ~form ~@forms)))

(defmacro g-
  "Return a non-nil value if false-form does not match and true-form does.
Example:
 (parse \"aboerivneiscde\" (g+ (g- _ \\q))) ; match any character except a q"
  [true-form false-form]
  `(when (g! ~false-form)
     (g ~true-form)))

(defmacro g_
  "Match form. The result of this match will be the result of this
parser. Then match form preceded by separator zero or more times.
Examples:
  (parse \"1,2,3\" (g_ #\"\\d\" \\,)) => non-nil value, all input matched
  (parse \"1,2,x\" (g_ #\"\\d\" \\,)) => non-nil value, match |1,2|
                                    *cur-pos* will be looking at the second ,

 (g_ #\"\\d\" \\,) is just a bit shorter form of:
 (g #\"\\d\" (g* \\, #\"\\d\"))"
  [form separator]
  `(letfn [(f# []
             ~@(translate-form (list form) true))]
     (when (g (f#))
       (g* ~separator (f#)))))

(defmacro prm
  "Return a non-nil value if one or more of the parsers in forms matches, in
any order.
  Example:
  (parse \"010100001\" (lex (prm \\0 \\1))) => \"010100001\""
  [form & forms]
  ;; this letfn prevents forms from being expanded twice
  `(letfn [(f# [] (g| ~form ~@forms))]
     (when (f#)
       (while (f#))
       true)))

(defmacro rep
  "Match forms a least min times and at most max times. (<= 0 min max) must
hold. If max is 0, rep will be a no-op, but still succeed."
  [min max form & forms]
  `(if (<= 0 ~min ~max)
     (loop [n# 0]
       (if (and (< n# ~max)
                (maybe-backtrack ~(cons form forms)))
         (recur (inc n#))
         (or (>= n# ~min)
             (= ~max 0))))
     (throw (RalikException. (str "rep: (<= 0 min max) failed")))))



(defmacro kws
  "Like kw except more than one keyword can be supplied as arguments and there
is no 'value' argument. Return non-nil if one of kwords matches. The first
match wins so kwords should be ordered accordingly; foobar before foo:
  ;; This may return a false negative depending on the value of :kw-term
  (parse \"foobar\" (kws foo foobar))
  ;; This will not becuase of the correct ordering of foobar and foo
  (parse \"foobar\" (kws foobar foo))"
  [kword & kwords]
  ;; perl?
  `(g (g| ~@(map #(if (seq? %) % (name %)) (cons kword kwords)))
      (-skip (g! :kw-term))))

;; -------------------------
;; Extractors and Collectors
;; -------------------------

(defn- collect-nth-form
  "Helper to wrap forms in code that will return the nth form's result.
forms is a list of forms given to <g, <g?, etc. result must be a (gensym)."
  [forms result nth]
  (when (>= nth (count forms))
    (throw (Exception.
            (str "not enough arguments passed to collector parser"))))
  (map (fn [x i] (if (= i nth)
                   `(when-let [res# ~@(translate-form (list x) true)]
                      (reset! ~result res#))
                   x))
       forms
       (iterate inc 0)))

(defn- collect-all-forms
  "Helper to append the result of each form in forms to result.
forms is a list of forms given to <g, <g?, etc. result must be a (gensym).
Return a list."
  [forms result]
  (map (fn [x] `(when-let [res# ~@(translate-form (list x) true)]
                  (swap! ~result conj res#)))
       forms))

(defn- collect-form-range
  "Helper to append the form n (inclusive) thru form m (exclusive) to result.
forms is a list of forms given to <g, <g?, etc. result must be a (gensym).
Return a list."
  [n m forms result]
  (when (> m (inc (count forms)))
    (throw (Exception.
            (str "not enough arguments passed to collector parser"))))
  (let [s (set (range n m))]
    (map (fn [x i] (if (s i)
                     `(when-let [res# ~@(translate-form (list x) true)]
                        (swap! ~result conj res#))
                     x))
         forms
         (iterate inc 0))))

(defmacro <g
  "Same as g but return the result of forms.
If form in an integer return the result of the nth form in forms.
If form is the vector [n,m], return the result of the nth (inclusive) to
mth (exclusive) forms as a vector.
Else, return the result of all forms as a vector."
  [form & forms]
  (cond
   ;; return the result of the nth form
   (integer? form)
   (if (>= form 0)
     (let [res (gensym)
           [form2 & forms2] (collect-nth-form forms res form)]
       `(let [~res (atom nil)]
          (when (g ~form2 ~@forms2)
            @~res)))
     (throw (Exception. (str "The first argument to <g must be >= 0"
                             ", got: " form))))
   ;; [N, M]
   (vector? form)
   (if (and (= (count form) 2)
            (<= 0 (form 0))
            (< (form 0) (form 1)))
     (let [res (gensym)
           [form2 & forms2] (collect-form-range (form 0) (form 1) forms res)]
       `(let [~res (atom [])]
          (when (g ~form2 ~@forms2)
            @~res)))
     (throw (Exception. (str "The first argument to >g must be a vector of"
                             " two elements [N, M] where 0 <= N < M holds,"
                             " got: " form))))
   ;; return the result of all forms
   :else
   (let [res (gensym)
         [form2 & forms2] (collect-all-forms (cons form forms) res)]
     `(let [~res (atom [])]
        (when (g ~form2 ~@forms2)
          @~res)))))

(defmacro <g*
  "Same as g* but return the result of forms as a vector.
If form in an integer conj the result of the nth form onto a vector and return
that.
If form is the vector [n,m], conj the result of the nth (inclusive) to
mth (exclusive) forms onto a vector and return that.
Else, return the result of all forms as a vector.
This parser never fails. If nothing matches [] is returned."
  [form & forms]
  `(loop [col# []
          old-pos# *cur-pos*]
     (let [res# (<g ~form ~@forms)]
       (if res#
         (recur (conj col# res#)
                *cur-pos*)
         (do
           (set! *cur-pos* old-pos#) ; backtrack to beginning of failed <g
           col#)))))

(defmacro <g+
  "Same as g+ but return the result of forms as a vector.
If form in an integer conj the result of the nth form onto a vector and return
that.
If form is the vector [n,m], conj the result of the nth (inclusive) to
mth (exclusive) forms onto a vector and return that.
Else, return the result of all forms as a vector."
  [form & forms]
  `(letfn [(f# [] (<g ~@(translate-form (cons form forms) true)))]
     (when-let [res# (f#)]
       (into [res#] (<g* 0 (f#))))))

(defmacro <g?
  "Same as g? but return the result of forms upon success.
If form in an integer return the result of the nth form.
If form is the vector [n,m], return the result of the nth (inclusive) to
mth (exclusive) form as a vector.
Else, return the result of all forms as a vector.
If forms fail, reutrn :g?-failed.
This parser backtracks upon failure."
  [form & forms]
  `(let [old-pos# *cur-pos*]
     (if-let [res# (<g ~form ~@forms)]
       res#
       (do (set! *cur-pos* old-pos#)
           :g?-failed))))

(defmacro <g|
  "Same as g| but return the result of the first successful form."
  [form & forms]
  (let [old-pos (gensym)]
    `(let [~old-pos *cur-pos*]
       (or ~@(map (fn [cur-form]
		    `(or
		      (<g 0 ~@(translate-form (list cur-form) true))
                      (do (set! *cur-pos* ~old-pos)
                          nil)))
                  (cons form forms))))))

(defmacro <g-
  "Same as g- but return the result of true-form or nil."
  [true-form false-form]
  `(when (g! ~false-form)
     (<g 0 ~true-form)))


(defmacro <g_
  "Same as g_ except:
If i is not supplied, reuturn [form [separator form] [separator form] ...]
If i is 0, return [form form ...]
if i is 1, return [separator separator ...]. separatorS will only be conj'd if
followed by a successful form."
  ([form separator]
     ;; return: nil, [form], or[form [separator form] [separator form] ...]
     `(let [col# (atom [])]
        (when-let [res# (<g 0 ~form)]                 ; XXX: form expanded
          (swap! col# conj res#)                      ; 
          (swap! col# into (<g* ~separator ~form))))) ; XXX: twice
  ([i form separator]
     (case i
       ;; return nil or [form form ...]
       0 `(let [col# (atom [])]
            (g_ (when-let [res# (<g 0 ~form)]
                  (swap! col# conj res#))
                ~separator)
            (when-not (empty? @col#)
              @col#))
       ;; return nil, [], or [separator separator ...]
       1 `(when (g ~form)             ; XXX: form expanded
            (<g* 0 ~separator ~form)) ; XXX: twice
       (throw (RalikException. (str "<g_ expected the integer 0 or 1 as its"
                                    " first argument, got: " i))))))

(defmacro <prm
  "Same as prm but return a vector of each successive match of form."
  [form & forms]
  ;; this letfn prevents forms from being expanded twice
  `(let [col# (atom [])]
     (letfn [(f# [] (<g| ~form ~@forms))]
       (when-let [res# (f#)]
         (swap! col# conj res#)
         (swap! col# into (<g* 0 (f#)))))))

(defmacro <rep
  "Same as rep but return a vector of successive matches of forms.
If max is 0, <rep will succeed and return []."
  [min max form & forms]
  `(if (<= 0 ~min ~max)
     (loop [n# 0
            col# []
            old-pos# *cur-pos*]
       (if (< n# ~max)
         (if-let [res# (<g ~form ~@forms)]
           (recur (inc n#)
                  (conj col# res#)
                  *cur-pos*)
           (do (set! *cur-pos* old-pos#) ; backtrack last failed match
               (when (>= n# ~min)
                 ;; minimum reached, success
                 col#)))
         ;; maximum reached, success
         col#))
     (throw (RalikException. (str "<rep: (<= 0 min max) failed")))))

(defmacro <kw
  "Return the keyword matched as a string.

The keyword must be an (unquoted) symbol, a string, or a clojure keyword. Its
string name will be used to match. The atomic parser :kw-term is used to
define the code that matches characters that cannot IMMEDIATELY follow the
keyword. This is generally the set of characters that define a valid keyword
for your domain.
 (:kw-term @*atomic-parsers*) to see the current value"
  [kword]
  `(<g 0 ~(name kword) (-skip (g! :kw-term))))

(defmacro <kws
  "Return the first keyword that matches as a string or nil if no match.
This behaves as <kw except no optional return values can be supplied."
  [kword & kwords]
  `(<g 0
       (<g| ~(name kword) ~@(map name kwords))
       (-skip (g! :kw-term))))

(defn- lex-nth-form
  "Helper to wrap forms in code that will set the start (inclusive) and
 (exclusive) indexes of the text matched by the nth form. forms is a list of
forms given to <g, <g?, etc. start and end must be symbols."
  [forms start end nth]
  (when (>= nth (count forms))
    (throw (RalikException.
            (str "not enough arguments passed to lex parser"))))
  (map (fn [x i] (if (= i nth)
                   `(let [start# *cur-pos*]
                      (when-let [res# ~@(translate-form (list x) true)]
                        (reset! ~start start#)
                        (reset! ~end *cur-pos*)))
                   x))
       forms
       (iterate inc 0)))

(defn- lex-form-range
  "Helper to mark the start (inclusive) and end (exclusive) positions of the
text matched by the nth (inclusive) to mth (exclusive) forms. start and end
must be symbols."  [forms n m start end]
  (when (> m (inc (count forms)))
    (throw (RalikException.
            (str "not enough arguments passed to lex parser"))))
  (concat (take n forms)
          (list `(reset! ~start *cur-pos*))
          (take (- m n) (drop n forms))
          (list `(reset! ~end *cur-pos*))
          (drop m forms)))

(defmacro <lex
  "Return the string matched by forms.
If form in an integer return the string matched by the nth form. If form is
the vector [n,m], return the string matched by the nth (inclusive) to
mth (exclusive) forms. Else, return the string matched by all forms."
  [form & forms]
  (cond
   ;; 0, 1, 2, ...
   (integer? form)
   (if (>= form 0)
     (let [start (gensym "str-start-")
           end (gensym "str-end-")
           [form2 & forms2] (lex-nth-form forms start end form)]
       `(do
          (skip)
          (binding [*skip?* false]
            (let [~start (atom nil)
                  ~end (atom nil)
                  res# (g ~form2 ~@forms2)]
              (when res#
                (subs *text-to-parse* @~start @~end))))))
     (throw (RalikException.
             (str "The first argument to <lex must be >= 0, got: " form))))
   ;; [N, M]
   (vector? form)
   (if (and (= (count form) 2)
            (<= 0 (form 0))
            (< (form 0) (form 1)))
     (let [start (gensym "str-start-")
           end (gensym "str-end-")
           [form2 & forms2] (lex-form-range forms (form 0) (form 1)
                                            start end)]
       `(do
          (skip)
          (binding [*skip?* false]
            (let [~start (atom nil)
                  ~end (atom nil)]
              (when (g ~form2 ~@forms2)
                (subs *text-to-parse* @~start @~end))))))
     (throw (RalikException.
             (str "The first argument to <lex must be a vector of"
                  " two elements [N, M] where 0 <= N < M holds,"
                  " got: " form))))
   ;; return all text matched
   :else
   `(do
      (skip)
      (binding [*skip?* false]
        (let [start# *cur-pos*
              res# (g ~form ~@forms)]
          (when res#
            (subs *text-to-parse* start# *cur-pos*)))))))


;; (defmacro >g
;;   "Call the function f with the result of forms as specified by spec.

;; forms+f should be a list of one or more arguments with a function at the tail
;; position.

;; spec can be one of:
;;   * int >= 0
;;     Call f with the result of the nth form.
;;   * [N, M] where 0 <= N < M
;;     Collect the result of each form from N (inclusive) to M (exclusive)
;;     into a vector. Apply f to this vector.
;;   * Not an integer or vector. spec will be considered the first form.
;;     Collect the result of every form into a vector. Apply f to this vector.

;; Return the result of f or nil if any form failed."
;;   [spec & forms+f]
;;   (cond
;;    ;; 0, 1, 2, ...
;;    (integer? spec)
;;    (if (>= spec 0)
;;      (let [res (gensym)
;;            forms2 (collect-nth-form (butlast forms+f) res spec)]
;;        `(let [~res (atom nil)]
;;           (g ~@forms2
;;              (~(last forms+f) @~res))))
;;      (throw (Exception. (str "The first argument to >g must be >= 0, got:"
;;                              spec))))
;;    ;; [N, M]
;;    (vector? spec)
;;    (if (and (= (count spec) 2)
;;             (<= 0 (spec 0))
;;             (< (spec 0) (spec 1)))
;;      (let [res (gensym)
;;            forms2 (collect-form-range (spec 0) (spec 1)
;;                                       (butlast forms+f) res)]
;;        `(let [~res (atom [])]
;;           (g ~@forms2
;;              (apply ~(last forms+f) @~res))))
;;      (throw (Exception. (str "The first argument to >g must be a vector of"
;;                              " two elements [N, M] where 0 <= N < M holds,"
;;                              " got: " spec))))
;;    ;; spec not an integer or a vector, collect all form results
;;    :else
;;    (let [res (gensym)
;;          forms2 (collect-all-forms (cons spec (butlast forms+f)) res)]
;;      `(let [~res (atom [])]
;;         (g ~@forms2
;;            (apply ~(last forms+f) @~res))))))

;; (defmacro >g_
;;   "Collect the results of each matched form and apply the function f to them.
;; separator is not included in the list that f is applied to."
;;   [form separator f]
;;   `(let [col# (atom [])]
;;      (g_ (when-let [res# ~@(translate-form (list form) true)]
;;            (swap! col# conj res#))
;;          ~separator)
;;      (when-not (empty? @col#)
;;        (apply ~f @col#))))

;; (defmacro >lex
;;   "Same as lex except the function in the tail position of args is called with
;; the resulting string. The result of that function is the result of the
;; parser."
;;   [& forms+f]
;;   `(when-let [s# (lex ~@(butlast forms+f))]
;;      (~(last forms+f) s#)))

;; -------
;; Utility
;; -------

(defn wsp-skipper
  "Simple non-memoized skipper to ignore all subsequent [ \\r\\n\\t\\f].
Always returns a non-nil value"
  []
  (while (and (< *cur-pos* *end-pos*)
              (and (some #{(nth *text-to-parse* *cur-pos*)}
                         [\space \newline \return \tab \formfeed])
                   (set! *cur-pos* (inc *cur-pos*)))))
  ;; skippers never fail, but they could throw on,
  ;; for instance, invalid comment nesting
  true)

(defn offset->line-number
  "Given a zero-based offset into a string, return:
 {:line l, ; one-based line where offset found
  :col c,  ; zero-based column number where offset found on :line
  :text t} ; text of line (minus trailing line terminator) where offset found
Will throw if offset is not in the range [0, (count string)]."
  [offset string]
  (let [n (count string)]
    (when-not (< -1 offset (inc n))
      (throw (RalikException.
              (str "offset->line-number: offset must be in the"
                   " range [0, " n "] got " offset))))
    (let [m (re-matcher #"(?m:(?:\r?\n)|\r|\u0085|\u2028|\u2029|$)" string)]

      (loop [pos 0                      ; absolute pos in string
             lino 1]
        (.find m pos)
        (if (.hitEnd m)
          {:line lino
           :column (- offset pos)
           :text (subs string pos n)}
          (if (> (.end m) offset)
            {:line lino
             :column (- offset pos)
             :text (subs string pos (max (.start m) pos))}
            (recur (.end m) (inc lino))))))))

(defn spep
  "Simple Parse Error Printer.
A utility to print the line of text that caused a parse error with a ^ on the
next line pointing to the error position. m is the map returned from
offset->line-number. Output is printed to the current value of *out*"
  [m]
  (let [spacing (apply str (repeat (:column m) " "))]
    (println "\nParse Error: line" (:line m) (or (:hint m) ""))
    (printf "%s\n%s^\n\n" (:text m) spacing)))

(defmacro awhen
  "If x evaluates to non-nil call f with the result of that evaluation as its
only argument, else return nil. This is like an anaphoric defined in Graham's
\"On Lisp\" except 'it' is automagically gensym'ed.
  (awhen true not)                 => false
  (awhen (+ 2 2) (fn [x] (* x 2))) => 8
  (awhen false (fn [x] ...))       => nil, and fn is not called
It works best when used with clojures #() lambda syntax:
  (awhen (some-parser-matched?) #(do-something-with-it %))
The result of some-function does not have to be named but if it's nil, awhen
returns nil and #() is not called"
  [x f]
  `(when-let [r# ~x]
     (~f r#)))

(defmacro aif
  "If test evaluates to non-nil call then-fn with the result of that
evaluation as its only argument, else evaluate else-form."
  [test then-fn else-form]
  `(if-let [r# ~test]
     (~then-fn r#)
     ~else-form))

;; ----------------
;; Grammar Creation
;; ----------------

(def *trace-depth*)
(def *trace-indent*)
(def ^{:doc "Rule name to total-time-in-body map. Must be bound as an atom so
the macro with-profile can modify it. Initially unbound."}
  *rule-profile-map*)

(defn print-profile-info
  "Print the contents of *rule-profile-map* to the current value of *out*"
  []
  (when-let [smap (seq @*rule-profile-map*)]
    (let [name-field-size (max (reduce #(max %1 (count (name (key %2))))
				       0 smap)
			       (count "Rule Name"))]
      (printf (str "%-" name-field-size
		   "s  Total Time (ms)  Times Called\n"
      		   "%s  ===============  ============\n")
      	      "Rule Name" (apply str (repeat name-field-size "=")))
      (doseq [[k [t n]] (sort #(compare (fnext %2) (fnext %1)) smap)]
	(printf (str "%-" name-field-size "s  %-15d  %d\n")
		k t n)))))

(defmacro with-profile
  "Collect profiling info on the rule. A vector with the total time in the
rule and the number of times the rule was called is stored in the dynamic var
*rule-profile-map*. The key is the rule name. print-profile-info will display
this accumulated info. Return the result of the rule."
  [rule-name & body]
  `(let [enter-time# (.getTime (java.util.Date.))
	 result# (do ~@body)
	 exit-time# (.getTime (java.util.Date.))]
     (if-let [[total-time# times-called#] (get @*rule-profile-map*
					       '~rule-name)]
       ;; already in the map
       (swap! *rule-profile-map* assoc '~rule-name
	      [(+ total-time# (- exit-time# enter-time#))
	       (inc times-called#)])
       ;; new entry 
       (swap! *rule-profile-map* assoc '~rule-name
	      [(- exit-time# enter-time#) 1]))
     result#))

(defn- profile-rule
  "defgrammar helper"
  [rule profile?]
  (if profile?
    (let [rule-name (first rule)]
      `(~rule-name ~(second rule) (with-profile ~rule-name ~@(nnext rule))))
    rule))

(defmacro with-trace
  "defgrammar helper"
  [fn-name & body]
  `(do
     ;; (when (> *trace-depth* 200)
     ;;     (throw (Exception. "parser might stuck be in a loop")))
     (set! *trace-depth* (+ *trace-depth* *trace-indent*))
     (printf "%s%s: %s\n"
	     (apply str (repeat *trace-depth* " "))
	     (name '~fn-name)
	     (subs (.replaceAll (re-matcher #"\r|\n" *text-to-parse*) "|")
		   *cur-pos*
		   (min *end-pos*
			(max *cur-pos*
			     (+ *cur-pos*
				(- 78 *trace-depth*
				   1 (count (name '~fn-name))))))))
     (flush)
     (let [result# (do ~@body)]
       (let [sresult# (if-not result#
			"nil"
			(str result#))]
	 (println (str (apply str (repeat *trace-depth* " "))
		       (name '~fn-name) " =>")
		  (subs sresult# 0
			(min (count sresult#)
			     (max 0
				  (- 78 *trace-depth*
				     3 (count (name '~fn-name)))))))
         (flush))
       (set! *trace-depth* (- *trace-depth* *trace-indent*))
       
       result#)))

(defn- memoize-rule
  "Wrap the body of a (quoted) rule in code that caches the rule's
result. This is called at expansion time."
  [rule memoize?]
  (if memoize?
    (let [rule-name (first rule)]
      `(~rule-name
	~(second rule)			; arg list
	(if-let [[cached-result# pos#]
		 (get @*grammar-rule-cache* ['~rule-name *cur-pos*])]
	  ;; return the cached result
	  (do (set! *cur-pos* pos#) cached-result#)
	  ;; else parse and cache the result
	  (let [cp# *cur-pos* parse-result# (do ~@(nnext rule))]
	    (swap! *grammar-rule-cache* assoc ['~rule-name cp#]
		   [parse-result# *cur-pos*])
	    parse-result#))))
    ;; rule passes through untouched
    rule))

(defn- trace-rule
  "Wrap rule-body (sans the argument list) in code that produces a
trace of the parse"
  [rule trace?]
  (if trace?
    (let [rule-name (first rule)]
      `(~rule-name ~(second rule) (with-trace ~rule-name ~@(nnext rule))))
    rule))

(defn- arg-rule
  "Insert [] if the second element of the rule is not a vector."
  [rule]
  (if (vector? (fnext rule))
    rule
    (list* (first rule) [] (next rule))))

(defn- defgrammar-helper
  "Insert an arg list, memoize, trace, and profile rule if needed."
  [rule memoize? trace? profile?]
  (profile-rule
   (trace-rule
    (memoize-rule
     (arg-rule rule)
     memoize?)
    trace?)
   profile?))

(defn- find-orphaned-rules
  "Return a list of all rule names that are not referenced in the given rules
or nil if no orphans found. start-rule may or may not be referenced and is not
checked."
  [rules start-rule]
  (let [name-set (atom (clojure.set/difference (set (map first rules))
					       #{start-rule}))]
    ;; remove a rule from name-set as it is found in the body of each rule
    (doseq [r rules]
      (swap! name-set clojure.set/difference (set (flatten (rest r)))))
    ;; any left over are orphans
    (seq @name-set)))

(defmacro defgrammar
  "Expand to the function `name' that expects or or two arguments: a string of
the text to parse and an optional start rule. doc-string is not optional. The
third argument must be a possibly empty vector of key val pairs:

.--------------+-------------------------------------------------------------.
|     key      |                          val                                |
|--------------+-------------------------------------------------------------|
| :skipper     | Must be a function of no arguments to eat characters        |
|              | between tokens. It should always return a non-nil value.    |
|              | To disable skipping use nil.                                |
|              | Default: wsp-skipper (skips all white space)                |
| :start-rule  | The name of the production rule that will be called first.  |
|              | Default: start                                              |
| :match-case? | true if characters must match case                          |
|              | Default: false                                              |
| :print-err?  | true to print a simple parse error message on parse failure |
|              | Default: false                                              |
| :memoize?    | true to define a packrat parser where the result of a rule  |
|              | is cached. This may or may not improve the performance of   |
|              | parser. It depends on the grammar.                          |
|              | Default: false                                              |
| :trace?      | true to print a trace of the parse to the current value of  |
|              | *out* as the parser is working                              |
|              | Default: false                                              |
| :profile?    | true to print profiling info upon a successful parse        |
|              | Default: false                                              |
| :inherit?    | Set to true if this grammar will be called from within the  |
|              | body of another grammar. If true, this grammar will inherit |
|              | the state of the calling grammar at the point of the call.  |
|              | The expanded function will take no arguments instead of the |
|              | text to parse. All other keys are ignored if :inherit? is   |
|              | true.                                                       |
|              | Default: false                                              |
`--------------+-------------------------------------------------------------'
    
The rules are defined as if they were functions in a letfn form with one
difference, the [argument vector is optional]. If the second form is a vector
it will be taken as the argument list. So if the rule returns a vector, but
takes no arguments, [] should precede the returned vector. At least one rule
is required.

  (rule1 (code-to-parse)) 		; macro will insert [] after rule1
  (rule1 [\"User needs to place an empty [] before this vector else it
          will be mistaken for the argument list and give a funky error\"])
  (rule1 [x y] (code-to-parse))		; this rule will pass through as is

The expanded function will return the result of the start rule on success, or
nil and, if :print-err? is true, print a simple parse error message on
failure.

Example:

  (defgrammar parse-infix
    \"Syntax check an algebraic infix expressions\"
    [:start-rule expr :print-err? true]
    (expr (g (sum-expr) eoi))
    (sum-expr (g (mul-expr) (g* #\"[+-]\" (mul-expr))))
    (mul-expr (g (pow-expr) (g* #\"[*/%]\" (pow-expr))))
    (uny-expr (g| (g #\"[+-]\" (uny-expr))
                   (pow-expr)))
    (pow-expr (g (primary) (g? \"**\" (uny-expr))))
    (primary (g| (number)
                  (variable)
                  (g \\( (sum-expr) \\))
                  (g \\[ (sum-expr) \\])))
    (number (g #\"[\\d]+\"))
    (variable (g #\"[a-z]\")))

  (parse-infix \"4*[2*a*(a+3)+6*(4-a)]+5*a**2\")  => true
  (parse-infix \"4*[2*a*(a[+3)+6*(4-a)]+5*a**2\") => indicate the stray ["
  [name doc-string
   [& {:keys [skipper start-rule match-case? print-err? memoize? trace?
	      inherit? profile?]
       :as key-args
       :or {skipper 'wsp-skipper, start-rule 'start, match-case? false,
	    memoize? false, trace? false, inherit? false, profile? false}}]
   rule & rules]
  ;; some error handling
  (if-let [bad-key (loop [ks (keys key-args)]
                     (if (nil? ks)
                       nil
                       (if (some #{(first ks)} [:skipper :start-rule
                                                :match-case? :print-err?
						:memoize? :trace? :inherit?
						:profile?])
                         (recur (next ks))
                         (first ks))))]
    (throw (RalikException.
            (str "unknown defgrammar key argument: " bad-key))))
  (when-not (symbol? name)
    (throw (RalikException.
            (str "defgrammar name must be a symbol, got " name))))
  (when-not (string? doc-string)
    (throw (RalikException.
            (str "defgrammar doc-string must be a string, got "
                            doc-string))))
  (when (not-any? #{start-rule} (map first (list* rule rules)))
    (throw (RalikException.
            (format "start-rule `%s' not found in grammar `%s'"
                    start-rule name))))
  ;; warn about unreferenced rules
  (when-let [orphans (find-orphaned-rules (conj rules rule) start-rule)]
    (printf
     "WARNING: These rules are not referenced in the grammar `%s':\n" name)
    (apply print orphans)
    (println) (flush))
  ;; code
  (if inherit?
    `(defn ~name
       ~doc-string
       []
       (letfn [~@(map (fn [r]
                        (defgrammar-helper r (and memoize?
						  (not= (first r)
							start-rule))
			  trace? profile?))
                   (conj rules rule))
               ;; expand all atomic parser code
               ~@(for [[_ [name code]] @*atomic-parsers*]
                   `(~name [] ~code))]
         (~start-rule)))
    `(defn ~name
       ~doc-string
       [text#]
       (binding [*text-to-parse* text#,
                 *cur-pos* 0,
                 *err-pos* 0,
                 *err-msg* "",
                 *end-pos* (count text#),
                 *skipper* ~skipper,
                 *skip?* ~(and skipper true),
                 *match-char-case?* ~match-case?,
                 *char=* ~(if match-case?
                            'char-case=
                            'char=)
		 *grammar-rule-cache* (atom {})
		 *trace-depth* -2
		 *trace-indent* 2
		 *rule-profile-map* (atom {})]
         (letfn [~@(map #(defgrammar-helper % (and memoize?
						   (not= (first %)
							 start-rule))
			   trace? profile?)
                     (conj rules rule))
                 ;; Expand atomic parsers to local fns so they no longer have
                 ;; to be expanded at each point in the grammar.  This will
                 ;; allow the body of def-atomic-parser to freely use ralik
                 ;; core parsers instead of low-level character parsers. The
                 ;; atomic parsers body will only be expanded once, in the
                 ;; function body.
                 ;; TODO: better way to handle :kw-term
                 ~@(for [[_ [name code]] @*atomic-parsers*]
                     `(~name [] ~code))]
           (if-let [result# (~start-rule)]
	     (do
	       (when ~profile?
		 (print-profile-info))
	       result#)
             (when ~print-err?
               (spep (assoc (offset->line-number *err-pos* *text-to-parse*)
                       :hint *err-msg*)))))))))

;; -----------
;; Testing Foo
;; -----------

(defmacro parse
  "Parse text with forms. Return non-nil on successful parse. or print a parse
error message and return nil. forms is in an implied (g ...). Skipping is
enabled and performed with the fn wsp-skipper. This is a testing utility for
use at the repl."
  [text & forms]
  `(binding [*cur-pos* 0                ; position in *text-to-parse*
             *text-to-parse* ~text      ; string to parse
             *end-pos* (count ~text)    ; O_o
             *err-pos* 0                ; position of parse failure
             *err-msg* "no idea"        ; parse error hint
             *skipper* wsp-skipper      ; function to skip
             *skip?* true               ; O_o
             *match-char-case?* false   ; ignore case
             *char=* char=]             ; use case-insensitive fn
     (letfn [(go# []
               (and ~@(translate-form forms true)))
             ~@(for [[_ [name code]] @*atomic-parsers*]
                 `(~name [] ~code))]
       (if-let [result# (go#)]
         result#
         (spep (merge (offset->line-number *err-pos* *text-to-parse*)
                      {:hint *err-msg*}))))))

(defmacro parse2
  "Parse text with forms. Return non-nil on successful parse.
 message and return nil. forms are in an implied (g ...). Skipping is enabled
and performed with the fn wsp-skipper.

This is a testing utility created for use with clojure.test. It will not print
an error message on failure."
  [text & forms]
  `(binding [*cur-pos* 0                ; position in *text-to-parse*
             *text-to-parse* ~text      ; string to parse
             *end-pos* (count ~text)    ; O_o
             *err-pos* 0                ; position of parse failure
             *err-msg* "no idea"        ; parse error hint
             *skipper* wsp-skipper      ; function to skip
             *skip?* true               ; O_o
             *match-char-case?* false   ; ignore case
             *char=* char=]             ; use case-insensitive fn
     (letfn [(go# []
               (and ~@(translate-form forms true)))
             ~@(for [[_ [name code]] @*atomic-parsers*]
                 `(~name [] ~code))]
       (go#))))

(defmacro parse-avg-time
  "Call form n times and report an average run time. f should be a function of
one argument that returns non-nil if form is successful. Return a a floating
point number of the time in milliseconds or nil if f returns nil."
  [n form f]
  `(let [avg-time# 0]
     (loop [i# 0
            time-sum# 0]
       (if (= i# ~n)
         (/ time-sum# (float ~n))
         (let [start-time# (.getTime (java.util.Date.))
               result# (~f ~form)]
           (when result#
             (recur (inc i#) (+ time-sum# (- (.getTime (java.util.Date.))
                                             start-time#)))))))))

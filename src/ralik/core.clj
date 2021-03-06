;; TODO:
;; 1. Parse error report doesn't handle lines longer than the term width.
;;    Need to window the line of text.

(ns ^{:doc "A Parsing Expression Grammar based parser"}
  ralik.core
  (:use [clojure.set])
  (:import [ralik RalikException CutException ParserException]))

;; ------------
;; Dynamic Vars
;; ------------

(def ^{:dynamic true
       :doc "The string being parsed. Initially unbound."}
  *text-to-parse*)
(def ^{:dynamic true
       :doc "The current parse position. Initially unbound."}
  *cur-pos*)
(def ^{:dynamic true
       :doc "The character count of *text-to-parse*. Initially unbound."}
  *end-pos*)
(def ^{:dynamic true
       :doc "The function to perform skipping. Initially unbound."}
  *skip-fn*)
(def ^{:dynamic true
       :doc "Skip flag. If true *skip-fn* is called prior to matching.
Initially unbound."}
  *skip?*)
(def ^{:dynamic true
       :doc "The function to compare two characters for equality. Should be
bound to one of: char= or char-case= prior to calling any parsers.
Initially unbound."}
  *char=*)
(def ^{:dynamic true
       :doc "The greatest char pos in *text-to-parse* where a parser failed.
*cur-pos* cannot be used as it's constantly being reset when the parser
backtracks. If a parse operator fails, it check if *cur-pos* > *err-pos*. If
so, *err-pos* is set the value of *cur-pos*. Initially unbound."}
  *err-pos*)
(def ^{:dynamic true
       :doc "A very short string giving a (possibly erroneous) hint as to why
the operator failed. Initially unbound."}
  *err-msg*)
(def ^{:dynamic true
       :doc "For Pseudo-cut operations. This gets bound to false upon entry
into an alternative (g| <g| >g|) parser. A subsequent !cut! within one of
these parsers will set this var to true. The ordered choice parser will not
try any more alternatives. Additionally, *cur-pos* will not be reset to the
beginning of the form containing the !cut!. Initially unbound."}
  *cut*)

;; -----------
;; odds & ends
;; -----------

(defn adv-err-pos
  ([msg] (adv-err-pos msg *cur-pos*))
  ([msg pos]
     (set! *err-pos* (max *err-pos* pos))
     (set! *err-msg* msg)
     nil))

(defn re-pattern?
  "Is x a java.util.regex.Pattern?"
  [x]
  (= (type x) java.util.regex.Pattern))

;; ------------------
;; Low-Level Matchers
;; ------------------

(defn skip
  "If *skip?* is true call *skip-fn*
Called from the match method."
  []
  (when *skip?* (*skip-fn*)))

(def ^{:doc "Function to compare two characters for equality, ignoring case."}
  char= (fn [^Character c1 ^Character c2]
          (= (Character/toLowerCase c1)
             (Character/toLowerCase c2))))

(def ^{:doc "Function to compare two characters for equality, using case."}
  char-case= =)

(defprotocol Matchable
  (match [x] "Match x against *text-to-parse* at *cur-pos*.
Return the text/character matched on success else return nil or false"))

(extend-protocol Matchable
  java.lang.Character
  (match [c]
    (skip)
    (if (< *cur-pos* *end-pos*)
      (let [result (.charAt *text-to-parse* *cur-pos*)]
        (or (and (*char=* result c)
                 (set! *cur-pos* (inc *cur-pos*))
                 result)
            (adv-err-pos (str "expected character `" c "'"))))
      (adv-err-pos (str "expected character `" c "' at end of input"))))
  java.lang.String
  (match [s]
    (skip)
    (loop [sseq (seq s)
           ttpseq (seq (subs *text-to-parse* *cur-pos*))
           pos *cur-pos*
           result ""]
      (if sseq
        (if ttpseq
          (let [c (first ttpseq)]
            (if (*char=* (first sseq) c)
              (recur (next sseq) (next ttpseq) (inc pos) (str result c))
              (adv-err-pos (str "expected `" s "'"))))
          (adv-err-pos (str "expected `" s "' at end of input") *end-pos*))
        (do (set! *cur-pos* pos)
            result))))
  java.util.regex.Pattern
  (match [pat]
    (skip)
    ;; <= because a * or ? regexps can match at the end of the input string
    (if (<= *cur-pos* *end-pos*)
      (let [m (re-matcher pat (subs *text-to-parse* *cur-pos*))]
        (if (.lookingAt m)
          (do (set! *cur-pos* (+ *cur-pos* (.end m)))
              (.group m))
          (adv-err-pos (str "expected regepx match `" pat "'"))))
      (adv-err-pos (str "expected regexp match `" pat "' at end of input")))))

;; (defmulti match
;;   "Match x against *text-to-parse* at *cur-pos*.
;; Return the text/character matched on success else return nil or false"
;;   (fn [x] [(type x)]))

;; (defmethod match [java.lang.Character] [c]
;;   (skip)
;;   (if (< *cur-pos* *end-pos*)
;;     (let [result (.charAt *text-to-parse* *cur-pos*)]
;;       (or (and (*char=* result c)
;;                (set! *cur-pos* (inc *cur-pos*))
;;                result)
;;           (adv-err-pos (str "expected character `" c "'"))))
;;     (adv-err-pos (str "expected character `" c "' at end of input"))))

;; (defmethod match [java.lang.String] [s]
;;   (skip)
;;   (loop [sseq (seq s)
;;          ttpseq (seq (subs *text-to-parse* *cur-pos*))
;;          pos *cur-pos*
;;          result ""]
;;     (if sseq
;;       (if ttpseq
;;         (let [c (first ttpseq)]
;;           (if (*char=* (first sseq) c)
;;             (recur (next sseq) (next ttpseq) (inc pos) (str result c))
;;             (adv-err-pos (str "expected `" s "'"))))
;;         (adv-err-pos (str "expected `" s "' at end of input") *end-pos*))
;;       (do (set! *cur-pos* pos)
;;           result))))

;; (defmethod match [java.util.regex.Pattern] [pat]
;;   (skip)
;;   ;; <= because a * or ? regexps can match at the end of the input string
;;   (if (<= *cur-pos* *end-pos*)
;;     (let [m (re-matcher pat (subs *text-to-parse* *cur-pos*))]
;;       (if (.lookingAt m)
;;         (do (set! *cur-pos* (+ *cur-pos* (.end m)))
;;             (.group m))
;;         (adv-err-pos (str "expected regepx match `" pat "'"))))
;;     (adv-err-pos (str "expected regexp match `" pat "' at end of input"))))

;; ----------------
;; Form Translation
;; ----------------

(def backtracking-parser? '#{g* g! g& g? g| <g|})

(def ^{:doc "Any parser that DIRECTLY calls translate-form or maybe-backtrack
must be in this set"}
  translating-parser? '#{g g+ g| g& g_ <g <g+ <g|})

;; '(\x \y)
;; Given that example, translate-form will return:
;; ((match \x) (match \y))
;; The calling macro should splice that into its body.
(defn translate-form
  [form in-parser?]
  (if in-parser?
    (map #(cond
           (list? %) (if (= (first %) 'match)
                       %
                       (translate-form % (translating-parser? (first %))))
           ;; TODO: This is `off'. I have a match method, but I'm checking the
           ;;       type before returning a match. Also, it seems that (match
           ;;       "x") is faster than (match \x) so converting "x" to \x
           ;;       seems pointless. Maybe just get rid of the method and use
           ;;       match-string, and match-regexp.
           (string? %) (cond (= % "") (list 'match "")
                             (> (count %) 1) (list 'match %)
                             :else (list 'match (first %)))
           (or (char? %) (re-pattern? %)) (list 'match %)
           (symbol? %) (if (:atomic-parser (meta (resolve %)))
                         (list %)
                         %)
           :else %)
         form)
    (map #(if (list? %)
            (translate-form % (translating-parser? (first %)))
            %)
         form)))

;; ------------
;; Backtracking
;; ------------

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

;; -------------------------------------------------------
;; Root parsers, everything else is derived from these.
;; 
;; Parser       Purpose                       Peg Syntax  
;; -------------------------------------------------------
;; g            sequence                      e1 e2 ... en
;; g*           zero or more                  e*
;; g|           ordered choice                e1 / e2
;; -------------------------------------------------------

(defmacro g
  "Return a non-nil value if forms match. This macro performs the duty of a 
PEG sequence. Return the result of the last form in forms or nil as
soon as a form in forms returns nil.

Example:
  ;; This will fail when \\y fails to match \\q
  (tparse \"xqz\" (g \\x \\y \\z))"
  [form & forms]
  (let [tforms (translate-form (cons form forms) true)]
    (if (> (count tforms) 1)
      `(and ~@tforms)
      (first tforms))))

(defn emit-g*
  [tforms]
  `(loop [] (if ~tforms (recur) true)))

(defmacro g*
  "Match forms zero or more times. This operator always returns a non-nil
value. *cur-pos* is advanced each time forms succeeds. If any form in forms
fails, the parser backtracks to the end position of the last successful parse
of forms."
  [form & forms]
  (if (and (empty? forms)
           (seq? form)
           (backtracking-parser? (first form)))
    (let [tforms (translate-form (cons form ()) true)]
      (emit-g* (if (next tforms)
                 `(and ~@tforms)
                 (first tforms))))
    (emit-g* `(maybe-backtrack ~(cons form forms)))))

(defmacro g|
  "Return a non-nil value when the first alternate matches.
Example to match \\x, or \\y, or a digit followed by \\i:
  (g| \\x                    ; 1st alternate
      \\y                    ; 2nd alternate
      (g #\"\\d\" \\i))         ; 3rd alternate, grouped"
  [form & forms]
  (let [old-pos (gensym "old-pos-")]
    `(binding [*cut* false]
       (let [~old-pos *cur-pos*]
         (try
           (or ~@(map (fn [cur-form]
                        (if (and (seq? cur-form)
                                 (backtracking-parser? (first cur-form)))
                          `(or (g 0 ~cur-form)
                               (when *cut* (throw (CutException.))))
                          `(or (g 0 ~cur-form)
                               (if *cut*
                                 (throw (CutException.))
                                 (do (set! *cur-pos* ~old-pos)
                                     nil)))))
                      (cons form forms)))
           ;; if a cut `!' was encountered don't reset *cur-pos* and don't try
           ;; any more alternates
           (catch CutException e# nil))))))

;; --------------------------------------------------------
;; Derived parsers
;;
;; Parser       Purpose                          Peg Syntax
;; --------------------------------------------------------
;; g?           optional                         e / ""
;; g&           positive look ahead              &e
;; g!           negative look ahead              !e
;; g+           one or more                      e e*
;; g_           interspersed list of items       e1 (e2 e1)*
;; g-           match all but...                 !e1 e2
;; g||          a or b, or a followed by b       (e1 e2?) / e2
;; rep          like regexp x{M,N}               N/A
;; prm          permutation                      (e1 / e2)+
;; skip-        disable skipping                 N/A
;; skip+        enable  skipping                 N/A
;; case-        case insensitive                 N/A
;; case+        case sensitive                   N/A
;; --------------------------------------------------------

(defmacro g+
  "Return a non-nil value if forms matches at least once."
  [form & forms]
  `(letfn [(f# [] (g ~form ~@forms))]
     (g (f#) (g* (f#)))))

(defmacro g?
  "Match forms zero or one time. This operator always succeeds but the return
value will show if forms actually matched or not.
  * Return the result of forms if forms succeeded.
  * Return :empty if forms did not succeed."
  [form & forms]
  `(g| (g ~form ~@forms) :empty))

(defmacro g&
  "Return a non-nil value if forms match. This parser peeks ahead, matching
forms, but does not advance *cur-pos*."
  [form & forms]
  `(binding [*cur-pos* *cur-pos*]
     (g ~form ~@forms)))

(defmacro g!
  "Return a non-nil value if forms do not match. This parser peeks ahead,
  matching forms, but does not advance *cur-pos*."
  [form & forms]
  `(not (g& ~form ~@forms)))

(defmacro g-
  "Return a non-nil value if false-form does not match and true-form does.
Example:
 (tparse \"aboerivneiscde\" (g+ (g- <_ \\q))) ; match any character except a q"
  [true-form false-form]
  `(g (g! ~false-form) ~true-form))

(defmacro g_
  "Match form. The result of this match will be the result of this
parser. Then match form preceded by separator zero or more times.
Examples:
  (tparse \"1,2,3\" (g_ #\"\\d\" \\,)) => non-nil value, all input matched
  (tparse \"1,2,x\" (g_ #\"\\d\" \\,)) => non-nil value, match |1,2|
                                     *cur-pos* will be looking at the second ,

 (g_ #\"\\d\" \\,) is just a bit shorter form of:
 (g #\"\\d\" (g* \\, #\"\\d\"))"
  [form separator]
  `(letfn [(f# [] (g ~form))]
     (g (f#) (g* ~separator (f#)))))

(defmacro prm
  "Return a non-nil value if one or more of the parsers in forms matches, in
any order.
  Example:
  (tparse \"010100001\" (<lex (prm \\0 \\1))) => \"010100001\""
  [form & forms]
  `(g+ (g| ~form ~@forms)))

;; TODO: throw on m keys other than :l and :h
(defmacro rep
  "Match forms min to max times. m must be a map: {:l m :h n} where both :l
and :h are optional and will be replaced by 0 and Integer/MAX_VALUE
respectively.

NOTE: m is evaluated at runtime so it may be a var or expression evaluable
      within lexical scope of the expansion.

Examples (some are nonsensical, but accepted):
  (rep 3 \\x)           ; match an \\x exactly 3 times
  (rep 0 \\x)           ; (g? \\x)
  (rep {:l 3} \\x)      ; match an \\x at least 3 times
  (rep {:h 3} \\x)      ; match an \\x at most 3 times
  (rep {:l 3 :h 9} \\x) ; match an \\x at least 3 times and at most 9 times
  (rep {} \\x)          ; (g* \\x) with an upper limit of Integer/MAX_VALUE"
  [m form & forms]
  `(if (some true? ((juxt integer? map?) ~m))
     (let [i# (integer? ~m)
           l# (if i# ~m (get ~m :l 0))
           h# (if i# ~m (get ~m :h Integer/MAX_VALUE))]
       (if (<= 0 l# h#)
         (loop [n# 0]
           (if (and (< n# h#)
                    (maybe-backtrack ~(cons form forms)))
             (recur (inc n#))
             (or (>= n# l#)
                 (= h# 0))))
         (throw (RalikException. (str "rep: (<= 0 :l :h) failed")))))
     (throw (RalikException. "first arg to rep must be an integer or map"))))

(defmacro kw
  "Match the given keyword.

The keyword must be an (unquoted) symbol, a string, or a clojure keyword. Its
string name will be used to match. The atomic parser kw-terminator is used to
define the code that matches characters that cannot IMMEDIATELY follow the
keyword. This is generally the set of characters that define a valid keyword
for your domain.

 (<kw :foo) will not match \"foobar\" but (<g \"foo\") will."
  [kword]
  `(g ~(name kword) (skip- (g! kw-terminator))))

(defmacro kws
  "Match as the kw parser, but allows mutliple choices.
Each keyword must be castable to clojure.lang.Named"
  [kword & kwords]
  `(g| (g ~(name kword) (skip- (g! kw-terminator)))
       ~@(map (fn [x]
                `(g ~(name x) (skip- (g! kw-terminator))))
              kwords)))

(defmacro g||
  "Match form1 or form2, or form1 followed by form2

Examples from the boost Spirit documentation. All will match the entire text.
 (tparse \"123.456\" (g|| #\"\\d+\" (g \".\" #\"\\d+\")) (g! (ch)))
 (tparse \"123\" (g|| #\"\\d+\" (g \".\" #\"\\d+\")) (g! (ch)))
 (tparse \".456\" (g|| #\"\\d+\" (g \".\" #\"\\d+\")) (g! (ch)))"
  [form1 form2]
  `(letfn [(f# [] (g ~form2))]
     (g| (g ~form1 (g? (f#)))
         (f#))))

(defn case-skip-emitter
  [sym value form & forms]
  `(binding [~sym ~value]
     (g ~form ~@forms)))

(defmacro case+
  "Forms are parsed with case sensitivity enabled. \"FoO\" will not match
\"foo\". Character and string matchers are affected. Regular expressions are
not. Return the result of the last form in forms."
  [form & forms]
  (apply case-skip-emitter '*char=* 'char-case= form forms))

(defmacro case-
  "Forms are parsed with case sensitivity disabled. \"FoO\" will match \"foo\".
Character and string matchers are affected. Regular expressions are not.
Return the result of the last form in forms."
  [form & forms]
  (apply case-skip-emitter '*char=* 'char= form forms))

(defmacro skip+
  "Enable skipping while parsing with forms. Return the result of the last
form in forms."
  [form & forms]
  (apply case-skip-emitter '*skip?* true form forms))

(defmacro skip-
  "Disable skipping while parsing with forms. Return the result of the last
form in forms."
  [form & forms]
  (apply case-skip-emitter '*skip?* false form forms))

;; -------------------------------------------------------------------------
;; Extractors and Collectors
;;
;; The result of a parser is selected and returned in various ways. The
;; character `<' is prefixed to basic parsers, <g, <g*, etc.
;; -------------------------------------------------------------------------

;; Helpers

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
  (when (> m (count forms))
    (throw (Exception.
            (str "not enough arguments passed to collector parser"))))
  (let [s (set (range n m))]
    (map (fn [x i] (if (s i)
                     `(when-let [res# ~@(translate-form (list x) true)]
                        (swap! ~result conj res#))
                     x))
         forms
         (iterate inc 0))))

(defn- collect-form-set
  "Retrun the results of forms given their indexes as a set of integer
s is the set of literal integer indexes, forms is a list of forms, result must
be a (gensym). Return a list."
  [s forms result]
  (map (fn [x i] (if (s i)
                   `(when-let [res# ~@(translate-form (list x) true)]
                      (swap! ~result conj res#))
                   x))
       forms
       (iterate inc 0)))

;; Parsers

;; TODO: Allow negative integers for reverse indexing.
;;       -1 would probably be the most useful.
;;       (<g -1 \x \x \x \x \x \x "this")  => "this"
;;       Could just add the negative index to the number of forms to get the
;;       absolute index, then just recur on <g.
(defmacro <g
  "Same as g but return the result of forms.

If form in an integer return the result of the nth form in forms.
0 <= i <= forms-count must hold.

If form is the vector [start, end], return the result of the start (inclusive)
to end (exclusive) forms as a vector. 0 <= start < end <= forms-count must
hold.
If a range of one is specified, [1 2] for example, the range is an implied nth
selector:
  (<g [1 2] \\x \\y \\z) is the same as (<g 1 \\x \\y \\z)
will return \\y, not [\\y].

If form is a set of integers, return the results of those forms whoes index is
in the set. Each index must be 0 <= index <= forms-count. If the set is empty,
it's ignored.

Else, return the result of all forms as a vector.

If a single form is supplied, return the result of that form:
  (<g \\x) is the same as (<g 0 \\x)
will return \\x, not [\\x]

NOTE: The index values must be literal integers. They are evaluated at compile
      time."
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
   ;; [start, end]
   (vector? form)
   (if (and (= (count form) 2)
            (<= 0 (form 0))
            (< (form 0) (form 1)))
     (if (= (- (form 1) (form 0)) 1)
       ;; convert range of 1 to nth selector
       `(<g ~(form 0) ~@forms)
       (let [res (gensym)
             [form2 & forms2] (collect-form-range (form 0) (form 1) forms res)]
         `(let [~res (atom [])]
            (when (g ~form2 ~@forms2)
              @~res))))
     (throw (Exception. (str "The first argument to <g must be a vector of"
                             " two elements [start, end] where"
                             " 0 <= start < end holds, got: " form))))
   (set? form)
   (if (empty? form)
     `(<g ~@forms)
     (if-let [bad-indexes (seq (filter #(or (< % 0)
                                            (>= % (count forms)))
                                       form))]
       (throw (RalikException. (str "<g index(s) out of range "
                                    (seq bad-indexes))))
       (if (= (count form) 1)
         `(<g ~(first form) ~@forms)
         (let [res (gensym)
               [form2 & forms2] (collect-form-set form forms res)]
           `(let [~res (atom [])]
              (when (g ~form2 ~@forms2)
                @~res))))))
   ;; one form given, dont put its result in a vector.
   (empty? forms) `(<g 0 ~form)
   ;; return the result of all forms as a vector
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
If forms fail, return :empty.
This parser backtracks upon failure."
  [form & forms]
  `(let [old-pos# *cur-pos*]
     (if-let [res# (<g ~form ~@forms)]
       res#
       (do (set! *cur-pos* old-pos#)
           :empty))))

(defmacro <g|
  "Same as g| but return the result of the first successful form."
  [form & forms]
  (let [old-pos (gensym)]
    `(binding [*cut* false]
       (let [~old-pos *cur-pos*]
         (try
           (or ~@(map (fn [cur-form]
                        (if (and (seq? cur-form)
                                 (backtracking-parser? (first cur-form)))
                          `(or (<g 0 ~cur-form)
                               (when *cut* (throw (CutException))))
                          `(or (<g 0 ~cur-form)
                               (if *cut*
                                 (throw (CutException.))
                                 (do (set! *cur-pos* ~old-pos)
                                     nil)))))
                      (cons form forms)))
           (catch CutException e#
             nil))))))

(defmacro <g-
  "Same as g- but return the result of true-form or nil."
  [true-form false-form]
  `(when (g! ~false-form)
     (<g 0 ~true-form)))

(defmacro <g_
  "Same as g_ except:
If i is not supplied, return [form [separator form] [separator form] ...]
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

(defmacro g||
  "Match form1 or form2, or form1 followed by form2

Examples from the boost Spirit documentation. All will match the entire text.
 (tparse \"123.456\" (g|| #\"\\d+\" (g \".\" #\"\\d+\")) (g! (ch)))
 (tparse \"123\" (g|| #\"\\d+\" (g \".\" #\"\\d+\")) (g! (ch)))
 (tparse \".456\" (g|| #\"\\d+\" (g \".\" #\"\\d+\")) (g! (ch)))"
  [form1 form2]
  `(letfn [(f# [] (g ~form2))]
     (g| (g ~form1 (g? (f#)))
         (f#))))

(defmacro <g||
  "Same as g|| except:
If i is not supplied, on success return:
 [form1 form2], [:empty form2], or [form1 :empty]
If i is 0, on success return: form1 or :empty
if i is 1, on success return: form2 or :empty
NOTE: i must be a literal integer"
  ([form1 form2]
     `(letfn [(f# [] (<g 0 ~form2))]
        (<g| (<g ~form1 (<g? 0 (f#)))
             (>g 0 (f#) #(vector :empty %)))))
  ([i form1 form2]
     (when-not (#{0, 1} i)
       (throw (RalikException. (str "<g|| expected the literal integer 0 or 1"
                                    " as its first argument, got: " i))))
     `(letfn [(f# [] (<g 0 ~form2))]
        (when-let [res# (<g| (<g ~form1 (<g? 0 (f#)))
                             (>g 0 (f#) #(vector :empty %)))]
          (res# ~i)))))

(defmacro <prm
  "Same as prm but return a vector of each successive match of form."
  [form & forms]
  `(let [col# (atom [])]
     ;; this letfn prevents forms from being expanded twice     
     (letfn [(f# [] (<g| ~form ~@forms))]
       (when-let [res# (f#)]
         (swap! col# conj res#)
         (swap! col# into (<g* 0 (f#)))))))

(defmacro <rep
  "Same as rep but return a vector of successive matches of forms."
  [m form & forms]
  `(let [l# (or (and (integer? ~m) ~m)
                (and (map? ~m) (get ~m :l 0)))
         h# (or (and (integer? ~m) ~m)
                (and (map? ~m) (get ~m :h Integer/MAX_VALUE)))]
     (if (<= 0 l# h#)
       (loop [n# 0
              col# []
              old-pos# *cur-pos*]
         (if (< n# h#)
           (if-let [res# (<g ~form ~@forms)
                    ;; TODO: Modify <g instead
                    ;; ~(if (empty? forms)
                    ;;         ;; (<rep 4 \x) will return [\x \x \x \x] instead
                    ;;         ;; of [[\x] [\x] [\x] [\x]]
                    ;;         `(<g 0 ~form)
                    ;;         `(<g ~form ~@forms))
                    ]
             (recur (inc n#)
                    (conj col# res#)
                    *cur-pos*)
             (do (set! *cur-pos* old-pos#) ; backtrack last failed match
                 (when (>= n# l#)
                   ;; minimum reached, success
                   col#)))
           ;; maximum reached, success
           col#))
       (throw (RalikException. (str "<rep: (<= 0 min max) failed"))))))

(defmacro <kw
  "Return the keyword matched as a string.

The keyword must be an (unquoted) symbol, a string, or a clojure keyword. Its
string name will be used to match. The atomic parser kw-terminator is used to
define the code that matches characters that cannot IMMEDIATELY follow the
keyword. This is generally the set of characters that define a valid keyword
for your domain.

 (<kw :foo) will not match \"foobar\"."
  [kword]
  `(<g 0 ~(name kword) (skip- (g! kw-terminator))))

(defmacro <kws
  "Return the first keyword that matches as a string or nil if no match.
This behaves as <kw except no optional return values can be supplied."
  [kword & kwords]
  `(<g| (<g 0 ~(name kword) (skip- (g! kw-terminator)))
        ~@(map (fn [x]
                 `(<g 0 ~(name x) (skip- (g! kw-terminator))))
               kwords)))

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

;; TODO: only literal integers allowed as selector indexes
(defmacro <lex
  "Return the string matched by forms.

If form in an integer return the string matched by the nth form.

If form is the vector [start, end], return the string matched by the
start (inclusive) to end (exclusive) forms.

Else, return the string matched by all forms."
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
   ;; [start, end]
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

;; TODO: rethink this and <kw
(defmacro <sym
  "Collect all the text matched by forms and apply symbol.
This is a more general form of the >kw parser which only allows strings,
keywords or symbols as forms: (>kw :foo symbol). This parser will return
anything clojure's symbol can handle."
  [form & forms]
  `(>lex ~form ~@forms symbol))

(defmacro <skip-
  [form & forms]
  `(binding [*skip?* false]
     (<g ~form ~@forms)))

(defmacro <skip+
  [form & forms]
  `(binding [*skip?* true]
     (<g ~form ~@forms)))

(defmacro <case-
  [form & forms]
  `(binding [*char=* char=]
     (<g ~form ~@forms)))

(defmacro <case+
  [form & forms]
  `(binding [*char=* char-case=]
     (<g ~form ~@forms)))

;; ---------------------------------------------------------------------------
;; Forward Parsers (glorified anaphorics)
;;
;; The result of a successful parse is passed to the function at the tail of
;; the parsers argument list. The result of this fn is returned. The character
;; `>' is prefixed to basic parsers, >g, >g*, etc.
;; ---------------------------------------------------------------------------

(defn- forward-parser-helper
  "Return a similar body found in some of the forward parser macros."
  [let-sym parser-sym form forms+f]
  `(~let-sym [res# (~parser-sym ~form ~@(butlast forms+f))]
             (if (vector? res#)
               (apply ~(last forms+f) res#)
               (~(last forms+f) res#))))

(defmacro >g
  [form & forms+f]
  (forward-parser-helper 'when-let '<g form forms+f))

(defmacro >g*
  [form & forms+f]
  (forward-parser-helper 'let '<g* form forms+f))

(defmacro >g+
  [form & forms+f]
  (forward-parser-helper 'when-let '<g+ form forms+f))

(defmacro >g?
  [form & forms+f]
  (forward-parser-helper 'let '<g? form forms+f))

(defmacro >g|
  [form & forms+f]
  (forward-parser-helper 'when-let '<g| form forms+f))

(defmacro >g-
  [true-form false-form f]
  (forward-parser-helper 'when-let '<g- true-form (list false-form f)))

(defmacro >g_
  "Collect the result of forms as <g_ and call f with them."
  ([form separator f]
     (forward-parser-helper 'when-let '<g_ form (list separator f)))
  ([i form separator f]
     (forward-parser-helper 'when-let '<g_ i (list form separator f))))

(defmacro >g||
  "Same as <g|| except call f with the successful result"
  ([form1 form2 f]
     (forward-parser-helper 'when-let '<g|| form1 (list form2 f)))
  ([i form1 form2 f]
     (forward-parser-helper 'when-let '<g|| i (list form1 form2 f))))

(defmacro >prm
  [form & forms+f]
  (forward-parser-helper 'when-let '<prm form forms+f))

(defmacro >rep
  "Same as <rep except call f with the successful result.
NOTE: f will always recieve exactly one argument, a possibly empty vector."
  [m form & forms+f]
  `(when-let [res# (<rep ~m ~form ~@(butlast forms+f))]
     (~(last forms+f) res#)))

(defmacro >kw
  [kword f]
  `(when-let [res# (<kw ~kword)]
     (~f res#)))

(defmacro >kws
  [kword & kwords+f]
  `(when-let [res# (<kws ~kword ~@(butlast kwords+f))]
     (~(last kwords+f) res#)))

(defmacro >lex
  [form & forms+f]
  `(when-let [res# (<lex ~form ~@(butlast forms+f))]
     (~(last forms+f) res#)))

(defmacro >sym
  [form & forms+f]
  (forward-parser-helper 'when-let '<sym form forms+f))

(defmacro >skip-
  [form & forms+f]
  `(binding [*skip?* false]
     (>g ~form ~@forms+f)))

(defmacro >skip+
  [form & forms+f]
  `(binding [*skip?* true]
     (>g ~form ~@forms+f)))

(defmacro >case-
  [form & forms+f]
  `(binding [*char=* char=]
     (>g ~form ~@forms+f)))

(defmacro >case+
  [form & forms+f]
  `(binding [*char=* char-case=]
     (>g ~form ~@forms+f)))

;; =======================================================================
;; 
;;                           Atomic Parsers
;;                           
;; =======================================================================

(defmacro defatomic
  "Define a ralik operator that can be called within a parser without
parentheses. eoi instead of (eoi)."
  [name doc-str & body]
  (when-not (string? doc-str)
    (throw (RalikException.
            (str "defatomic requests a doc string on: " name))))
  ;; nag-free redefinition atomic parsers
  (when (:atomic-parser (meta (resolve name)))
    (ns-unmap *ns* name))
  `(defn ~name ~doc-str {:atomic-parser true} [] ~@body))

;; TODO: need tests for this
(defatomic !cut!
  "Define a pseudo `cut' operator.
Used within an ordered choice parser: g|, <g|, or >g|. If the current
alternative contains a !cut! and fails, the parser will not backtrack"
  (when-not (bound? #'*cut*)
    (throw (RalikException. (str "cut operator (!) must only occur within an"
                                 " alternate parser (g|, <g|, or >g|)\n"))))
  (set! *cut* true)
  :!-result)

(defatomic <_
  "Match any single character if not at the end of the input.
A skip is performed first. Return the character."
  (skip)
  (or (and (< *cur-pos* *end-pos*)
           (set! *cur-pos* (inc *cur-pos*))
           (.charAt *text-to-parse* (dec *cur-pos*)))
      (adv-err-pos "unexpected end of input")))

(defatomic kw-terminator
  "Define the kw and kws parser's terminator condition.
This should define characters that cannot immediately follow the last
character of the keyword(s) given to kw or kws.

The default is: (match #\"[a-zA-Z0-9_]\")"
  (match #"[a-zA-Z0-9_]"))

(defatomic eoi
  "Return true if at the end of input. A skip is performed first."
  (skip)
  (or (= *cur-pos* *end-pos*)
      (adv-err-pos "expected end of input")))

(defatomic wsp
  "Match a single whitespace character"
  (or (match #"[ \n\t\r\f\v]")
      (adv-err-pos "expected whitespace character")))

(defatomic wsp*
  "Match zero or more whitespace characters"
  (match #"[ \n\t\r\f\v]*"))

(defatomic wsp+
  "Match one or more whitespace characters"
  (or (match #"[ \n\t\r\f\v]+")
      (adv-err-pos "expected whitespace character")))

(defatomic blank
  "Match a single space or tab."
  (or (match #"[ \t]")
      (adv-err-pos "expected space or tab")))

(defatomic blank*
  "Match zero or more spaces or tabs"
  (match #"[ \t]*"))

(defatomic blank+
  "Match one or more spaces or tabs"
  (or (match #"[ \t]+")
      (adv-err-pos "expected space or tab")))

(defatomic eol
  "Match a single end of line terminator \r\n, \r, or \n"
  (or (match #"\r?\n|\r")
      (adv-err-pos "expected end of line terminator or end of input")))

(defatomic eol*
  "Match a zero or more line terminator \r\n, \r, or \n"
  (match #"(\r?\n|\r)*"))

(defatomic eol+
  "Match one or more end of line terminators \r\n, \r or \n"
  (or (match #"(\r?\n|\r)+")
      (adv-err-pos "expected end of line terminator or end of input")))

;; =======================================================================
;; 
;;                          Grammar Creation
;;                          
;; =======================================================================

(def ^{:dynamic true
       :doc "The map to use when a grammar's :memoize? key is true.
Each key will be:
  ['rule-name position-before-rule-is-executed]
Each associated value will be:
  [parse-result position-after-rule-is-executed]
The result of the rule is cached regardless if it succeeds or not. Initially
unbound."}
  *grammar-rule-cache*)

(def ^:dynamic *trace-depth*)
(def ^:dynamic *trace-indent*)
(def ^{:dynamic true
       :doc "Rule name to total-time-in-body map. Must be bound as an atom so
the macro with-profile can modify it. Initially unbound."}
  *rule-profile-map*)

;; TODO: Heuristic sloth column
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

(defn print-trace-enter
  "with-trace helper"
  [rule-name]
  (let [substring (with-out-str
                    (print (apply str (repeat *trace-depth* " ")))
                    (print (name rule-name))
                    (print ": ")
                    (print (re-find #"(?m)^.*$"
                                    (.replaceAll
                                     (re-matcher #"\t"
                                                 (subs *text-to-parse*
                                                       *cur-pos*))
                                     "        "))))
        sslen (count substring)]
    (println (subs substring 0 (min sslen 79)))))

(defn print-trace-exit
  "with-trace helper"
  [rule-name parse-result]
  (let [substring (with-out-str
                    (print (apply str (repeat *trace-depth* " ")))
                    (print (name rule-name))
                    (print " => ")
                    (pr parse-result))
        sslen (count substring)]
    (println (subs substring 0 (min sslen 79)))))

(defmacro with-trace
  "defgrammar helper to print a trace of the parse

A term width of 80 columns is assumed. If *trace-depth* exceeds this, nothing
is printed."
  [fn-name & body]
  `(do
     (set! *trace-depth* (+ *trace-depth* *trace-indent*))
     (when (< *trace-depth* 80)
       (print-trace-enter '~fn-name))
     (let [parse-result# (do ~@body)]
       (when (< *trace-depth* 80)
         (print-trace-exit '~fn-name parse-result#))
       (set! *trace-depth* (- *trace-depth* *trace-indent*))
       parse-result#)))

(defn- memoize-rule
  "Wrap the body of a (quoted) rule in code that caches the rule's result.

Each cache map key is: [position-before-parse rule-name].
Each value is: [parse-result position-after-parse]"
  [rule memoize?]
  (if (or (= memoize? true)
          (and (set? memoize?)
               (memoize? (first rule))))
    ;; cache the result
    (let [rule-name (first rule)]
      `(~rule-name
	~(second rule)          ; ARG-RULE will ensure this is always a vector
	(if-let [[cached-result# pos#]
		 (@*grammar-rule-cache* ['~rule-name *cur-pos*])]
	  ;; return the cached result
	  (do (set! *cur-pos* pos#) cached-result#)
	  ;; else parse and cache the result
	  (let [cp# *cur-pos*
                parse-result# (do ~@(nnext rule))]
	    (swap! *grammar-rule-cache*
                   assoc ['~rule-name cp#] [parse-result# *cur-pos*])
	    parse-result#))))
    ;; else rule passes through untouched
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

(defn- find-bad-keyarg
  "Return the first key in m (a map) not found in valid-keys (a vector) .
If no key found, return nil."
  [m valid-keys]
  (loop [ks (keys m)]
    (if (nil? ks)
      nil
      (if (some #{(first ks)} valid-keys)
        (recur (next ks))
        (first ks)))))

(defn- chk-grammar-args
  "Perform defgrammar argument checks"
  [name doc-string key-args rule rules start-rule]
  (if-let [bad-key (find-bad-keyarg key-args [:skipper :start-rule :init-fn
                                              :match-case? :print-err?
                                              :memoize? :trace? :inherit?
                                              :profile? :ppfn])]
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
    (println) (flush)))

(defn- emit-inherited-grammar
  "defgrammar helper to emit an inherited grammar as a function"
  [name doc-string start-rule skipper trace? rule rules]
  `(defn ~name
     ~doc-string
     []
     (binding [*skip-fn* ~skipper
               *skip?* ~(and skipper true)]
       (letfn [~@(map (fn [r]
                        (defgrammar-helper r false trace? false))
                   (conj rules rule))]
         (~start-rule)))))

(defn- emit-grammar
  "defgrammar helper to emit a grammar as a function"
  [name doc-string skipper start-rule init-fn match-case? print-err? memoize?
   trace? profile? ppfn rule rules]
  `(defn ~name
     ~doc-string
     [text#]
     (try
       (binding [*text-to-parse* text#
                 *cur-pos* 0
                 *err-pos* 0
                 *err-msg* ""
                 *end-pos* (count text#)
                 *skip-fn* ~skipper
                 *skip?* ~(and skipper true)
                 *char=* ~(if match-case?
                            'char-case=
                            'char=)
                 *grammar-rule-cache* (atom {})
                 *trace-indent* 2
                 *trace-depth* -2
                 *rule-profile-map* (atom {})]
         (letfn [~@(map #(defgrammar-helper % (and (not= (first %)
                                                         start-rule)
                                                   memoize?)
                           trace? profile?)
                     (conj rules rule))]
           ~(when init-fn
              `(~init-fn))
           (if-let [result# (~start-rule)]
             (do
               (when ~profile?
                 (print-profile-info))
               (~ppfn result#))
             (when ~print-err?
               (spep (assoc (offset->line-number *err-pos* *text-to-parse*)
                       :msg *err-msg*))))))
       (catch ParserException e#))))

;; TODO: Allow start-rule selection at run time.
;;       RESOLVE will not work with functions defined with LETFN.
(defmacro defgrammar
  "Expand to the function `name' that expects one argument: a string of the
text to parse. doc-string is not optional. The third argument must be a
possibly empty vector of key val pairs:

.--------------+-------------------------------------------------------------.
|     key      |                          val                                |
|--------------+-------------------------------------------------------------|
| :init-fn     | A function that takes no arguments called just prior to     |
|              | calling the first rule.                                     |
|              | Default: nil                                                |
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
| :memoize?    | true to define a packrat parser where the result of each    |
|              | rule is cached. This may or may not improve the performance |
|              | of parser. It depends on the grammar.                       |
|              | The value may also be a set in which case only rule names   |
|              | specified in the set will be memoized.                      |
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
|              | text to parse. All keys except :start-rule and :trace? are  |
|              | ignored.                                                    |
|              | Default: false                                              |
| :ppfn        | Post Parse Function. Call this function on a successful     |
|              | parse with the parse result as its only argument. The       |
|              | result of this fn will be the result of the parser.         |
|              | Default: identity                                           |
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

  (defgrammar infix-expr
    \"Syntax check an algebraic infix expressions.
      This doc string is required\"
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
    (number (g #\"\\d+\"))
    (variable (g #\"[a-z]\")))

  (infix-expr \"4*[2*a*(a+3)+6*(4-a)]+5*a**2\")  => true
  (infix-expr \"4*[2*a*(a[+3)+6*(4-a)]+5*a**2\") => indicate the stray ["
  [name doc-string
   [& {:keys [init-fn skipper start-rule match-case? print-err? memoize?
              trace? inherit? profile? ppfn]
       :as key-args
       :or {skipper 'wsp-skipper, start-rule 'start,
            match-case? false,print-err? false, memoize? false, trace? false,
            inherit? false, profile? false, ppfn identity}}]
   rule & rules]
  ;; some error handling
  (chk-grammar-args name doc-string key-args rule rules start-rule)
  ;; emit code
  (if inherit?
    ;; init-fn match-case?, print-err?, profile?, memoize?, and ppfn are
    ;; ignored for now...
    (emit-inherited-grammar name doc-string start-rule skipper trace? rule
                            rules)
    (emit-grammar name doc-string skipper start-rule init-fn match-case?
                  print-err? memoize? trace? profile? ppfn rule rules)))

;; =======================================================================
;; 
;;                          Testing Foo
;;                          
;; =======================================================================

(def whitespace? #{\space \newline \return \tab \formfeed \u000b})

(defn wsp-skipper
  "Skip function to ignore all subsequent [ \\r\\n\\t\\f\\v].
Returns true."
  []
  (while (and (< *cur-pos* *end-pos*)
              (whitespace? (nth *text-to-parse* *cur-pos*))
              (set! *cur-pos* (inc *cur-pos*))))
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
offset->line-number. By default it will contain the keys :line and :column.
These two are required. Other accepted keys that can be merged with m are
:tag, :msg, and :omit-all?.

Example output:

     .-------------------------- :tag = \"Parse Error\"
     |                     .---- :msg = \"expected `foo'\"
     |                     |
     v                     v
Parse Error: line 1: expected `foo'
bar
^

Default values for these are:
:tag \"Parse Error\"
:msg \"\"

The complete first line can be omitted with the key :omit-all?. In this case
:tag, and :msg will be ignored and just the erroneous line will be printed:
bar
^"
  [m]
  (let [spacing (apply str (repeat (:column m) " "))]
    (when-not (m :omit-all?)
      (printf "%s: line %d: %s\n"
              (get m :tag "Parse Error")
              (:line m)
              (get m :msg "")))
    (printf "%s\n%s^\n" (:text m) spacing)))

(defn parse-error
  "Call from within a grammar.
This will immediately exit the parser."
  [pos msg & {:keys [tag]}]
  (spep (merge (offset->line-number pos *text-to-parse*)
               {:tag tag :msg msg}))
  (throw (ParserException.)))

(defmacro tparse
  "repl test utility. Parse text with the single parsing form.
The following key args may be given:
:skip?
  true to skip all whitespace
  default: true
:print-err?
  true to display a message on parse error
  default: true
:match-case?
  true if literal strings and characters are to match case sensitively
  default: false
:show-pos?
  true to indicate the current parse position after returning. This will
  happen on success or failure.

NOTE: print-err? and show-pos? are mutually exclusive

Return non-nil on successful parse."
  [text form & {:keys [skip? print-err? match-case? show-pos?]
                :or {skip? true, print-err? true, match-case? false,
                     show-pos? false}}]
  (when (and print-err? show-pos?)
    (throw (RalikException.
            (str ":print-err? and :show-pos? are mutually exclusive."
                 "\nSet one or neither to true"))))
  `(binding [*cur-pos* 0
             *text-to-parse* ~text
             *end-pos* ~(count text)
             *err-pos* 0
             *err-msg* "no idea"
             *skip-fn* wsp-skipper
             *skip?* ~skip?
             *char=* ~(if match-case?
                        'char-case=
                        'char=)]
     (let [result# ~form]
       (cond
        (and (not result#) ~print-err?)
        (do
          (spep (merge (offset->line-number *err-pos* *text-to-parse*)
                       {:msg *err-msg*}))
          result#)
        ~show-pos?
        (do
          (spep (merge (offset->line-number *cur-pos* *text-to-parse*)
                       {:omit-all? true}))
          (boolean result#))
        :else
        result#))))

(defmacro tparse2
  "For use with clojure.test
Unlike tparse, this macro will accept one or more forms.
Does not print a parse error message."
  [text form & forms]
  `(tparse ~text ~(list* '<g form forms) :print-err? false))

(defmacro tparse3
  "repl test utility for checking backtracking
On success or failure, show the current parse position.
Return only the two values true or false."
  [text form]
  `(tparse ~text ~form :print-err? false :show-pos? true))

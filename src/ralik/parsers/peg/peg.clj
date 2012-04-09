;;; peg.clj
;;;
;;; Friday, April  6 2012

(ns ^{:doc "PEG parser"}
    ralik.parsers.peg.peg
    (:use ralik.core)
    ;; (:require [clojure.contrib.string :as string])
    ;; (:use [clojure.walk :only [postwalk]])
    ;; (:use [clojure.pprint :only [pprint]])
    )

(declare make-skipper)

(defgrammar peg
  "Parse a PEG file. Return a map.

The keys are the production rule names, as strings. The values are the right
hand side. Peg operators and syntax are transformed:

       'x'    => \"x\"
       \"x\"    => \"x\"
        .     => _
      [a-z]   => (peg-cc \"[a-z]\")     character class
       Foo    => (peg-nt \"Foo\")       non-terminal
    'x' / 'y' => (peg-or \"x\" \"y\")
    ('x' 'y') => (peg-g \"x\" \"y\")      explicit group*
       !'x'   => (peg-! \"x\")
       &'x'   => (peg-& \"x\")
       'x'*   => (peg-* \"x\")
       'x'+   => (peg-+ \"x\")
       'x'?   => (peg-? \"x\")

* A squence of two or more terminal or non-terminal symbols is a list:
    x <- y z => {\"x\" (\"y\" \"z\")}"
  [:skipper (make-skipper)
   :trace? false
   :print-err? true]
  ;;
  (start
   (let [rules (atom {})]
     (g (g* (awhen (definition)
              #(if (get @rules (first %))
                 (throw (Exception.
                         (str "duplicate production in PEG: " (first %))))
                 (swap! rules conj %))))
        eoi @rules)))
  ;; x <- y
  (definition
    (let [id (atom nil)]
      (g (reset! id (identifier))
         "<-"
         (awhen (expression)
           #(vector @id %)))))
  ;; x / y
  (expression
   (let [res (atom [])]
     (g_ (awhen (peg-sequence)
           #(swap! res conj %))
         \/)
     (if (empty? @res)
       nil
       (if (next @res)
         (cons 'peg-or @res)
         (first @res)))))
  ;; x y z
  (peg-sequence
   (let [res (atom [])]
     (g+ (awhen (prefix)
           #(swap! res (fn [x] (conj x %)))))
     (if (next @res)
       (seq @res)
       (first @res))))
  ;; !x
  (prefix
   (let [op (atom nil)]
     (g (g? (awhen (lex (g| \& \!))
              #(reset! op (symbol (str "peg-" %)))))
        (awhen (suffix)
          #(if @op
             (list @op %)
             %)))))
  ;; x*
  (suffix
   (let [pri (atom nil)]
     (g (reset! pri (primary))
        (g? (awhen (lex (g| \? \* \+))
              #(swap! pri (fn [x] (list (symbol (str "peg-" %)) x))))))
     @pri))
  ;; 
  (primary
   (let [ret (atom nil)]
     (g| (g (awhen (identifier)
              #(reset! ret
                       (list 'peg-nt %))) (g! "<-") @ret)
         (g \( (awhen (expression) #(reset! ret (list 'peg-g %))) \) @ret)
         (literal)
         (peg-char-class)
         (g \. '_))))
  ;; foo
  (identifier
   (g (lex #"[a-zA-Z_][a-zA-Z0-9_]*")))
  ;; 'foo' or "foo" 
  (literal
   (let [ret (atom [])]
     (g| (g \' (-skip (g* (g! \') (awhen (peg-char)
                                    #(swap! ret conj %)))
                      \')
            (apply str @ret))
         (g \" (-skip (g* (g! \") (awhen (peg-char)
                                    #(swap! ret conj %)))
                      \")
            (apply str @ret)))))
  ;; [a-z]
  (peg-char-class
   (let [cc (atom [])]
     (g \[ (-skip (g+ (g! \]) (awhen (lex (peg-char-range))
                                #(swap! cc conj %))))
        \] (when (seq @cc)
             (list 'peg-cc (apply str @cc))))))
  ;; a-z
  (peg-char-range
   (g| (g (peg-char) \- (g! \]) (peg-char))
       (peg-char)))
  ;; 
  (peg-char
   (lex
    (g| (g \\ (g| \n \r \t \f \'\" \] \[ \\
                  #"[0-2][0-7][0-7]"
                  #"[0-7][0-7]?"))
        (g (g! \\) _)))))

(defn make-skipper
  "Skip all whitespace and #-slyle single-line comments"
  []
  (fn []
    (while (and (< *cur-pos* *end-pos*)
                (or
                 ;; whitespace
                 (and
                  (some #{(nth *text-to-parse* *cur-pos*)}
                        [\space \newline \return \tab \formfeed])
                  (set! *cur-pos* (inc *cur-pos*)))
                 ;; single-line comment
                 (let [m (re-matcher  #"(?m)#.*?$"
                                      (subs *text-to-parse* *cur-pos*))]
                   (when (.lookingAt m)
                     (set! *cur-pos* (+ *cur-pos* (.end m))))))))
    true))
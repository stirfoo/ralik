;;; parser.clj
;;;
;;; Sunday, May  26 2013

(ns ^{:doc "Define a PL/0 parser/evaluator.
http://en.wikipedia.org/wiki/PL/0"}
  parsers.pl-0.parser
  (:use ralik.core)
  (:use [ralik.atomics :only [eoi uint10 wsp+]])
  (:use [clojure.pprint :only [pprint]])
  (:import [ralik RalikException]))

;; Symbols and Scope

(defn pproc? [s] (= (:type s) :PROCEDURE))
(defn pconst? [s] (= (:type s) :CONST))
(defn pvar? [s] (= (:type s) :VAR))

(defprotocol Foo
  (add-sym [this s])
  (resolve-sym [this s]))

(defrecord Symbol [name pos type scope])

(defrecord Scope [name symbol-map parent]
  Foo
  (add-sym [this s]
    ;; (println "adding" (:name s) "to" name)
    (swap! symbol-map assoc (:name s) (assoc s :scope name)))
  (resolve-sym [this s]
    ;; (println "resolving" {:name (:name s) :type (:type s)} "in" name)
    (@symbol-map (:name s))))

(defn init-scope
  "Define the-scope as a Global scope with no defined symbols and no parent."
  []
  (def the-scope (atom (Scope. "Global" (atom {}) nil))))

(defn lookup [s]
  "Resolve the Symbol s in the current scope. Return nil if not found."
  (loop [scope @the-scope]
    (when scope
      (if-let [dsym (resolve-sym scope s)]
        dsym
        (recur (.parent scope))))))

(defn push-scope
  "Set the-scope to a new Scope with the-scope as its parent"
  [name]
  (swap! the-scope #(Scope. name (atom {}) %)))

(defn pop-scope
  "Set the-scope to the-scope's parent"
  []
  (when (.parent @the-scope)
    (swap! the-scope #(.parent %)))
  true)

(defn define-symbol
  "Add the Symbol s to the current scope."
  [s]
  (when-let [dsym (lookup s)]
    (when (pproc? dsym)
      (parse-error (:pos s) "cannot redefine a procedure"
                   :tag "PL/0"))
    (when (pproc? s)
      (parse-error (:pos s) (str "cannot redefine a " (name (:type dsym))
                                 " as a procedure")
                   :tag "PL/0"))
    (when (= (:scope dsym) (:name @the-scope))
      (parse-error (:pos s) (str "cannot redefine a " (name (:type dsym))
                                 " in the same scope") :tag "PL/0")))
  (.add-sym @the-scope s))

(defn pl0-sym [s]
  "Return a symbol with `pl-' prefix."
  (symbol (str "pl0-" (:name s))))

;; Rule Handlers
;; Code emitters with some compile-time type checking

(defn on-const
  "Handle: CONST x=1, y=2;
In this case vars-vals will be ([Symbol 1] [Symbol 2]) and the return value
will be '(let [x 1 y 2])"
  [& vars-vals]
  (list 'let
        (into [] (mapcat (fn [[x y]]
                           (define-symbol (assoc x :type :CONST))
                           [(pl0-sym x) y])
                         vars-vals))))

(defn on-var
  "Handle: VAR a, b, c;
In this case var-syms will be (Symbol Symbol Symbol) and the return value will
be '(with-local-vars [a nil b nil c nil])"
  [& var-syms]
  (list 'with-local-vars
        (into [] (interleave (map #(do
                                     (define-symbol (assoc % :type :VAR))
                                     (pl0-sym %))
                                  var-syms)
                             (repeat nil)))))

(defn on-proc-def
  "Handle: PROCEDURE Foo;
In this case add Foo to the current scope as a procedure, push a new empty
scope, and return s (The function name as a Symbol)"
  [s]
  (define-symbol (assoc s :type :PROCEDURE))
  (push-scope (:name s))
  s)

(defn on-assign
  "Handle: x := e
Ensure x is defined VAR in scope and return '(var-set x e)"
  [s e]
  (if-let [dsym (lookup s)]
    (do
      (when-not (pvar? dsym)
        (parse-error (:pos s) (str "cannot assign to a " (name (:type dsym)))
                     :tag "PL/0"))
      (list 'var-set (pl0-sym s) e))
    (parse-error (:pos s) "unknown identifier" :tag "PL/0")))

(defn on-call
  "Handle: CALL foo
Ensure foo is a PROCEDURE in scope and return '(foo)"
  [s]
  (if-let [dsym (lookup s)]
    (do
      (when-not (pproc? dsym)
        (parse-error (:pos s) " not a procedure" :tag "PL/0"))
      (list (pl0-sym s)))
    (parse-error (:pos s) "unknown procedure" :tag "PL/0")))

(defn on-?
  "Handle: ?foo
Ensure foo is a VAR in scope and return '(var-set foo op) where op is the read
form."
  [op s]
  (if-let [dsym (lookup s)]
    (do
      (when-not (pvar? dsym)
        (parse-error (:pos s) (str "connot read into a " (name (:type dsym)))
                     :tag "PL/0"))
      (list 'var-set (pl0-sym s) op))
    (parse-error (:pos s) "unknown identifier" :tag "PL/0")))

(defn on-factor-ident
  "Handle a symbol in an expression
Ensure the symbol is a VAR or CONSTANT in scope and return:
1. a var-get form that checks at run time if the var has been initialized by
   the PL/0 program.
2. s if a CONSTANT" 
  [s]
  (if-let [dsym (lookup s)]
    (case (:type dsym)
      :PROCEDURE
      (parse-error (:pos s) "cannot take the value of a procedure"
                   :tag "PL/0")
      :VAR
      ;; run time check
      ;; Can only be used with eval in :ppfn because it depends on dynamic
      ;; vars created by defgrammar. If the emitted code from pl-0 were
      ;; written to a file then loaded, it would not run.
      `(if-let [res# (var-get ~(pl0-sym s))]
         res#
         (parse-error ~(:pos s) "uninitialized VAR" :tag "PL/0"))
      :CONST
      (pl0-sym s)
      :unknown
      (parse-error (:pos s)
                   (str "internal error, " (:name s) " has type :unknown")
                   :tag "PL/0"))
    (parse-error (:pos s) "unknown identifier" :tag "PL/0")))

;; Builtin PL/0 read fn: ?x

(defn pl0-read-int
  "Read and return an integer from *in*"
  []
  (loop [x (read-line)]
    (if (re-find #"^[+-]?\d+$" x)
      (if (= (first x) \+)
        (Integer/parseInt (subs x 1))
        (Integer/parseInt x))
      (do
        (println x "is not an integer,"
                 " try again")
        (recur (read-line))))))

;; Parser

(def opmap {"=" '=, "#" 'not=, "<=" '<=, "<" '<, ">=" '>=, ">" '>, "+" '+,
            "-" '-, "*" '*, "/" 'quot, "!" 'println,"odd" 'odd?, "" :empty,
            "?" '(pl0-read-int)})

(defn getop
  [x]
  (get opmap (cond
              (string? x) (.toLowerCase x)
              (char? x) (str x)
              :else x)
       x))

(def reserved-word? #{"CONST" "VAR" "PROCEDURE" "CALL" "BEGIN" "END" "IF"
                      "THEN" "WHILE" "DO" "ODD"})

(defatomic kw-terminator
  "Define characters that cannot immediately follow a PL/0 keyword"
  (match #"[a-zA-Z0-9]"))

(defgrammar pl-0-skipper
  "Skip whitespace and (* style *) comments."
  [:start-rule Skip
   :skipper nil
   :inherit? true]
  (Skip (g* (g| wsp+
                (g "(*" (g* (g- <_ "*)")) "*)")))))

(defgrammar pl-0
  "Parse and evaluate Wirth's PL/0."
  [:start-rule Program
   :skipper pl-0-skipper
   :match-case? true
   :trace? false
   :print-err? true
   :ppfn #(do
            #_(pprint %)
            (eval %))]
  (Program
   (init-scope)
   (<g 0 (>g (Block) #(concat % [true])) "." eoi))
  (Block (>g (<g? (ConstExpr))
             (<g? (VarExpr))
             (>g* (ProcExpr)
                  #(if (empty? %&)
                     :empty
                     (list 'letfn `[~@%&])))
             (Statement)
             #(let [x (reverse %&)]
                (reduce (fn [x y]
                          (if (= y :empty)
                            x
                            (concat y [x])))
                        (first x)
                        (next x)))))
  (ConstExpr (>g 1 (kw :CONST) (<g_ 0 (>g (Ident) "=" uint10
                                          #(vector %1 %3))
                                    ",")
                 ";"
                 on-const))
  (VarExpr (>g 1 (kw :VAR) (<g_ 0 (Ident) ",") ";"
               on-var))
  (ProcExpr (>g #{1 3} (kw :PROCEDURE)
                (>g (Ident) on-proc-def)
                ";" (Block) ";"
                (pop-scope)
                #(if (= %2 :empty)
                   (list (pl0-sym %1) [])
                   (list (pl0-sym %1) [] %2))))
  (Statement (<g| (AssgnStmt)
                  (CallStmt)
                  (BeginStmt)
                  (IfStmt)
                  (WhileStmt)
                  (IOStatment)
                  :empty))
  (AssgnStmt (>g #{0 2} (Ident) ":=" (Expression)
                 on-assign))
  (CallStmt (>g 1 (kw :CALL) (Ident)
                on-call))
  (BeginStmt (>g 1 (kw :BEGIN) (<g_ 0 (Statement) ";") (kw :END)
                 #(if (next %&)
                    (list* 'do %&)
                    (first %&))))
  (IfStmt (>g #{1 3} (kw :IF) (Condition) (kw :THEN) (Statement)
              #(list 'when %1 %2)))
  (WhileStmt (>g #{1 3} (kw :WHILE) (Condition) (kw :DO) (Statement)
                 #(list 'while %1 %2)))
  (IOStatment (<g| (>g (>g \? getop) (Ident)
                       on-?)
                   (>g (>g \! getop) (Expression)
                       #(list %1 %2))))
  (Condition (<g| (>g (>kw :ODD getop) (Expression)
                      #(list %1 %2))
                  (>g (Expression)
                      (>g| "=" "#" "<=" "<" ">=" ">" getop)
                      (Expression)
                      #(list %2 %1 %3))))
  (Expression (>g_ (Term) (>g #"[+-]" getop)
                   #(reduce (fn [x [op y]]
                              (list op x y))
                            %&)))
  (Term (>g_ (Factor) (>g #"[*/]" getop)
             #(reduce (fn [x [op y]]
                        (list op x y))
                      %&)))
  (Factor (<g| (>g (Ident) on-factor-ident)
               uint10
               (<g 1 "(" (Expression) ")")
               (>g (>g #"[+-]" getop) (Factor)
                   #(list %1 %2))))
  (Ident (>g #"[a-zA-Z][a-zA-Z0-9]*"
             #(when-not (reserved-word? %)
                (Symbol. % (- *cur-pos* (count %)) :unknown :unknown)))))

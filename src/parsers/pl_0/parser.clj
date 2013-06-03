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

(defn pproc? [s] (= (:type s) :procedure))
(defn pconst? [s] (= (:type s) :const))
(defn pvar? [s] (= (:type s) :var))

(defprotocol Foo
  (add-sym [this s])
  (resolve-sym [this s]))

(defrecord Symbol [name pos type])

(deftype Scope [name ^{:volatile-mutable true} symbol-map parent]
  Foo
  (add-sym [this s]
    ;; (println "adding" (:name s) "to" name)
    (set! symbol-map (assoc symbol-map (:name s) s)))
  (resolve-sym [this s]
    ;; (println "resolving" {:name (:name s) :type (:type s)} "in" name)
    (symbol-map (:name s))))

(defn init-scope
  []
  (def the-scope (atom (Scope. "Global" {} nil))))

(defn lookup [s]
  (loop [scope @the-scope]
    (when scope
      (if-let [dsym (resolve-sym scope s)]
        dsym
        (recur (.parent scope))))))

(defn push-scope
  [name]
  (swap! the-scope #(Scope. name {} %)))

(defn pop-scope
  []
  (when (.parent @the-scope)
    (swap! the-scope #(.parent %)))
  true)

(defn define-symbol
  [s]
  (when-let [dsym (lookup s)]
    (when (pproc? dsym)
      (parse-error (:pos s) "PL/0: cannot redefine a procedure"))
    (when (pproc? s)
      (parse-error (:pos s) "PL/0: cannot redefine a " (name (:type dsym))
                   " as a procedure")))
  (.add-sym @the-scope s))

(defn pl0-sym [s]
  (symbol (str "pl0-" (:name s))))

;; Rule Handlers

(defn on-const
  [& vars-vals]
  (list 'let
        (into [] (mapcat (fn [[x y]]
                           (define-symbol (assoc x :type :constant))
                           [(pl0-sym x) y])
                         vars-vals))))

(defn on-var
  [& var-syms]
  (list 'with-local-vars
        (into [] (interleave (map #(do
                                     (define-symbol (assoc % :type :var))
                                     (pl0-sym %))
                                  var-syms)
                             (repeat 0)))))

(defn on-proc-def
  [s]
  (define-symbol (assoc s :type :procedure))
  (push-scope (:name s))
  s)

(defn on-assign
  [s e]
  (if-let [dsym (lookup s)]
    (do
      (when-not (pvar? dsym)
        (parse-error (:pos s) (str "PL/0: cannot assign to "
                                   (name (:type dsym)))))
      (list 'var-set (pl0-sym s) e))
    (parse-error (:pos s) (str "PL/0: unknown identifier"))))

(defn on-call
  [s]
  (if-let [dsym (lookup s)]
    (do
      (when-not (pproc? dsym)
        (parse-error (:pos s) (str "PL/0: not a procedure")))
      (list (pl0-sym s)))
    (parse-error (:pos s) (str "PL/0: unknown procedure"))))

(defn on-?
  [op s]
  (if-let [dsym (lookup s)]
    (do
      (when-not (pvar? dsym)
        (parse-error (:pos s) (str "PL/0: connot read into a "
                                   (name (:type dsym)))))
      (list 'var-set (pl0-sym s) op))
    (parse-error (:pos s) (str "PL/0: unknown identifier"))))

(defn on-factor-ident
  [s]
  (if-let [dsym (lookup s)]
    (case (:type dsym)
      :procedure
      (parse-error (:pos s) (str "PL/0: cannot take the value of a procedure"))
      :var
      (list 'var-get (pl0-sym s))
      :constant
      (pl0-sym s)
      :undefined
      (parse-error (:pos s)
                   (str "PL/0: internal error, " (:name s)
                        " has type :undefined")))
    (parse-error (:pos s) (str "PL/0: unknown identifier"))))

;; Parser

(def opmap {"=" '=, "#" 'not=, "<=" '<=, "<" '<, ">=" '>=, ">" '>, "+" '+,
            "-" '-, "*" '*, "/" 'quot, "!" 'println,
            "?" '(Integer/parseInt (read-line)), "odd" 'odd?,"" :empty})

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
            #_(pprint @the-scope)
            #_(pprint %)
            (eval %))]
  (Program
   (init-scope)
   (<g 0 (>g (Block) #(concat % [true])) "." eoi))
  (Block (>g (>g? (ConstExpr) getop)
             (>g? (VarExpr) getop)
             (>g* 0 (ProcExpr)
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
                (Symbol. % (- *cur-pos* (count %)) :undefined)))))

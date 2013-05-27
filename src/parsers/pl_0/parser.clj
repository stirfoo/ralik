;;; parser.clj
;;;
;;; Sunday, May  26 2013

(ns ^{:doc "Define a PL-0 parser/evaluator.
http://en.wikipedia.org/wiki/PL/0"}
  parsers.pl-0.parser
  (:use ralik.core)
  (:use [clojure.pprint :only [pprint]])
  (:import [ralik RalikException]))

(def opmap {"=" '=
            "#" 'not=
            "<=" '<=
            "<" '<
            ">=" '>=
            ">" '>
            "+" '+
            "-" '-
            "*" '*
            "/" 'quot
            "!" 'println
            "?" '(Integer/parseInt (read-line))
            "odd" 'odd?
            "if" 'when
            "while" 'while
            "" :empty
            :g?-failed :empty})

(defn getop
  [x]
  (get opmap (cond
              (string? x) (.toLowerCase x)
              (char? x) (str x)
              :else x)
       x))

(def reserved-word? #{"const" "var" "procedure" "call" "begin" "end" "if"
                      "then" "while" "do" "odd"})

(defrecord Constant [name value])

(defn vset
  "Handle x := y statement."
  [x y]
  (cond
   (= (type x) Constant) (throw (Exception.
                                 (str "PL/0 can't assign to constant `"
                                      (:name x) "'")))
   (var? x) (var-set x y)
   :else (throw (Exception. "PL/0 illegal assignment"))))

(defn vget
  "Return the value of x.
Called each time a var or constant is referenced."
  [x name]
  (cond
   (var? x) (var-get x)
   (= (type x) Constant) (:value x)
   :else (throw (Exception. (str "PL/0 undefined var or constant `"
                                 name "'")))))

(defgrammar pl-0-skipper
  "Skip whitespace and (* style *) comments."
  [:start-rule Skip
   :inherit? true]
  (Skip (skip- (g* (g| wsp
                       (g "(*" (g* (g- _ "*)")) "*)"))))))

(defgrammar pl-0
  "Parse and evaluate Wirth's PL/0."
  [:start-rule Program
   :skipper pl-0-skipper
   :match-case? false
   :trace? false
   :print-err? true
   :ppfn eval]
  (Program (<g 0 (Block) "." eoi))
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
  (ConstExpr (>g 1 (kw :const) (<g_ 0 (>g (Ident) "=" (Num)
                                          #(vector %1 %3))
                                    ",")
                 ";"
                 #(list 'let (into [] (mapcat (fn [[x y]]
                                                [x (Constant. (name x) y)])
                                              %&)))))
  (VarExpr (>g 1 (kw :var) (<g_ 0 (Ident) ",") ";"
               #(list 'with-local-vars
                      (into [] (interleave %& (repeat nil))))))
  (ProcExpr (>g [1 4] (kw :procedure) (Ident) ";" (Block) ";"
                #(let [[id _ b] %&]
                   (if (empty? b)
                     (list id [])
                     (list id [] b)))))
  (Statement (<g| (AssgnStmt)
                  (CallStmt)
                  (BeginStmt)
                  (IfStmt)
                  (WhileStmt)
                  (IOStatment)
                  :empty))
  (AssgnStmt (>g (Ident) ":=" (Expression)
                 #(list vset %1 %3)))
  (CallStmt (>g 1 (kw :call) (Ident)
                #(list %)))
  (BeginStmt (>g 1 (kw :begin) (<g_ 0 (Statement) ";") (kw :end)
                 #(if (next %&)
                    (list* 'do %&)
                    (first %&))))
  (IfStmt (>g (>kw :if getop) (Condition) (kw :then) (Statement)
              #(list %1 %2 %4)))
  (WhileStmt (>g (>kw :while getop) (Condition) (kw :do) (Statement)
                 #(list %1 %2 %4)))
  (IOStatment (<g| (>g (>g \? getop) (Ident)
                       #(list vset %2 %1))
                   (>g (>g \! getop) (Expression)
                       #(list %1 %2))))
  (Condition (<g| (>g (>kw :odd getop) (Expression)
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
  (Factor (<g| (>g (Ident) #(list vget % (name %)))
               (Num)
               (<g 1 "(" (Expression) ")")
               (>g (>g #"[+-]" getop) (Factor)
                   #(list %1 %2))))
  (Ident (>g #"[a-zA-Z][a-zA-Z0-9]*"
             #(when-not (reserved-word? (.toLowerCase %))
                (symbol %))))
  (Num (>lex #"\d+" Integer/parseInt)))

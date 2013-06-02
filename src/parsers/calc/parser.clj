;;; parser.clj
;;;
;;; Sunday, April  13 2012

(ns ^{:doc "Define an infix calculator."}
  parsers.calc.parser
  (:use ralik.core)
  (:use [clojure.pprint :only [pprint]])
  (:import [ralik RalikException]))

(defgrammar calc
  "A toy calculator. Parse and evaluate an infix expression with numbers,
assignable variables, and a handful of functions.

Partial PEG (NUMBER and VARIABLE are terminals):
  Start    <- Decl* SumExpr
  Decl     <- Variable \"=\" SumExpr \";\"
  SumExpr  <- MulExpr ((\"+\" / \"-\") MulExpr)*
  MulExpr  <- PowExpr ((\"*\" / \"/\" / \"%\") PowExpr)*
  PowExpr  <- Primary (\"**\" PowExpr)?
  Thunk    <- \"random\" \"(\" \")\"
  UnaryFn  <-
   (\"abs\" / \"acos\" / \"asin\" / \"atan\" / \"ceil\" / \"cos\" /
    \"cosh\" / \"exp\" / \"floor\" / \"log\" / \"log10\" / \"round\" /
    \"sin\" / \"sinh\" / \"sqrt\" / \"rad\" / \"deg\")
   \"(\" SumExpr \")\"
  BinaryFn <-
   (\"atan2\" / \"pow\"  / \"max\" / \"min\" / \"hypot\")
   \"(\" SumExpr \",\" SumExpr \")\"
  Func     <- Thunk / UnaryFn / BinaryFn
  Primary  <-
    NUMBER /
    Func /
    VARIABLE /
    \"[\" SumExpr \"]\" /
    \"(\" SumExpr \")\" /
    \"[+-]\" Primary

Example:
  user> (require '[parsers.calc.parser :as cp])
  nil
  user> (cp/calc \"x = random(); y = sqrt(x); x * sin(radians(45)) ** y\")
  (let
   [x (Math/random) y (Math/sqrt x)]
   (* x (Math/pow (Math/sin (Math/toRadians 45)) y)))
  0.27703768482683516     ; will not be the same as random() was used
  user>"

  [:start-rule Start
   :print-err? true
   :ppfn #(do (pprint %) (eval %))]
  ;; 
  (Start
   (>g (<g* 0 (Decl)) (SumExpr) eoi
       #(let [[decls expr _] %&]
          (if (empty? decls)
            expr
            (list 'let (vec (apply concat decls))
                  expr)))))
  ;;
  (Decl
   (>g (Variable) "=" (SumExpr) ";"
       #(let [[v _ n _] %&]
          (vector v n))))
  ;; 
  (SumExpr
   (>g_ (MulExpr) #"[+-]"
        #(reduce (fn [x [op y]]
                   (list (symbol op) x y))
                 (first %&)
                 (next %&))))
  ;; 
  (MulExpr
   (>g_ (PowExpr) (>lex #"[*/%]" {"*" '*
                                  "/" '/
                                  "%" 'mod})
        #(reduce (fn [x [op y]]
                   (list op x y))
                 (first %&)
                 (next %&))))
  ;; ** is right-associative 2**3**4 => 2**(3**4)
  (PowExpr
   (>g (Primary) (<g? "**" (PowExpr))
       #(if (= %2 :empty)
          %1
          (list 'Math/pow %1 (second %2)))))
  ;;
  (Thunk
   (>g (>kw :random
            #(symbol (str "Math/" %)))
       "(" ")"
       #(let [[f _ _] %&]
          (list f))))
  ;;
  (UnaryFn
   (>g (>kws :abs :acos :asin :atan :ceil :cosh :cos :exp :floor :log :log10
             :round :sin :sinh :sqrt :tan :tanh :rad :deg
             #(case %
                "rad" 'Math/toRadians
                "deg" 'Math/toDegrees
                (symbol (str "Math/" %))))
       "(" (SumExpr) ")"
       #(let [[f _ a _] %&]
          (list f a))))
  ;; 
  (BinaryFn
   (>g (>kws :atan2 :pow :max :min :hypot
             #(symbol (str "Math/" %)))
       "(" (SumExpr) "," (SumExpr) ")"
       #(let [[f _ a1 _ a2 _] %&]
          (list f a1 a2))))
  ;;
  (Func
   (<g| (Thunk)
        (UnaryFn)
        (BinaryFn)))
  ;; 
  (Primary
   (<g| (Number)
        (Func)
        (Variable)
        (<g 1 "(" (SumExpr) ")")
        (<g 1 "[" (SumExpr) "]")
        (>g #"[+-]" (Primary)
            #(list (symbol %1) %2))))
  ;; 
  (Number
   (>lex #"(0|([1-9][0-9]*))(\.[0-9]+)?([Ee][+-]?[0-9]+)?"
         #(if (some #{\. \e \E} %)
            (Double/parseDouble %)
            (Integer/parseInt %))))
  ;; 
  (Variable
   (>g #"[a-zA-Z][a-zA-Z_]*" symbol)))


;;; parser.clj
;;;
;;; Thursday, May  23 2013

(ns ^{:doc "Define an Extended BNF parser.
Loosely based on ISO/IEC 14977.
http://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form"}
  parsers.ebnf.parser
  (:use ralik.core)
  (:use [clojure.pprint :only [pprint]])
  (:import [ralik RalikException]))

(defgrammar ebnf-skipper
  "Skip whitespace and possibly nested (* comments *)"
  [:start-rule Skip
   :skipper nil
   :inherit? true]
  (Skip (g* (g| #"[ \n\r\t\f\v]" (Comment))))
  (Comment (g "(*" (g* (g| (Comment) (g- <_ "*)"))) "*)")))

(defgrammar ebnf
  "Parse Extended BNF.
Return a vector of ralik rules."
  [:start-rule Syntax
   :skipper ebnf-skipper                ; an inherited grammar
   :trace? false
   :print-err? true
   :ppfn pprint]
  (Syntax (<g 0 (<g+ 0 (SyntaxRule)) eoi))
  (SyntaxRule (>g (MetaIdent) "=" (DefList) ";"
                  #(let [[id _ dl _] %&]
                     (list id dl))))
  (DefList (>g_ 0 (SingleDef) "|"
                #(if (next %&)
                   (list* 'g| %&)
                   (first %&))))
  (SingleDef (>g_ 0 (Term) ","
                  #(if (next %&)
                     (list* 'g %&)
                     (first %&))))
  ;; TODO: handle {'A'}-, which should translate to: (g+ "A")
  (Term (>g (Factor) (<g? 1 "-" (Exception))
            #(let [[f e] %&]
               (if (= e :empty)
                 f
                 (list 'g- f e)))))
  (Exception (Factor))
  (Factor (>g (<g? 0 uint10 "*") (Primary)
              #(let [[n p] %&]
                 (if (= n :empty)
                   p
                   ;; convert 3 * ['x'] to: (rep {:h 3} "x")
                   ;;           instead of: (rep 3 (? "x"))
                   (if (and (seq? p)
                            (= (first p) 'g?))
                     (if (nnext p)
                       (list 'rep {:h n} (cons 'g (next p)))
                       (list 'rep {:h n} (fnext p)))
                     (list 'rep n p))))))
  (Primary (<g| (OptionalSeq) (RepetedSeq) (SpecialSeq) (GroupedSeq)
                (>g (MetaIdent) list) (TerminalStr) (Empty)))
  (Empty (g "" :empty))
  (OptionalSeq (>g 1 "[" (DefList) "]" #(list* 'g? %&)))
  (RepetedSeq (>g 1 "{" (DefList) "}" #(list* 'g* %&)))
  (GroupedSeq (<g 1 "(" (DefList) ")"))
  (TerminalStr (g| (<g 1 "'" (<skip- 0 (<lex (g+ (g- <_ "'")))) "'")
                   (<g 1 "\"" (<skip- 0 (<lex (g+ (g- <_ "\"")))) "\"")))
  (MetaIdent (>lex #"[a-zA-Z][a-zA-Z0-9]*( ?[a-zA-Z0-9]+)*"
                   #(symbol (.replace % " " "-"))))
  (SpecialSeq (>g 1 "?" (<lex (skip- (g* (g- <_ "?")))) "?"
                  #(list 'special (.trim %)))))

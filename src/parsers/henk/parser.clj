;;; parser.clj
;;;
;;; Wednesday, June  5 2013

;; TODO: incomplete

(ns ^{:doc "Define a Henk 2000 parser.
http://www.staff.science.uu.nl/~jeuri101/MSc/jwroorda/"}
  parsers.henk.parser
  (:use ralik.core)
  (:use [ralik.atomics :only [uint10]])
  (:import [ralik RalikException ParserException]))

(def reserved? #{"data" "let" "letrec" "in" "case" "of" "at" "BOX"})

(defatomic kw-termiator "" (match #"[a-zA-Z0-9]"))

(defgrammar henk-skipper
  "Skip
 * all whitespace
 * nested {- multiline comments -}
 * -- single line comments"
  [:start-rule Skip
   :skipper nil
   :trace? false
   :inherit? true]
  (Skip (g* (g| wsp+
                (SComment)
                (MComment))))
  (MComment (g "{-"
               (g* (g- (g| (MComment)
                           <_)
                       "-}"))
               "-}"))
  (SComment (g "--" (g* (g- <_
                            (g| eol eoi))))))

(defgrammar henk
  "Parse Henk 2000"
  [:start-rule Program
   :skipper henk-skipper
   :trace? false
   :print-err? true]
  (Program (g (g* (g| (Tdecl)
                      (Vdecl)))
              eoi))
  (Tdecl (g (kw :data) (Tvar) "=" "{" (g+ (Tvar)) "}"))
  (Vdecl (g| (g (kw :let) "{" (Bind) "}")
             (g (kw :letrec) "{" (g+ (Bind)) "}")))
  (Bind (g (Tvar) "=" (Expr)))
  (Expr (g| (g (g| "\\/"
                   "\\"
                   "|~|"
                   "/\\" )
               (g+ (Tvar)) "." (Expr))
            (g (Bexpr) "->" (Expr))
            (Bexpr)
            (g (Vdecl) (kw :in) (Expr))
            (g (kw :case) (Expr) (kw :of)
               "{" (g+ (Alt)) "}"
               (g? (kw :at) "{" (g* (Aexpr)) "}"))))
  (Bexpr (g+ (Aexpr)))
  (Aexpr (g| (Tvar)
             (Literal)
             "*"
             (kw :BOX)
             (g "(" (Expr) ")")))
  (Tvar (g (Var) (g? ":" (Aexpr))))
  (Var (g| "_"
           (Identifier)))
  (Alt (g (Pat) (g* (Tvar)) "->" (Expr)))
  (Pat (g| (Tvar)
           (Literal)))
  (Literal (g uint10))
  (Identifier (>g #"[a-zA-Z][a-zA-Z0-9]*"
                  #(when-not (reserved? %)
                     %))))
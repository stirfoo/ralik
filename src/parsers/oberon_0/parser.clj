;; parser.clj
;;;
;;; Tuesday, May  28 2013

(ns ^{:doc "Oberon-0 parser"}
  parsers.oberon-0.parser
  (:use ralik.core)
  (:use [clojure.pprint :only [pprint]])
  (:import [ralik RalikException ParserException]))

;; The next three forms ensure MODULE and PROCEDURE names match

(def scope-name-stack (atom []))

(defn push-scope-name
  [sym]
  (swap! scope-name-stack conj sym)
  sym)

(defn pop-scope-name [end-sym ctx]
  (let [start-sym (peek @scope-name-stack)]
    (pop @scope-name-stack)
    (when (not= (:name start-sym) (:name end-sym))
      (parse-error (:pos end-sym)
                   (str ctx " END name should be: " (:name start-sym))))
    end-sym))

(def reserved-word? #{"ARRAY" "BEGIN" "CONST" "DIV" "DO" "ELSE" "ELSIF" "END"
                      "IF" "MOD" "MODULE" "OF" "OR" "PROCEDURE" "RECORD"
                      "THEN" "TYPE" "VAR" "WHILE"})

(defgrammar oberon-0-skipper
  "Skip whitespace and (* style *) comments."
  [:start-rule Skip
   :trace? false
   :memoize? false
   :inherit? true]
  (Skip (skip- (g* (g| #"[ \n\r\t\v\f]+"
                       (g "(*" (g* (g- _ "*)")) "*)"))))))

(defgrammar oberon-0
  "Oberon-0 parser"
  [:start-rule Module
   :skipper oberon-0-skipper
   :match-case? true
   :trace? false
   :memoize? #{Factor}
   :profile? false
   :print-err? true]
  ;; ModuleHeader ModuleBody '.'
  (Module (g (ModuleHeader) (ModuleBody) "." eoi))
  ;; 'MODULE' Ident ';'
  (ModuleHeader (g (kw "MODULE") (>g (Ident) push-scope-name %)
                    ";"))
  ;; Decls ('BEGIN' StatementSeq)? 'END' Ident
  (ModuleBody (g (Decls) (g? (kw "BEGIN") (StatementSeq))
                  (kw "END") (>g (Ident) #(pop-scope-name % "MODULE"))))
  ;; ConstDecl? TypeDecl? VarDecl? (ProcDecl ';')*
  (Decls (g (g? (ConstDecl)) (g? (TypeDecl)) (g? (VarDecl))
             (g* (ProcDecl) ";")))
  ;; 'CONST' (Ident '=' Expr ';')*
  (ConstDecl (g (kw "CONST") (g* (g (Ident) "=" (Expr) ";"))))
  ;; 'TYPE' (Ident '=' Type ';')*
  (TypeDecl (g (kw "TYPE") (g* (g (Ident) "=" (Type) ";"))))
  ;; 'VAR' (IdentList ':' Type ';')*
  (VarDecl (g (kw "VAR") (g* (g (IdentList) ":" (Type) ";"))))
  ;; ProcHeading ';' ProcBody
  (ProcDecl (g (ProcHeading) ";" (ProcBody)))
  ;; 'PROCEDURE' Ident FormalParms?
  (ProcHeading (g (kw "PROCEDURE") (>g (Ident) push-scope-name)
                  (g? (FormalParms))))
  ;; Decls ('BEGIN' StatementSeq)? 'END' Ident
  (ProcBody (g (Decls) (g? (kw "BEGIN") (StatementSeq)) (kw "END")
               (>g (Ident) #(pop-scope-name % "PROCEDURE"))))
  ;; '(' (FPSect (';' FPSect)*)? ')'
  (FormalParms (g "(" (g? (g_ (FPSect) ";")) ")"))
  ;; 'VAR' IdentList ':' Type
  (FPSect (g (g? (kw "VAR")) (IdentList) ":" (Type)))
  ;; Ident / ArrayType / RecordType
  (Type (g| (Ident)
            (ArrayType)
            (RecordType)))
  ;; 'RECORD' FieldList? (';' FieldList?)* 'END'
  (RecordType (g (kw "RECORD") (g_ (g? (FieldList)) ";") (kw "END")))
  ;; IdentList ':' Type
  (FieldList (g (IdentList) ":" (Type)))
  ;; 'ARRAY' Expr 'OF' Type
  (ArrayType (g (kw "ARRAY") (Expr) (kw "OF") (Type)))
  ;; Ident (',' Ident)*
  (IdentList (<g_ 0 (Ident) ","))
  ;; Statement (';' Statement)*
  (StatementSeq (g_ (g? (Statement)) ";"))
  ;; Assign / ProcCall / IfStatement / WhileStatement
  (Statement (g| (Assign)
                 (ProcCall)
                 (IfStatement)
                 (WhileStatement)))
  ;; 'WHILE' Expr 'DO' StatementSeq 'END'
  (WhileStatement (g (kw "WHILE") (Expr) (kw "DO") (StatementSeq) (kw "END")))
  ;; 'IF' Expr 'THEN' StatementSeq
  ;; ElsifStatement*
  ;; ElseStatement?
  ;; 'END'
  (IfStatement (g (kw "IF") (Expr) (kw "THEN") (StatementSeq)
                  (g* (ElsifStatement))
                  (g? (ElseStatement))
                  (kw "END")))
  ;; 'ESLIF' Expr 'THEN' StatementSeq
  (ElsifStatement (g (kw "ELSIF") (Expr) (kw "THEN") (StatementSeq)))
  ;; 'ESLE' StatementSeq
  (ElseStatement (g (kw "ELSE") (StatementSeq)))
  ;; Ident Selector* ActualParms?
  (ProcCall
   (g (Ident) (g* (Selector)) (g? (ActualParms))))
  ;; '(' (Expr (',' Expr)*)? ')'
  (ActualParms (g "(" (g? (g_ (Expr) ",")) ")"))
  ;; Ident Selector* ':=' Expr
  (Assign (g (Ident) (g* (Selector)) ":=" (Expr)))
  ;; SimpleExpr ('=' / '#' / '<=' / '<' / '>=' / '>') SimpleExpr
  (Expr (g (SimpleExpr)
           (g? (g| "=" "#" "<=" "<" ">=" ">")
               (SimpleExpr))))
  ;; [+-]? Term (('+' / '-' / 'OR') Term)*
  (SimpleExpr (g (g? #"[+-]") (g_ (Term) (g| "+" "-" "OR"))))
  ;; Factor (('*' / 'DIV' / 'MOD' / '&') Factor)*
  (Term (g_ (Factor) (g| "*" "DIV" "MOD" "&")))
  ;; Ident Selector* / [0-9] [0-9]* / '(' Expr ')' / '~' Factor
  (Factor (g| (g (Ident) (g* (Selector)))
              uint10
              (g "(" (Expr) ")")
              (g "~" (Factor))))
  ;; '.' Ident / '[' Expr ']'
  (Selector (g| (g "." (Ident))
                (g "[" (Expr) "]")))
  ;; [a-zA-Z] [a-zA-Z0-9]*
  (Ident (>g #"[a-zA-Z][a-zA-Z0-9]*"
             #(when-not (reserved-word? %)
                %))))

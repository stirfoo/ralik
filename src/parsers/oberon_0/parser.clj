;; parser.clj
;;;
;;; Sunday, April  8 2012

;; Works ok, but incomplete

(ns ^{:doc "Oberon-0 parser"}
  parsers.oberon-0.parser
  (:use ralik.core)
  (:import [ralik RalikException]))

(def ^{:private true}*keywords*
  #{"DIV" "MOD" "OR" "IF" "THEN" "ELSIF" "ELSE" "END"
    "WHILE" "DO" "ARRAY" "OF" "RECORD" "VAR" "PROCEDURE"
    "BEGIN" "CONST" "TYPE" "MODULE"})

(defgrammar oberon-0
  "Parse Oberon-0 as defined in Niklaus Wirth's Compiler Construction.
Return an ATS on success."
  [:start-rule start
   :match-case? true
   :print-err? true
   :profile? false
   :trace? false
   :memoize? false]
  (start
   (<g 0 (module) eoi))  
  ;; OK
  ;; "foo"
  ;;   => ($sym "foo")
  (ident
   (>lex #"[a-zA-Z][a-zA-Z0-9]*"
         #(when-not (*keywords* %)
            (list '$sym %))))
  ;; OK
  ;; "123"
  ;;   => 123
  (integer
   (>lex #"[0-9]+" Integer/parseInt)))
  ;; OK
  ;; ".foo"
  ;;   => ($rref ($sym "foo"))        ; record reference
  ;; "[i+3]"
  ;;   => ($aref ($+ ($sym "i") 3))   ; array reference
  (selector
   (<g| (>g 1 "." (ident)
            #(list '$rref %))
        (>g 1 "[" (expression) "]"
            #(list '$aref %))))
  ;; OK
  ;; "X.foo.bar.baaz[3]"
  ;;   => ($aref ($rref ($rref ($rref ($sym "X") ($sym "foo"))
  ;;                           ($sym "bar"))
  ;;                    ($sym "baaz"))
  ;;             3)
  ;;   
  ;;                 $aref
  ;;                  /  \
  ;;             $rref    3
  ;;              / \
  ;;         $rref   baaz
  ;;          / \
  ;;     $rref   bar
  ;;      / \
  ;;     X   foo
  ;;     
  (symref
   (>g (ident) (<g* 0 (selector))
       #(reduce (fn [x y]
                  (list* (first y) x (rest y)))
                %1 %2)))
  ;; OK
  (number
   (integer))
  ;; OK
  (factor
   (<g| (symref)
        (number)
        (<g 1 "(" (expression) ")")
        (>g 1 "~" (factor)
            #(list 'not %))))
  ;; OK
  (term
   (>g (factor) (<g* (g| "*" (<kw :DIV) (<kw :MOD) "&")
                     (factor))
       #(reduce (fn [x y]
                  (list* (symbol (str "$" (.toLowerCase
                                           (str (first y)))))
                         x (rest y)))
                %1
                %2)))
  ;; OK
  (simple-expression
   (>g (g? (<lex #"[+-]"))
       (>g (term) (<g* (g| "+" "-" (<kw :OR)) (term))
           #(reduce (fn [x y]
                      (list* (symbol (str "$" (.toLowerCase
                                               (str (first y)))))
                             x (rest y)))
                    %1
                    %2))
       #(if (= %1 :g?-failed)
          %2
          (list (symbol (str "$" %1)) %2))))
  ;; OK
  ;; NOTE: a function call can not be an expression if it's given args
  (expression
   (>g (simple-expression)
       (<g? (<g| "=" "#" "<=" "<" ">=" ">")
            (simple-expression))
       #(if (= %2 :g?-failed)
          %1
          (list* (symbol (str "$" (first %2))) %1 (rest %2)))))
  ;; OK
  (assignment
   (>g (symref) ":=" (expression)
       #(list '$:= %1 %3)))
  ;; OK
  (actual-parameters
   (>g 1 "(" (<g? (<g_ 0 (expression) ",")) ")"
       #(if (= % :g?-failed)
          []
          (vec %))))
  ;; OK
  (procedure-call
   (>g (symref) (g? (actual-parameters))
       #(if (= %2 :g?-failed)
          (list '$call %1)
          (list* '$call %1 %2))))
  ;;
  (if-cond
   (>g (<kw :IF) (expression) (<kw :THEN) (statement-sequence)
       #(let [[_ expr _ sseq] %&]
          (if (next sseq)
            (list '$cond expr (conj (seq sseq) '$do))
            (list '$cond expr (first sseq))))))
  ;;
  (elsif-cond
   (>g (<kw :ELSIF) (expression) (<kw :THEN) (statement-sequence)
       #(let [[_ expr _ sseq] %&]
          (if (next sseq)
            (list expr (conj (seq sseq) '$do))
            (list expr (first sseq))))))
  ;;
  (else-cond
   (>g 1 (<kw :ELSE) (statement-sequence)
       #(if (next %&)
          (conj %& '$do)
          (first %&))))
  ;; OK
  ;; ($cond
  ;;   ($= x 1) (:= x ($+ x 1))         ; IF
  ;;   ($= x 2) (:= x ($+ x 2))         ; ELSIF
  ;;   ($= x 3) (:= x ($+ x 3))         ; ELSIF
  ;;   ($= x 4) (:= x ($+ x 4))         ; ELSIF
  ;;   :else (:= x ($+ x 5))            : ELSE
  (if-statement
   (>g (if-cond)
       (<g* 0 (elsif-cond))
       (<g? 0 (else-cond))
       (<kw :END)
       #(let [[ifc elsifc elsec _] %&]
          (if (= elsec :g?-failed)
            (concat ifc (mapcat (fn [x] x) elsifc))
            (concat ifc (mapcat (fn [x] x) elsifc) [:else elsec])))))
  ;; OK
  (while-statement
   (>g (<kw :WHILE) (expression)
       (<kw :DO) (statement-sequence)
       (<kw :END)
       #(let [[_ expr _ sseq _] %&]
          (list* '$while expr sseq))))
  ;; OK
  (statement
   (<g| (assignment)
        (procedure-call)
        (if-statement)
        (while-statement)))
  ;; OK
  ;; "", ";" ";;;;;;;;"
  ;;   => [($noop)]
  ;; else
  ;;   => [s1 s2 s3 ...]
  (statement-sequence
   (>g_ 0 (<g? 0 (statement)) ";"
        #(let [xs (filter (partial not= :g?-failed) %&)]
           (if (empty? xs)
             ['($noop)]
             (vec xs)))))
  ;; OK
  ;; "x"
  ;;   => [($sym "x")]
  ;; "x, y"
  ;;   => [($sym "x") ($sym "y")]
  (ident-list
   (<g_ 0 (ident) ","))
  ;; OK
  ;; 1D array
  ;; "ARRAY m*n OF INTEGER"
  ;;   => ($array-decl ($type ($sym "INTEGER"))
  ;;                   ($* ($sym "x")
  ;;                       ($sym "n")))
  ;; 3D array
  ;; "ARRAY 3 OF
  ;;    ARRAY 3 OF
  ;;      ARRAY 3 OF FLOAT"
  ;;   => ($array-decl
  ;;         ($type ($array-decl ($type ($array-decl ($type ($sym "REAL"))
  ;;                                                 3))
  ;;                             3))
  ;;         3)
  (array-type
   (>g (<kw :ARRAY) (expression) (<kw :OF) (ob-type)
       #(let [[_ expr _ otype] %&]
          (list '$array-decl otype expr))))
  ;; OK
  ;; "x: FLOAT"
  ;; [($type ($sym "FLOAT")) ($sym "x")]
  ;; "x, y: FLOAT"
  ;; [($type ($sym "FLOAT")) ($sym "x") ($sym "y")]
  (field-list
   (>g (ident-list) ":" (ob-type)
       #(vec (list* %3 %1))))
  ;; OK
  ;; ($record)
  ;; ($record [TYPE x y z] [TYPE x y z])
  (record-type
   (>g (<kw :RECORD) (<g_ 0 (<g? 0 (field-list)) ";") (<kw :END)
       #(let [[_ fields _] %&]
          (list* '$record (filter (partial not= :g?-failed) fields)))))
  ;; OK
  (ob-type
   (>g| (ident)
        (array-type)
        (record-type)
        #(list '$type %1)))
  ;; OK
  ;; [TYPE x y z]
  (fp-section
   (>g (<g? (<kw :VAR)) (ident-list) ":" (ob-type)
       #(let [[_ ids _ otype] %&]
          (vec (list* otype ids)))))
  ;; OK
  ;; []
  ;; ($pargs (TYPE x y z)
  (formal-parameters
   (>g 1 "(" (<g? (<g_ 0 (fp-section) ";")) ")"
       #(if (= % :g?-failed)
          []
          (list* '$pargs %))))
  ;; OK
  ;; ($proc name)
  ;; ($proc name ($pargs (TYPE x y z)))
  (procedure-heading
   (>g (<kw :PROCEDURE) (ident) (g? (formal-parameters))
       #(let [[_ id parms] %&]
          (if (some #{parms} [:g?-failed, []])
            (list '$proc-head id)
            (list '$proc-head id parms)))))
  ;;
  (procedure-body
   (>g (declarations) (<g? 1 (<kw :BEGIN) (statement-sequence)) (<kw :END)
       (ident)
       #(let [[decls sseq _ _] %&]
          (if (= sseq :g?-failed)
            (list '$proc-body decls)
            (list* '$proc-body decls sseq)))))
  ;;
  (procedure-declaration
   (>g (procedure-heading) ";" (procedure-body)
       #(list '$proc-decl %1 %3)))
  ;; OK
  ;; "CONST"
  ;;   => ($noop)
  ;; "CONST x=42; y=42+99;"
  ;;   => ($const-decl ($symbol "x") 42
  ;;                   ($symbol "y") ($+ 42 99))
  (const-decl
   (>g (<kw :CONST) (<g* (ident) "=" (expression) ";")
       #(let [[_ decls] %&]
          (if (empty? decls)
            '($noop)
            (conj (mapcat (fn [x]
                            (let [[id _ expr _] x]
                              (list id expr)))
                          decls)
                  '$const-decl)))))
  ;; OK
  ;; "TYPE"
  ;;   => ($noop)
  ;; "TYPE x=INTEGER; y=FLOAT;"
  ;;   => ($type-decl ($type ($symbol "INTEGER")) ($symbol "x")
  ;;                  ($type ($symbol "FLOAT")) ($symbol "y"))
  (type-decl
   (>g (<kw :TYPE) (<g* (ident) "=" (ob-type) ";")
       #(let [[_ decls] %&]
          (if (empty? decls)
            '($noop)
            (conj (mapcat (fn [x]
                            (let [[id _ otype _] x]
                              (list otype id)))
                          decls)
                  '$type-decl)))))
  ;; OK
  ;; "VAR"
  ;;   => ($noop)
  ;; "VAR x, y: INTEGER; z: FLOAT"
  ;;   => ($var-decl ($type ($symbol "INTEGER")) [($symbol "x") ($symbol "y")]
  ;;                 ($type ($symbol "FLOAT")) [($symbol "z")])
  (var-decl
   (>g (<kw :VAR) (<g* (ident-list) ":" (ob-type) ";")
       #(let [[_ decls] %&]
          (if (empty? decls)
            '($noop)
            (conj (mapcat (fn [x]
                            (let [[ids _ otype _] x]
                              (list otype ids)))
                          decls)
                  '$var-decl)))))
  ;; 
  (declarations
   (>g (<g? 0 (const-decl))
       (<g? 0 (type-decl))
       (<g? 0 (var-decl))
       (<g* 0 (procedure-declaration) ";")
       #(let [[cd td vd pd] %&
              res1 (filter (fn [x] (and (not= x :g?-failed)
                                        (not= x [])))
                           [cd td vd])]
          (if (and (empty? res1)g
                   (empty? pd))
            '($noop)
            (concat ['$decls] res1 pd)))))
  ;; 
  (module
   (>g (<kw :MODULE) (ident) ";"
       (declarations)
       (<g? 1 (<kw :BEGIN) (statement-sequence))
       (<kw :END) (ident) "."
       #(let [[_ id _ decls sseq _ _ _] %&]
          (if (= sseq :g?-failed)
            (list '$module id decls)
            (list* '$module id decls sseq))))))

(defn pp-oberon-0
  "pretty print the AST on a successful parse"
  [text]
  (when-let [res (oberon-0 text)]
    (clojure.pprint/pprint res)))
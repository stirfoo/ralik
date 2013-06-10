;;; parser.clj
;;;
;;; Saturday, June  8 2013

(ns ^{:doc "Define a SQLite parser.
http://www.sqlite.org/hlr40000.html"}
  parsers.sqlite.parser
  (:use ralik.core)
  (:import [ralik RalikException ParserException]))

;; TODO: far, far from complete

(defn bad-char
  [c]
  (parse-error (- *cur-pos* 1)
               (str "invalid character `" c "'")
               :tag "sqlite"))

;; O_o
(def reserved-word?
  #{"abort" "add" "after" "all" "alter" "analyze" "and" "as" "asc" "attach"
    "autoincrement" "before" "begin" "between" "by" "cascade" "case" "cast"
    "check" "collate" "column" "commit" "conflict" "constraint" "create"
    "cross" "current_date" "current_time" "current_timestamp" "database"
    "default" "deferred" "deferrable" "delete" "desc" "detach" "distinct"
    "drop" "end" "each" "else" "escape" "except" "exclusive" "exists"
    "explain" "fail" "for" "foreign" "from" "full" "glob" "group" "having"
    "if" "ignore" "immediate" "in" "index" "initially" "inner" "insert"
    "instead" "intersect" "into" "is" "isnull" "join" "key" "left" "like"
    "limit" "match" "natural" "not" "notnull" "null" "of" "offset" "on" "or"
    "order" "outer" "plan" "pragma" "primary" "query" "raise" "references"
    "regexp" "reindex" "rename" "replace" "restrict" "right" "rollback"
    "row" "select" "set" "table" "temp" "temporary" "then" "to" "transaction"
    "trigger" "union" "unique" "update" "using" "vacuum" "values" "view"
    "virtual" "when" "where"})

(defatomic kw-teminator
  ""
  (match #"[a-zA-Z_]"))

(defgrammar sqlite-skipper
  "Skip
* all whitespace
* -- one line comments
* /*
     non-nesting
     multi line comments
  */
Parse error if \u0000 is read. (will Java even read 0x0?)"
  [:start-rule Skip
   :skipper nil
   :inherit? true
   :print-err? true]
  (Skip (g* (g| #"[ \n\t\r\v\f]+"
                (g "--"
                   (g* (g- <_
                           (g| eol
                               eoi
                               (>g \u0000 bad-char))))
                   (g| eol
                       eoi))
                (g "/*"
                   (g* (g- <_
                           (g| "*/"
                               (>g \u0000 bad-char))))
                   "*/")))))

(defgrammar sqlite
  "Parse SQLite"
  [:start-rule Start
   :skipper sqlite-skipper
   :trace? false
   :profile? false
   :print-err? true]
  ;; (Start (<g 0 (Expr) eoi))
  (Start (g (g? (StmtList)) (g? ";") eoi))
  ;; TODO: H41920
  ;; H42000
  (StmtList (g_ (Stmt) ";"))
  ;; H42100
  (Stmt (g (g? :explain (g? (kw :query) (kw :plan)))
           (g| (AlterTableStmt)
               (AnalyzeStmt)
               (AttachStmt)
               (BeginStmt)
               (CommitStmt)
               (CreateIndexStmt)
               (CreateTableStmt)
               (CreateTriggerStmt)
               (CreateViewStmt)
               (CreateVirtualTableStmt)
               (DeleteStmt)
               (DeleteStmtLimited)
               (DetachStmt)
               (DropIndexStmt)
               (DropTableStmt)
               (DropTriggerStmt)
               (DropViewStmt)
               (InsertStmt)
               (PragmaStmt)
               (ReindexStmt)
               (ReleaseStmt)
               (RollbackStmt)
               (SavePointStmt)
               (SelectStmt)
               (UpdateStmt)
               (UpdateStmtLimited)
               (VacuumStmt))))
  ;; H42200
  (AlterTableStmt (g (kw :alter) (kw :table) (g? (DatabaseName "."))
                     (TableName) (g| (g (kw :rename) (kw :to) (NewTableName))
                                     (g (kw :add) (g? (kw :column))
                                        (ColumnDef)))))
  ;; H42300
  (AnalyzeStmt (g (kw :analyze) (g| (DatabaseName) (g? "." (TableOrIndexName))
                                    (TableOrIndexName))))
  ;; H42400
  (AttachStmt (g (kw :attach) (g? (kw :database)) (Expr) (kw :as)
                 (DatabaseName)))
  ;; H42500
  (BeginStmt (g (kw :begin) (g? (kws :deferred :immediate :exclusive))
                (g? (kw :transaction))))
  ;; H42600
  (CommitStmt (g (kws :commit :end) (g? :transaction)))
  ;; H42700
  (RollbackStmt (g (kw :rollback) (g? (kw :transaction))
                   (g? (kw :to) (g? (kw :savepoint)) (SavePointName))))
  ;; H42800
  (SavePointStmt (g (kw :savepoint) (SavePointName)))
  ;; H42900
  (ReleaseStmt (g (kw :release) (g? (kw :savepoint)) (SavePointName)))
  ;; H43000
  (CreateIndexStmt (g (kw :create) (g? (kw :unique)) (kw :index)
                      (g? (kw "if") (kw :not) (kw :exists))
                      (g? (DatabaseName) ".") (IndexName) (kw :on)
                      (TableName) "(" (g_ (IndexedColumn) ",") ")"))
  ;; H43100
  (IndexedColumn (g (ColumnName) (g? (kw :collate) (CollationName))
                    (g? (kws :asc :desc))))
  ;; H43200
  (CreateTableStmt (g (kw :create) (g? (kws :temp :temporary)) (kw :table)
                      (g? (kw :if) (kw :not) (kw :exists))
                      (g? (DatabaseName) ".") (TableName)
                      (g| (g "("
                             (g_ (ColumnDef) ",") (g? (g_ (TableConsraint)
                                                          ","))
                             ")")
                          (g (kw :as) (SelectStmt)))))
  ;; H43300
  (ColumnDef (g (ColumnName) (g? (TypeName)) (g* (ColumnConstraint))))
  ;; H43400
  (TypeName (g (g+ (Name)) (g? (g "("
                                  (SignedNumber) (g? "," (SignedNumber))
                                  ")"))))
  ;; H43500
  (ColumnConstraint (g (g? (kw :constraint) (Name))
                       (g| (g (kw :primary) (kw :key) (g? (kws :asc :desc))
                              (ConflictClause) (g? (kw :autoincrement)))
                           (g (kw :not) (kw :null) (ConflictClause))
                           (g (kw :unique) (ConflictClause))
                           (g (kw :check) "(" (Expr) ")")
                           (g (kw :default) (g| (SignedNumber)
                                                (LiteralValue)
                                                (g "(" (Expr) ")")))
                           (g (kw :collate) (CollationName))
                           (ForeignKeyClause))))
  ;; H43600
  (SignedNumber (g #"[+-]?" (NumericLiteral)))
  ;; H43700
  (TableConsraint (g (kw :constraint) (Name)
                     (g| (g (g| (g (kw :primary) (kw :key))
                                (kw :unique))
                            "(" (g_ (IndexedColumn) ",") ")"
                            (ConflictClause))
                         (g (kw :check) "(" (Expr) ")")
                         (g (kw :foreign) (kw :key)
                            "(" (g_ (ColumnName) ",") ")"
                            (ForeignKeyClause)))))
  ;; H43800
  (ForeignKeyClause (g (kw :references) (ForeignTable)
                       (g? "(" (g_ (ColumnName) ",") ")")
                       (g* (g| (g (kw :on) (kws :delete :update)
                                  (g| (g (kw :set) (kws :null :default))
                                      (kws :cascade :restrict)
                                      (g (kw :no) (kw :action))))
                               (g (kw :match) (Name))))
                       (g? (g? (kw :not)) (kw :deferrable)
                           (g? (kw :initially)
                               (kws :deferred :immediate)))))
  ;; H43900
  (ConflictClause (g| (g (kw :on) (kw :conflict)
                         (kws :rollback :abort :fail :ignore :replace))
                      :empty))
  ;; H44000
  (CreateTriggerStmt (g (kw :create) (g? (kws :temp :temporary))
                        (kw :trigger) (g? (kw :if) (kw :not) (kw :exists))
                        (g? (DatabaseName) ".") (TriggerName)
                        (g| (kws :before :after)
                            (g (kw :instead) (kw :of)))
                        (g| (kw :delete)
                            (kw :insert)
                            (g (kw :update) (g? (kw :of)
                                                (g_ (ColumnName) ","))))
                        (kw :on) (TableName)
                        (g? (kw :for) (kw :each) (kw :row))
                        (g? (kw :when) (Expr))
                        (kw :begin) (g+ (g| (UpdateStmt)
                                            (InsertStmt)
                                            (DeleteStmt)
                                            (SelectStmt))
                                        ";")
                        (kw :end)))
  ;; H44100
  (CreateViewStmt (g (kw :create) (g? (kws :temp :temporary))
                     (kw :view) (g? (kw :if) (kw :not) (kw :exists))
                     (g? (DatabaseName) ".") (ViewName) (kw :as)
                     (SelectStmt)))
  ;; H44200
  (CreateVirtualTableStmt (g (kw :create) (kw :virtual) (kw :table)
                             (g? (kw :if) (kw :not) (kw :exists))
                             (g? (DatabaseName) ".") (TableName)
                             (kw :using) (ModuleName)
                             (g? "(" (g_ (ModuleArgument) ",") ")")))
  ;; H44300
  (DeleteStmt (g (kw :delete) (kw :from) (QualifiedTableName)
                 (g? (kw :where) (Expr))))
  ;; H44400
  (DeleteStmtLimited (g (kw :delete) (kw :from) (QualifiedTableName)
                        (g? (kw :where) (Expr))
                        (g? (g? (kw :order) (kw :by) (g_ (OrderingTerm) ","))
                            (kw :limit) (Expr) (g? (g| (kw :offset)
                                                       ",")
                                                   (Expr)))))
  ;; H44500
  (DetachStmt (g (kw :detach) (g? (kw :database)) (DatabaseName)))
  ;; H44600
  (DropIndexStmt (g (kw :drop) (kw :index) (g? (kw :if) (kw :exists))
                    (g? (DatabaseName) ".") (IndexName)))
  ;; H44700
  (DropTableStmt (g (kw :drop) (kw :table) (g? (kw :if) (kw :exists))
                    (g? (DatabaseName) ".") (TableName)))
  ;; H44800
  (DropTriggerStmt (g (kw :drop) (kw :trigger) (g? (kw :if) (kw :exists))
                      (g? (DatabaseName) ".") (TriggerName)))
  ;; H44900
  (DropViewStmt (g (kw :drop) (kw :view) (g? (kw :if) (kw :exists))
                   (g? (DatabaseName) ".") (ViewName)))
  ;; H45000
  (Expr (g (Expr1)
           (g* (g| (g (kw :collate) (CollationName))
                   (g (g? (kw :not)) (kws :like :glob :regexp :match)
                      (Expr) (g? (kw :escape) (Expr)))
                   ;; Binary RHS
                   ;; (g (kw :is) (g? (kw :not)) (Expr))
                   (g (g? (kw :not)) (kw :between) (Expr) (kw :and)
                      (Expr))
                   (g (g? (kw :not)) (kw :in)
                      (g| (g "(" (g| (SelectStmt)
                                     (g_ (Expr) ",")
                                     :empty)
                             ")")
                          (g (g? (DatabaseName) ".") (TableName))))
                   (g (BinaryOperator) (Expr))))))
  (Expr1 (g|
          ;; ID ( ...
          (FuncExpr)
          ;; [+-!]|NOT ...
          (UnaryExpr)
          (LiteralValue)
          (BindParameter)
          (ColumnNameExpr)
          ;; EXISTS     ...
          ;; NOT EXISTS ...
          ;; ( SELECT   ...
          (NotExistsSelectExpr)
          ;; ( ...
          (ParenExpr)
          ;; CAST ...
          (CastExpr)
          ;; CASE ...
          (CaseExpr)
          ;; RAISE ...
          (RaiseFunction)))
  ;; Expr1 helpers
  (FuncExpr (g (FunctionName)
               "("
               (g| (g (g? (kw :distinct)) (g_ (Expr) ","))
                   "*"
                   :empty)
               ")"))
  (UnaryExpr (g (UnaryOperator) (Expr)))
  (ColumnNameExpr (g (g? (g? (DatabaseName) ".")
                         (TableName) ".")
                     (ColumnName)))
  (NotExistsSelectExpr (g (g? (g? (kw :not)) (kw :exists))
                          "(" (SelectStmt) ")"))
  (ParenExpr (g "(" (Expr) ")"))
  (CastExpr (g (kw :cast) "(" (Expr) (kw :as) (TypeName) ")"))
  (CaseExpr (g (kw :case) (g? (Expr))
               (g+ (kw :when) (Expr) (kw :then) (Expr))
               (g? (kw :else) (Expr)) (kw :end)))
  ;; H45100
  (RaiseFunction (g (kw :raise)
                    "("
                    (g| (kw :ignore)
                        (g (kws :rollback :abort :fail)
                           "," (ErrorMessage)))
                    ")"))
  ;; H45200
  (LiteralValue (<g| (NumericLiteral)
                     (StringLiteral)
                     (BlobLiteral)
                     (<kws :null :current_time :current_date
                           :current_timestamp)))
  ;; H45300
  (InsertStmt (g (g| (g (kw :insert) (g? (kw :or) (kws :rollback :abort
                                                       :replace :fail
                                                       :ignore))
                        (kw :into) (g? (DatabaseName) ".") (TableName))
                     (kw :replace))
                 (g| (g (g? "(" (g_ (ColumnName) ",") ")")
                        (g| (g (kw :values) (g_ (g "(" (g_ (Expr)
                                                           ",")
                                                   ")")
                                                ","))
                            (SelectStmt)))
                     (g (kw :default) (kw :values)))))
  ;; H45400
  (PragmaStmt (g (kw :pragma) (g? (DatabaseName) ".") (PragmaName)
                 (g| (g "=" (PragmaValue))
                     (g "(" (PragmaValue) ")")
                     :empty)))
  ;; H45500
  (PragmaValue (g| (SignedNumber)
                   (Name)
                   (StringLiteral)))
  ;; H45600
  (ReindexStmt (g (kw :reindex) (g| (CollationName)
                                    (g (g? (DatabaseName) ".")
                                       (g| (TableName)
                                           (IndexName)))
                                    :empty)))
  ;; H45700
  (SelectStmt (g (g_ (SelectCore) (CompoundOperator))
                 (g? (kw :order) (kw :by) (g_ (OrderingTerm) ","))
                 (g? (kw :limit) (Expr) (g? (g| (kw :offset)
                                                ","))
                     (Expr))))
  ;; H45800
  (SelectCore (g (kw :select) (g? (kws :distinct :all)) (g_ (ResultColumn)
                                                            ",")
                 (g? (kw :from) (JoinSource))
                 (g? (kw :where) (Expr))
                 (g? (kw :group) (kw :by) (g_ (Expr) ",")
                     (g? (kw :having) (Expr)))))
  ;; H45900
  (ResultColumn (g| "*"
                    (g (TableName) "." "*")
                    (g (Expr) (g? (g? (kw :as)) (ColumnAlias)))))
  ;; H46000
  (JoinSource (g (SingleSource)
                 (g? (g+ (JoinOp) (SingleSource) (JoinConstraint)))))
  ;; H46100
  (SingleSource (g| (g (g? (DatabaseName) ".") (TableName)
                       (g? (g? (kw :as)) (TableAlias))
                       (g| (g (kw :indexed) (kw :by) (IndexName))
                           (g (kw :not) (kw :indexed))
                           :empty))
                    ;; XXX
                    (g "(" (SelectStmt) ")" (g? (g? (kw :as)) (TableAlias)))
                    (g "(" (JoinSource) ")")))
  ;; H46200
  (JoinOp (g| ","
              (g (g? (kw :natural))
                 (g| (g (kw :left) (g? (kw :outer)))
                     (kws :inner :cross)
                     :empty)
                 (kw :join))))
  ;; H46300
  (JoinConstraint (g| (g (kw :on) (Expr))
                      (g (kw :using) "(" (g_ (ColumnName) ",") ")")
                      :empty))
  ;; H46400
  (OrderingTerm (g (Expr) (g? (kw :collate) (CollationName))
                   (g? (kws :asc :desc))))
  ;; H46500
  (CompoundOperator (g| (g (kw :union) (g? (kw :all)))
                        (kws :intersect :except)))
  ;; H46600
  (UpdateStmt (g (kw :update) (g? (kw :or) (kws :rollback :abort :replace
                                                :fail :ignore))
                 (QualifiedTableName) (kw :set) (g_ (g (ColumnName) "="
                                                       (Expr))
                                                    ",")
                 (g? (kw :where) (Expr))))
  ;; H46700
  (UpdateStmtLimited (g (UpdateStmt)
                        (g? (g? (kw :order) (kw :by) (g_ (OrderingTerm) ","))
                            (kw :limit) (Expr) (g? (g| (kw :offset)
                                                       ",")
                                                   (Expr)))))
  ;; H46800
  (QualifiedTableName (g (g? (DatabaseName) ".") (TableName)
                         (g| (g (kw :indexed) (kw :by) (IndexName))
                             (g (kw :not) (kw :indexed))
                             :empty)))
  ;; H46900
  (VacuumStmt (kw :vaccum))
  ;; These are missing from the railroad diagrams
  (DatabaseName (Ident))
  (TableName (Ident))
  (NewTableName (Ident))
  (TableOrIndexName (Ident))
  (SavePointName (Ident))
  (IndexName (Ident))
  (ColumnName (Ident))
  (CollationName (Ident))
  (Name (Ident))
  (ForeignTable
   )
  (TriggerName (Ident))
  (ViewName (Ident))
  (ModuleName (Ident))
  (ModuleArgument
   )
  (UnaryOperator (g| "+" "-" "~" (kw :not)))
  ;; all operators on the same line have the same preprecedence
  ;; highest to lowest
  (BinaryOperator
   (g| "||"
       "*" "/" "%"
       "+" "-"
       "<<" ">>" "&" "|"
       "<=" (g "<" (g! ">")) ">=" ">"
       "==" "=" "!=" "<>" (g (kw :is) (kw :not)) (kws :in :like :glob
                                                      :match :regexp)
       (kw :and)
       (kw :or)))
  (BindParameter (g| (g "?" (skip- #"\d*"))
                     (g #"[@:]" (skip- (Ident)))
                     (g "$" (skip- (Ident :allow-double-colon))
                        (g? "(" (skip- (g* (g- <_ ")"))) ")"))))
  (FunctionName (Ident))
  (ErrorMessage (StringLiteral))
  (PragmaName (Ident))
  (ColumnAlias (Ident))
  (TableAlias (Ident))
  (NumericLiteral (>g #"(((\d+(\.\d*)?)|(\.\d+)))([eE][+-]?\d+)?"
                      #(if (some #{\. \e \E} %)
                         (Double/parseDouble %)
                         (Integer/parseInt %))))
  ;; 'a literal '' str"ing' => "a literal '' str\"ing"
  (StringLiteral (<g 1 "'"
                     (<skip- (<lex (g* (g| (g "'" "'")
                                           (>g \u0000 bad-char)
                                           (g- <_ "'")))))
                     "'"))
  ;; x'a3f8cc3b' => [164 255 62 243]
  (BlobLiteral (<g 1 "x"
                   (<skip- 1 \'
                           (>g+ (<rep 2 #"[a-fA-F0-9]")
                                #(vec (for [x %&]
                                        (Integer/parseInt (apply str x) 16))))
                           \')))
  ;; foob$ar
  ;; [f oo bar]
  ;; "foo "" bar"
  ;; `foo `` bar`
  (Ident
   [& allow-double-colon]
   (>g| (if (seq allow-double-colon)
          (g #"([a-zA-Z]|::)([a-zA-Z0-9$]|::)*")
          (g #"[a-zA-Z][a-zA-Z0-9$]*"))
        (<lex (g| (g "["
                     (g* (g- <_
                             (g| "]"
                                 (>g \u0000 bad-char))))
                     "]")
                  (g \"
                     (g* (g| (g \" \")
                             (>g \u0000 bad-char)
                             (g- <_ \")))
                     \")
                  (g "`"
                     (g* (g| (g \" \")
                             (>g \u0000 bad-char)
                             (g- <_ \")))
                     "`")))
        #(when-not (reserved-word? (.toLowerCase %))
           %))))

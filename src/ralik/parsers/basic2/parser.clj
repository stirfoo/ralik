;;; parser.clj
;;;
;;; Sunday, April 15 2012

(ns ^{:doc "Adapted from:
http://www.bitsavers.org/pdf/dartmouth/BASIC_Oct64.pdf"}
  ralik.parsers.basic2.parser
  (:use ralik.core)
  (:import [ralik RalikException])
  (:import [ralik.parsers.basic2 BASICException]))

(def sym->node {\^ 'bpow, \* '*, \/ '/, \+ '+, \- '-, \< '<,
                "<=" '<=, \= '==, ">=" '>=, \> '>, "<>" 'bne})

(declare make-skipper)

(defgrammar basic2
  "Parse Dartmouth College BASIC Version 2.
Return a vector containing [line-number node] for each line in the program."
  [:trace? false
   :match-case? false
   :print-err? true
   :skipper (make-skipper)]
  ;; 
  (start
   (<g 0
       (statement-list)
       eoi))
  ;; 
  (statement-list
   (>g* (<g| (blank-line)
             (statement))
        #(into [] (mapcat (fn [x] (if (= x [:no-op])
                                    nil
                                    x))
                          %&))))
  ;;
  (statement
   (<g [0 2]
       (line-num)
       (<g| (rem-stmt) (let-stmt) (read-stmt) (data-stmt) (next-stmt)
            (print-stmt) (goto-stmt) (if-stmt) (for-stmt) (dim-stmt)
            (def-stmt) (gosub-stmt) (return-stmt) (stop-stmt) (end-stmt)
            (>g #"[a-zA-Z]+" (fn [x]
                               (set! *err-pos* (- *cur-pos* (count x)))
                               (set! *err-msg* "ILLEGAL INSTRUCTION")
                               nil)))
       (g| eol eoi)))
  ;;
  (blank-line
   (<g 1 eol :no-op))
  ;;
  (line-num
   (>g #"[1-9][0-9]*"
       #(let [nchars (count %)]
          (if (> nchars 5)
            (do
              (set! *err-pos* (- *cur-pos* nchars))
              (set! *err-msg* "ILLEGAL LINE NUMBER")
              nil)
            (Integer/parseInt %)))))
  ;; 
  (rem-stmt
   (>g 2 "REM" ! (<lex (g* (g- _ eol)))
       #(list 'brem %)))
  ;;
  (let-stmt
   (>g "LET" ! (var-name) \= (expr)
       #(list 'blet %3 %5)))
  ;; 
  (read-stmt
   (>g 2 "READ" ! (read-var-list)
       #(cons 'bread %&)))
  ;;
  (data-stmt
   (>g 2 "DATA" ! (number-list)
       #(cons 'bdata %&)))
  ;;
  (print-stmt
   (>g [2 4] "PRINT" ! (print-args) (print-sep)
       #(concat (cons 'bprint (if (= %1 :g?-failed)
                                nil
                                %1))
                (if (= %2 :g?-failed)
                  nil
                  (list %2)))))
  ;;
  (print-args
   (<g? 0 (>g_ (<g| (expr)
                    (literal-string))
               (print-sep)
               #(cons (first %&)
                      (mapcat (fn [[sep arg]]
                                (if (= sep :g?-failed)
                                  (list arg)
                                  (list sep arg)))
                              (rest %&))))))
  ;; 
  (print-sep
   (<g? 0 (<g| \, \;)))
  ;;
  (goto-stmt
   (>g 3 "GO" "TO" ! (line-num)
       #(list 'bgoto %)))
  ;;
  (if-stmt
   (>g "IF" ! (expr) (<g| "<=" "<>" \< ">=" \> \=
                          (do
                            (set! *err-msg* "ILLEGAL RELATION")
                            nil))
       (expr)
       "THEN" (line-num)
       #(list 'bif (list (sym->node %4) %3 %5) %7)))
  ;; => (bfor [X (brange i j k?)] s1 s2 s3 next)
  (for-stmt
   (>g (<g 2 "FOR" ! (var-name))
       (<g 1 \= (expr))                 ; %2
       (<g 1 "TO" (expr))
       (<g? 1 "STEP" (expr))
       #(list* 'bfor %1 %2 %3 (if (= %4 :g?-failed)
                               nil
                               (list %4)))))
  (next-stmt
   (>g 2 "NEXT" ! (var-name) #(list 'bnext %)))
  ;;
  (dim-stmt
   (>g 2 "DIM" ! (dim-arg-list)
       #(list* 'bdim %&)))
  ;;
  (def-stmt
    (>g "DEF" !
        (>g #"[fF][nN][a-zA-Z]" #(symbol (.toLowerCase %)))
        \(
        (>g #"[a-zA-Z]" #(symbol (.toLowerCase %)))
        \)
        \= (expr)
        #(list 'bdef %3 [%5] %8)))
  ;;
  (gosub-stmt
   (>g 2 "GOSUB" ! (line-num)
       #(list 'bgosub %)))
  ;;
  (return-stmt
   (<g 1 "RETURN" '(breturn)))
  ;; 
  (end-stmt
   (<g 1 "END" '(bend)))
  ;; 
  (stop-stmt
   (<g 1 "STOP" '(bstop)))
  ;; 
  (expr
   (>g_ (mul-expr) (<g| \+ \-)
        #(reduce (fn [x [op y]]
                   (list (sym->node op) x y))
                 %&)))
  ;; 
  (mul-expr
   (>g_ (pow-expr) (<g| \* \/)
        #(reduce (fn [x [op y]]
                   (list (sym->node op) x y))
                 %&)))
  ;; 
  (pow-expr
   (>g_ 0 (primary) \^
        #(reduce (fn [x y]
                   (list (sym->node \^) x y))
                 %&)))
  ;;
  (primary
   (<g| (funcall)
        (array-ref 'baget false)
        (var-ref)
        (number)
        (par-expr)))
  ;;
  (funcall
   (>g (>g #"[a-zA-Z]{3,3}"
           #(symbol (.toLowerCase %)))
       (par-expr)
       #(if (= %1 'rnd)
          (list 'bcall %1)
          (list 'bcall %1 %2))))
  ;; 
  (array-ref
   [node-name                           ; badim, baset, baget
    integer-index?]
   (>g (>g #"[a-zA-Z]" #(.toLowerCase %)) (subscript integer-index?)
       (fn [s [i j]]
         (if (= j :g?-failed)
           (list node-name s i)
           (list node-name s i j)))))
  ;;
  (var-ref
   (>g (var-name)
       #(list 'bvref %)))
  ;;
  (subscript
   [integer-index?]
   (<g [1 3]
       \((if integer-index? (integer) (expr))
       (<g? 1 \, (if integer-index? (integer) (expr))) \)))
  ;;
  (par-expr
   (<g 1 \( (expr) \)))
  ;;
  (read-var-list
   (<g_ 0 (<g| (array-ref 'baset false) ; false -> index can be an expression
               (var-name))
        \,))
  ;;
  (number-list
   (<g_ 0 (number) \,))
  ;;
  (dim-arg-list
   (<g_ 0 (array-ref 'badim true) \,))
  ;;
  (literal-string
   (<lex 1 \" (g* (g- _ (g| \" eol))) \"))
  ;; 
  (number
   (>g (<g? \-) (<g| (floating-point)
                     (integer))
       #(if (= %1 :g?-failed)
          %2
          (- %2))))
  ;; 
  (floating-point
   (>g #"((\d+\.\d*|\d*\.\d+)([eE]-?\d+)?)|(\d+E-?\d+)"
       #(Double/parseDouble %)))
  ;; 
  (integer
   (>g #"\d+" #(Integer/parseInt %)))
  ;; p
  (var-name
   (>g #"[a-zA-Z][0-9]?" #(.toLowerCase %))))

(defn make-skipper
  "Eat spaces and tabs.
A REM is actually an ignored statement but GOTO and GOSUB can reference them
so they can't be skipped."
  []
  (fn []
    (while (and (< *cur-pos* *end-pos*)
                (some #{(nth *text-to-parse* *cur-pos*)} [\space \tab])
                (set! *cur-pos* (inc *cur-pos*))))
    true))
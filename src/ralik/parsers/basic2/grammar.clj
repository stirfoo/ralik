;;; grammar.clj
;;;
;;; Sunday, April 15 2012

(ns ^{:doc "Adapted from:
http://www.bitsavers.org/pdf/dartmouth/BASIC_Oct64.pdf"}
  ralik.parsers.basic2.grammar
  (:use ralik.core))

(def sym->node {\^ 'bpow \* 'bmul \/ 'bdiv \+ 'badd \- 'bsub \< 'blt
                "<=" 'ble \= 'beq \> 'bgt ">=" 'bgt "<>" 'bne})

(declare make-skipper)

(defgrammar basic2
  "Parse Dartmouth College BASIC Version 2.
Return a vector of statement nodes."
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
        #(into [] (mapcat (fn [x]
                            (when-not (and (vector? x)
                                           (= (first x) :noop))
                              x))
                          %&))))
  ;;
  (statement
   (>g [0 2]
       (line-num :as-meta)
       (<g| (rem-stmt) (let-stmt) (read-stmt) (data-stmt)
            (print-stmt) (goto-stmt) (if-stmt) (for-stmt) (dim-stmt)
            (def-stmt) (gosub-stmt) (return-stmt) (stop-stmt) (end-stmt))
       (g| eol eoi)
       #(with-meta %2 %1)))
  ;;
  (blank-line
   (<g 1 eol :noop))
  ;;
  (line-num
   [& as-meta?]
   (>g 0 #"[1-9][0-9]*"
       #(let [n (Integer/parseInt %)]
          (if as-meta?
            {:bline-number n}
            n))))
  ;; 
  (rem-stmt
   (>g 1 "REM" (<lex (g* (g- _ eol)))
       #(list 'brem %)))
  ;;
  (let-stmt
   (>g "LET" (var-name) \= (expr)
       #(list 'blet %2 %4)))
  ;; 
  (read-stmt
   (>g 1 "READ" (var-list)
       #(cons 'bread %&)))
  ;;
  (data-stmt
   (>g 1 "DATA" (number-list)
       #(cons 'bdata %&)))
  ;;
  (print-stmt
   (>g [1 3] "PRINT" (print-args) (print-sep)
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
   (>g 2 "GO" "TO" (line-num)
       #(list 'bgoto %)))
  ;;
  (if-stmt
   (>g "IF" (expr) (<g| "<=" "<>" \< ">=" \> \=) (expr)
       "THEN" (line-num)
       #(list 'bif
              (list (sym->node %3) %2 %4)
              (list 'bgoto %6))))
  ;;
  (for-stmt
   (>g (<g 1 "FOR" (var-name))
       (<g 1 \= (expr))
       (<g 1 "TO" (expr))
       (<g? 1 "STEP" (expr))
       eol
       (statement-list)
       (>g (line-num :as-meta) "NEXT" (var-name)
           #(with-meta (list 'bnext %3) %1))
       #(if (not (= %1 (fnext %7)))
          (throw (Exception.
                  (str "LINE " ((meta %7) :bline-number)
                       ": FOR expected NEXT " %1 ", got NEXT " (fnext %7))))
          (list* 'bfor [%1 (list* 'brange %2 %3
                                  (if (= %4 :g?-failed)
                                    ()
                                    (list %4)))]
                 (conj %6 %7)))))
  ;;
  (dim-stmt
   (>g 1 "DIM" (dim-arg-list)
       #(list* 'bdim %&)))
  ;;
  (def-stmt
    (>g "DEF" (>g 0 #"FN[A-Z]" symbol) \( (>g 0 #"[A-Z]" symbol) \) \= (expr)
        #(list 'bdef %2 [%4] %7)))
  ;;
  (gosub-stmt
   (>g 1 "GOSUB" (line-num)
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
        (array-ref false)
        (var-name)
        (number)
        (par-expr)))
  ;;
  (funcall
   (>g (>g 0 #"[a-zA-Z]{3,3}" symbol) (par-expr)
       #(list 'bcall %1 %2)))
  ;;
  (array-ref
   [int-subscr?]
   (>g (>g 0 #"[a-zA-Z]" symbol) (subscript int-subscr?)
       (fn [s [i j]]
         (if (= j :g?-failed)
           (list (if int-subscr? 'bdim1 'bref1) s i)
           (list (if int-subscr? 'bdim2 'bref2) s i j)))))
  ;;
  (subscript
   [int-subscr?]
   (<g [1 3] \( (if int-subscr? (integer) (expr))
       (<g? 1 \, (if int-subscr? (integer) (expr))) \)))
  ;;
  (par-expr
   (<g 1 \( (expr) \)))
  ;;
  (var-list
   (<g_ 0 (<g| (array-ref false)
               (var-name))
        \,))
  ;;
  (number-list
   (<g_ 0 (number) \,))
  ;;
  (dim-arg-list
   (<g_ 0 (array-ref true)
        \,))
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
  ;; 
  (var-name
   (>g #"[a-zA-Z][0-9]?" symbol)))

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

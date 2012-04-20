;;; runner.clj
;;;
;;; Monday, April 16 2012

(ns ralik.parsers.basic2.runner
  (:require [clojure.walk :as walk])
  (:import [ralik RalikException])  
  (:import [ralik.parsers.basic2 ReturnException BASICException EndException
            NextException])
  (:use [clojure.pprint :only [pprint]])
  (:use [ralik.parsers.basic2.parser :only [basic2]]))

(comment TODO
  PRINT, FOR, NEXT)

(def *skip-for?* false)

(defn- contextify
  "Replace node root symbols with the gensyms found in compile2.
bsin, btan, bpow, etc."
  [node ctx]
  (walk/postwalk-replace ctx node))

(defn- line-num->fn
  [n]
  (symbol (str "fn-" n)))

;; (blet x y)
(defn- translate-let
  [[n node] k ctx]
  (let [node (contextify node ctx)]
    `(~(line-num->fn n) []
      (when-not *skip-for?*
        (swap! ~(ctx :vars) assoc ~(fnext node) ~(last node)))
      (~k))))

;; (bread a b c ...)
(defn- translate-read
  [[n node] k ctx]
  (let [node (contextify node ctx)]
    `(~(line-num->fn n) []
      (when-not *skip-for?*
        (when (= (deref ~(ctx :data)) :no-data)
          (throw (BASICException.
                  (str "No DATA supplied for READ statement"))))
        ~@(map #(if (string? %)
                  `(if-let [data# (first (deref ~(ctx :data)))]
                     ;; read data into var
                     (do (swap! ~(ctx :vars) assoc ~% data#)
                         ;; pop data
                         (swap! ~(ctx :data) rest))
                     (~(ctx :done) "READ out of data. Program terminated."))
                  %)
               (rest node)))
      (~k))))

(defn- spaces [n] (apply str (repeat n " ")))

;; (bprint a \, b \, c ...)
(defn- translate-print
  [[n node] k ctx]
  (let [node (contextify node ctx)]
    `(~(line-num->fn n) []
      (when-not *skip-for?*
        (loop [[output# & tail#] ~(into [] (rest node))
               nl?# true
               pos# (deref ~(ctx :pos))]
          (cond
           (= output# nil) (if nl?#
                             (do
                               (newline)
                               (reset! ~(ctx :pos) 0))
                             (reset! ~(ctx :pos) pos#))
           ;; next multiple of 16 or newline if overflow
           (= output# \,) (let [next-zone# (* 16 (Math/ceil (/ pos# 16)))]
                            (if (>= next-zone# 80)
                              (do (newline)
                                  (recur tail# false 0))
                              (do (print (spaces (- next-zone# pos#)))
                                  (recur tail# false next-zone#))))
           ;; next multiple of 3 or 6 spaces minumum or newline if overflow
           (= output# \;) (let [col# (* 3 (Math/ceil (/ pos# 3)))
                                next-zone# (if (< (- col# pos#) 6)
                                             (+ col# 3)
                                             col#)]
                            (if (>= next-zone# 80)
                              (do (newline)
                                  (recur tail# false 0))
                              (do (print (spaces (- next-zone# pos#)))
                                  (recur tail# false next-zone#))))
           :else (let [so# (str output#)
                       len# (count so#)
                       end# (+ pos# len#)]
                   (if (>= end# 80)
                     (do
                       (println (subs so# 0 (- 80 pos#)))
                       (recur tail# true 0))
                     (do
                       (print so#)
                       (recur tail# true end#)))))))
      (~k))))

(defn- basic-for-seq
  "Return a sequence from start to stop inclusive by step.
If step is not supplied, it is assumed to be 1 if start <= stop, or -1 if
start > stop."
  ([start stop]
     (if (<= start stop)
       (basic-for-seq start stop 1)
       (basic-for-seq start stop -1)))
  ([start stop step]
     (let [b (rationalize start)
           e (rationalize stop)
           s (rationalize step)
           conv-fn (if (every? integer? [b e s])
                     int
                     float)]
       (map conv-fn (let [res (into [] (range b e s))]
                      (if (= (peek res) stop)
                        res
                        (conj res stop)))))))

;; (bfor "x" start stop step)
(defn- translate-for
  [[n node] k ctx]
  (let [[_ variable start stop step] (contextify node ctx)
        step-val 0]
    `(~(line-num->fn n) []
      (when-not *skip-for?*
        (binding [*skip-for?* false]
          (doseq [i# (apply basic-for-seq (if ~step
                                            (list ~start ~stop ~step)
                                            (list ~start ~stop)))]
            ;; set the loop variable
            (swap! ~(ctx :vars) assoc ~variable i#)
            (try
              (~k)
              (catch NextException e#
                (let [msg# (.getMessage e#)]
                  (when-not (=  msg# ~variable)
                    (throw (BASICException. (str "FOR expected NEXT "
                                                 ~variable " got NEXT "
                                                 msg#)))))))))
        ;; FOR is done, NEXT ~variable should set *skip-for?* to false
        (binding [*skip-for?* ~variable]
          ;; 
          (~k)))
      (~k))))

;; (bnext "i")
(defn- translate-next
  [[n node] k ctx]
  (let [node (contextify node ctx)]
    `(~(line-num->fn n) []
      (let [i# *skip-for?*]
        (when-not *skip-for?*
          (throw (NextException. ~(last node))))
        (binding [*skip-for?* (if (= i# ~(last node))
                                false
                                i#)]
          (~k))))))

;; (bdim (badim "y" 3) (badim "z" 3 4))
(defn- translate-dim
  [[n node] k ctx]
  (let [node (contextify node ctx)]
    `(~(line-num->fn n) []
      (when-not *skip-for?*
        ~@(rest node))
      (~k))))

;; (bgoto n)
(defn- translate-goto
  [[n node] k ctx]
  `(~(line-num->fn n) []
    (when-not *skip-for?*
      (~(line-num->fn (fnext node))))
    (~k)))

;; (bif relational-expr n)
(defn- translate-if
  [[n node] k ctx]
  (let [node (contextify node ctx)]
    `(~(line-num->fn n) []
      (when-not *skip-for?*
        (if ~(second node)
          ;; goto n
          (~(line-num->fn (last node)))
          ;; continue
          (~k)))
      (~k))))

;; (bgosub n)
(defn- translate-gosub
  [[n node] k ctx]
  `(~(line-num->fn n) []
    (when-not *skip-for?*
      (try
        (~(line-num->fn (last node)))
        (catch ReturnException e#
          (~k))))
    (~k)))

;; (breturn)
(defn- translate-return
  [[n node] k ctx]
  `(~(line-num->fn n) []
    (if-not *skip-for?*
      (throw (ReturnException.))
      (~k))))

;; (bstop)
(defn- translate-stop
  [[n node] k ctx]
  `(~(line-num->fn n) []
    (when-not *skip-for?*
      (~(ctx :done) ""))
    (~k)))

;; (bend)
(defn- translate-end
  [[n node] k ctx]
  `(~(line-num->fn n) []
    (~(ctx :done) "")))

(defn- gen-noop-fn
  "Return a letfn function spec that simply calls its continuation.
Some statements like REM do nothing but they can be the target of a GOTO."
  [[n node] k ctx]
  `(~(line-num->fn n) []
    (~k)))

(defn- translate-stmt
  [stmt k ctx]
  (case (first (fnext stmt))
    'brem (gen-noop-fn stmt k ctx)
    'bdef (gen-noop-fn stmt k ctx)
    'bdata (gen-noop-fn stmt k ctx)
    'blet (translate-let stmt k ctx)
    'bgoto (translate-goto stmt k ctx)
    'bif (translate-if stmt k ctx)
    'bgosub (translate-gosub stmt k ctx)
    'breturn (translate-return stmt k ctx)
    'bend (translate-end stmt k ctx)
    'bstop (translate-stop stmt k ctx)
    'bread (translate-read stmt k ctx)
    'bprint (translate-print stmt k ctx)
    'bfor (translate-for stmt k ctx)
    'bdim (translate-dim stmt k ctx)
    'bnext (translate-next stmt k ctx)))

(defn- collect-data
  "Return a sequence of the leaves of all bdata nodes or :no-data"
  [stmts]
  (let [data (mapcat (fn [[n node]]
                       (when (= (first node) 'bdata)
                         (rest node)))
                     stmts)]
    (if (empty? data)
      :no-data
      data)))

(defn- gen-def-fns
  "Return a letfn function spec as defined by a BASIC DEF statement"
  [node ctx]
  (let [[_ fn-name [arg] val] (contextify node ctx)]
    `(~(symbol (.toLowerCase (name fn-name)))
      [~arg]
      (swap! ~(ctx :vars) assoc (name '~arg) ~arg)
      ~val)))

(defn- chk-duplicate-line-numbers
  [statements]
  (loop [stmts statements
         line-set #{}]
    (if (first stmts)
      (let [[n _] (first stmts)]
        (if (line-set n)
          (throw (BASICException. (str "duplicate line number (" n ")")))
          (recur (next stmts) (conj line-set n)))))))

(defn- chk-end-statement
  "Exactly one END statement allowed and it must be the last statement."
  [statements]
  (when (> (count (filter (fn [[_ [name _ _]]]
                            (= name 'bend))
                          statements))
           1)
    (throw (BASICException. "Only one END statement allowed.")))
  (when-let [[_ [name & _]] (last statements)]
    (when-not (= name 'bend)
      (throw (BASICException. "END must be last statement")))))

;; BASIC Mini World
(defn- compile2
  "Compile the given statements to a letfn. Return a list for eval-UATION."
  [statements]
  (chk-duplicate-line-numbers statements)
  (chk-end-statement statements)
  (let [vars (gensym) arrays (gensym) data (gensym) done (gensym)
        bsin (gensym) bcos (gensym) btan (gensym) batn (gensym) bexp (gensym)
        babs (gensym) blog (gensym) bsqr (gensym) brnd (gensym) bint (gensym)
        bvref (gensym) bcall (gensym) array-info (gensym) baget (gensym)
        badim (gensym) baset (gensym) bpow (gensym) bne (gensym) pos (gensym)
        ctx {:vars vars,:arrays arrays,:data data,:done done :pos pos
             'sin bsin,'cos bcos,'tan btan,'atn batn,'exp bexp,'abs babs,
             'log blog,'sqr bsqr,'rnd brnd,'int bint,'bpow bpow,'bne bne,
             'bvref bvref,'baget baget,'badim badim,'baset baset,
             'bcall bcall}
        ;; create functions for all statements
        stmt-fns (map (fn [x y]
                        (translate-stmt x y ctx))
                      statements
                      (conj (into []
                                  (map (fn [[n node]] (line-num->fn n))
                                       (rest statements)))
                            done))
        ;; create functions specified by DEF statements
        def-fns (map (fn [[n node]] (gen-def-fns node ctx))
                     (filter (fn [[n node]]
                               (= (first node) 'bdef))
                             statements))]
    `(let [~vars (atom {})
           ~arrays (atom {})
           ~data (atom '~(collect-data statements))
           ~pos (atom 0)]
       (letfn
           [~@stmt-fns
            ~@def-fns
            ;; BASIC built-ins
            (~bsin [x#] (Math/sin x#))
            (~bcos [x#] (Math/cos x#))
            (~btan [x#] (Math/tan x#))
            (~batn [x#] (Math/atan x#))
            (~bexp [x#] (Math/exp x#))
            (~babs [x#] (Math/abs x#))
            (~blog [x#] (Math/log (Math/abs x#)))  ; BASIC
            (~bsqr [x#] (Math/sqrt (Math/abs x#))) ; semantics
            (~brnd [] (rand))
            (~bint [x#] (int x#))
            (~bpow [x# y#] (Math/pow x# y#))
            (~bne [x# y#] (not (== x# y#)))
            ;; 
            (~array-info
             [x#]
             ;; return nil if x# is not in arrays
             ;; else return [array rows cols] where cols will be 0 if 1d array
             (if-let [v# (@~arrays x#)]
               (let [rows# (count v#)]
                 (try
                   [v# rows# (count (aget v# 0))]
                   (catch UnsupportedOperationException e#
                     [v# rows# 0])))
               nil))
            ;; variable read
            (~bvref
             [x#]
             (if-let [v# (@~vars x#)]
               v#
               (throw (BASICException. (str x# " referenced before set")))))
            ;; array right-hand-side read
            (~baget
             ;; one dim read
             ([x# i#]
                (when (< i# 0)
                  (throw (BASICException.
                          (str "array index must be > 0, got (" i# ")"))))
                (if-let [[a# rows# cols#] (~array-info x#)]
                  (do
                    (when (> cols# 0)
                      (throw (BASICException.
                              (str "can't access multidemensional array ("
                                   x# ") with a single index"))))
                    (if (> i# rows#)
                      ;;  index out of bounds
                      (throw (BASICException. (str "array index (" i# ")"
                                                   " out of bounds")))
                      (if-let [element# (aget a# i#)]
                        ;; return the value
                        element#
                        (throw (BASICException.
                                (str x# "(" i# ")" " has not been set"))))))
                  (throw (BASICException. (str "array (" x# ")"
                                               " not defined")))))
             ;; two dim read
             ([x# i# j#]
                (when (< i# 0)
                  (throw (BASICException.
                          (str "row index must be > 0, got (" i# ")"))))
                (when (< j# 0)
                  (throw (BASICException.
                          (str "column index must be > 0, got (" j# ")"))))
                ;; x# is in arrays
                (if-let [[a# rows# cols#] (~array-info x#)]
                  (if (> i# rows#)
                    (BASICException. (str "array index (" i# ")" " out of"
                                          " bounds"))
                    (if (> j# cols#)
                      (BASICException. (str "array index (" j# ")"
                                            " out of bounds"))
                      (if-let [element# (aget a# i# j#)]
                        ;; return the value
                        element# 
                        (throw (BASICException.
                                (str x# "(" i# ")"
                                     " has not been set"))))))
                  (throw (BASICException. (str "array (" x# ")"
                                               " not defined"))))))
            ;; array creation in DIM statement
            (~badim
             ([x# n#]
                (when (<= n# 0)
                  (throw (BASICException.
                          (str x# " has invalid index (" n# ")"))))
                (swap! ~arrays #(assoc % x# (make-array Object (inc n#)))))
             ([x# n# m#]
                (when (<= n# 0)
                  (throw (BASICException.
                          (str x# " has invalid index (" n# ")"))))
                (when (<= m# 0)
                  (throw (BASICException.
                          (str x# " has invalid index (" m# ")"))))
                (swap! ~arrays #(assoc % x# (make-array Object
                                                        (inc n#)
                                                        (inc m#))))))
            ;; array setting for in READ statement
            (~baset
             ([x# i#]
                ;; array found in arrays
                (if-let [[a# rows# cols#] (~array-info x#)]
                  (if (> cols# 0)
                    ;; x# is 2d not 1d
                    (throw (BASICException.
                            (str x# " is a 2d array. Missing column"
                                 " index.")))
                    (if (> i# rows#)
                      ;; i# is out of bounds
                      (throw (BASICException.
                              (str x# "(" i# ") is out of bounds in"
                                   " READ statement.")))
                      ;; ok, read data and set
                      (if-let [data# (first @~data)]
                        (do (aset a# i# data#)
                            (swap! ~data rest))
                        (~done "READ out of data. Program terminated."))))
                  ;; new array if i# <= 10
                  (if (<= i# 10)
                    (let [a# ((~badim x# 10) x#)]
                      (if-let [data# (first @~data)]
                        (do (aset a# i# data#)
                            (swap! ~data rest))
                        (~done "READ out of data. Program terminated.")))
                    (throw (BASICException.
                            (str "index " i# " > 10 but no DIM statement"
                                 " for " x#))))))
             ([x# i# j#]
                (if-let [[a# rows# cols#] (~array-info x#)]
                  (do
                    (when (> i# rows#)
                      (str x# " row index (" i# ") is out of bounds in"
                           " READ statement."))
                    (when (> j# cols#)
                      (str x# " column index (" j# ") is out of bounds in"
                           " READ statement."))
                    (if-let [data# (first @~data)]
                      (do (aset a# i# j# data#)
                          (swap! ~data rest))
                      (~done "READ out of data. Program terminated.")))
                  ;; new array if i# and j# <= 10
                  (if (and (<= i# 10)
                           (<= j# 10))
                    (let [a# ((~badim x# 10 10) x#)]
                      (if-let [data# (first @~data)]
                        (do (aset a# i# j# data#)
                            (swap! ~data rest))
                        (~done "READ out of data. Program terminated.")))
                    (throw (BASICException.
                            (str "index " i# " or " j# " > 10 but no DIM"
                                 " statement for " x#)))))))
            ;; RND takes no arg
            (~bcall [x# & y#] (apply x# y#))
            ;; called by END, STOP or when READ is out of data
            (~done [msg#]
                   ;; (println "vars:")
                   ;; (pprint @~vars)
                   ;; (println "arrays:")
                   ;; (pprint @~arrays)
                   ;; (println "data:")
                   ;; (pprint @~data)
                   (throw (EndException. msg#)))]
         ;; call the function associated with the first line of the program
         (~(line-num->fn (ffirst statements)))))))

(defn run
  [code]
  (when-not  (empty? code)
    (when-let [result (basic2 code)]
      (try
        (eval (compile2 result))
        (catch Exception e
          (println)
          (println (.getMessage e)))))))

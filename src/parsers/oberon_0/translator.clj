;;; translator.clj
;;;
;;; Friday, May 10 2013

(ns ^{:doc "Translate Oberon-0 to runnable Clojure

* Arrays are vectors, Multi-dim arrays are vectors of vectors

"}
  parsers.oberon-0.translator)

(def ^{:doc "String to value lookup table.
The value can be nil, and integer, or a function."}
  sym-table {})

(defmacro $sym
  "Return the value of the symbol with name"
  [name]
  `(if-let [x# (get sym-table ~name)]
     x#
    (throw (Exception. (str "variable `" ~name "' has no value")))))

(defmacro $aref
  "Return the value of the vector v at index n"
  [v n]
  `(if (vector? ~v)
     (~v ~n)
     (throw (Exception. (str "illegal index on non-array")))))

(defmacro $rref
  "Return the value of the record member given the name of the member"
  [rec name]
  `(if (map? rec)
     (if-let [x (get rec name)]
       x
       (throw (Exception. "")))))


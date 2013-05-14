;;; peg_hlink.clj
;;;
;;; Saturday, April  7 2012

(ns ^{:doc "Write a PEG to html where rules are hyper linked.
Usage: (hlink \"path/to/*.peg\" \"out-file-name\")
The written html will use peg.css if available."}
    parsers.peg.peg_hlink
    (:use ralik.parsers.peg.peg)
    ;; (:require [clojure.contrib.string :as string])
    ;; (:use [clojure.walk :only [postwalk]])
    ;; (:use [clojure.pprint :only [pprint]])
    )

(def ^{:dynamic true
       :doc "Column where the next character will be written"}
  *col*)

(def ^{:dynamic true
       :doc "Column where the next / will be written"}
  *goal-col*)

(def ^{:dynamic true
       :doc "true if the next token should write a space before itself"}
  *space?*)

(declare write-rhs)

(defn inccol
  "Add n to *col*"
  [n]
  (set! *col* (+ *col* n)))

(defn spc
  "Return a string of n spaces"
  [n]
  (apply str (repeat n " ")))

(defn write-str
  [x]
  (if *space?*
    (do
      (print " <span id='peg-dquotes'>\"</span>")
      (inccol 2))
    (do
      (print "<span id='peg-dquotes'>\"</span>")
      (inccol 1)))
  (print "<span id='peg-literal'>")
  (print x) (inccol (count x))
  (print "</span>")
  (print "<span id='peg-dquotes'>\"</span>"))

(defn write-or
  [x]
  (write-rhs (first x))
  (when (next x)
    (newline)
    (print (spc *goal-col*))
    (set! *col* (+ *goal-col* 1))
    (print "/")
    (binding [*space?* true]
      (write-or (rest x)))))

(defn write-g
  [x]
  (if *space?*
    (do (print " (") (inccol 2))
    (do (print "(") (inccol 1)))
  (binding [*space?* false
            *goal-col* *col*]
    (write-rhs x))
  (print ")") (inccol 1))

(defn write-!
  [x]
  (print "<span id='peg-pre-op'>")
  (if *space?*
    (do (print " !") (inccol 2))
    (do (print "!") (inccol 1)))
  (print "</span>")
  (binding [*space?* false]
    (write-rhs x)))

(defn write-&
  [x]
  (print "<span id='peg-pre-op'>")
  (if *space?*
    (do (print " &") (inccol 2))
    (do (print "&") (inccol 1)))
  (print "</span>")
  (binding [*space?* false]
    (write-rhs x)))

(defn write-*
  [x]
  (write-rhs x)
  (print "<span id='peg-post-op'>")
  (print "*") (inccol 1)
  (print "</span>"))

(defn write-+
  [x]
  (write-rhs x)
  (print "<span id='peg-post-op'>")
  (print "+") (inccol 1)
  (print "</span>"))

(defn write-?
  [x]
  (write-rhs x)
  (print "<span id='peg-post-op'>")
  (print "?") (inccol 1)
  (print "</span>"))

(defn write-dot
  [x]
  (if *space?*
    (do (print " .")  (inccol 2))
    (do (print ".")  (inccol 1)))
  (binding [*space?* true]
    (write-rhs x)))

(defn write-cc
  [x]
  (print "<span id='peg-cc'>")
  (if *space?*
    (do (print " [") (inccol 2))
    (do (print "[") (inccol 1)))
  (print (first x))
  (print "]") (inccol 1)
  (print "</span>"))

(defn write-nt
  [x]
  (if *space?*
    (do (printf " <a href='#%s'>" (first x)) (inccol 1))
    (printf "<a href='#%s'>" (first x)))
  (print (first x))  (inccol (count (first x)))
  (print "</a>"))

(defn write-rhs
  [x]
  (cond
   (empty? x) nil
   (seq? x) (cond
             (symbol? (first x)) (case (first x)
                                   peg-g (write-g (rest x))
                                   peg-or (write-or (rest x))
                                   peg-! (write-! (rest x))
                                   peg-& (write-& (rest x))
                                   peg-* (write-* (rest x))
                                   peg-+ (write-+ (rest x))
                                   peg-? (write-? (rest x))
                                   peg-cc (write-cc (rest x))
                                   peg-nt (write-nt (rest x))
                                   _ (write-dot (rest x)))
             (string? (first x)) (do (write-str (first x))
                                     (binding [*space?* true]
                                       (write-rhs (rest x))))
             (= (first x) '_) (do (write-dot)
                                  (binding [*space?* true]
                                    (write-rhs (rest x))))
             :else (do (write-rhs (first x))
                       (binding [*space?* true]
                         (write-rhs (rest x)))))
   (string? x) (write-str x)
   (= x '_) (write-dot nil)))

(defn write-rule
  [name rhs]
  (println "<pre>")
  (printf "<a name='%s'><b>%s</b></a>\n" name, name)
  (print "  <-")
  (binding [*space?* true
            *goal-col* 3
            *col* 4]
    (write-rhs rhs))
  (println "</pre>"))

(defn hlink
  "Write a pretty-printed hyper-linked .html of a given PEG.
file-name specifies the source .peg file. out-file specifies the .html file to
write which will be over-written without warning."
  [file-name out-file]
  (if-let [res (sort-by first (seq (peg (slurp file-name))))]
    (spit out-file
          (with-out-str 
            (println "<html>")
            (println "<head>")
            (println "<link rel='stylesheet'")
            (println "type='text/css' href='peg.css' />")
            (println "</head><body>")
            (println "Source:" file-name)
            (doseq [[name rhs] res]
              (write-rule name rhs))
            (print "</body></html>")
            (newline)))))

;;; parser.clj
;;;
;;; Sunday, April  8 2012

(ns ^{:doc "Define a CSV parser as define at
http://en.wikipedia.org/wiki/Comma-separated_values#Basic_rules_and_examples"}
  parsers.csv.parser
  (:use ralik.core)
  (:use [ralik.atomics :only [eoi eol eol+ eol* blank*]])
  (:use [clojure.pprint :only [pprint]])
  (:import [ralik RalikException ParserException]))

(def ^{:doc "Number of fields in the first record."}
  field-count (atom nil))

(defn check-field-count
  "All records must have the same number of fields as the first record.
If one is found that does not, fail and print the offending record."
  [& fields]
  (if (not= (count fields) @field-count)
    (parse-error (loop [pos (dec *cur-pos*)]
                   (if (and (> pos 0)
                            ;; TODO: fucking \tab
                            (#{\space \newline \return}
                             (.charAt *text-to-parse* pos)))
                     (recur (dec pos))
                     pos))
                 (str "record must have " @field-count " field(s)"))
    (vec fields)))

(defgrammar csv
  "Parse a CSV file.

There is no widely followed CSV standard, only some generally accepted
guidelines.

A single field is:
  1. A \"literal string\" which may contain commas. An embedded \" is defined
     as two consecutive unescaped \", with no space between them. The string
     may contain hard line breaks or tabs which are retained in the
     string. All other characters are read individually, including escaped
     characters. \\n will be returned as two characters, \\ and n. Leading and
     trailing spaces or tabs outside the terminating \" are ignored.
  2. Any string of characters except a comma or an end of line terminator.
     Leading and trailing spaces and tabs are retained.
  3. Empty. Two consecutive commas ,, with no space between them or a comma at
     the end of a line or the end of input. In this case an empty string,
     \"\", is returned. If there is only spaces or tabs between two commas, a
     string containing the white space is returned.

A record consists of a one or more fields separated by a single comma and
ending in a line terminator or end of input. All records must have the same
number of fields. The parser enforces this. Empty lines are ignored.

The complete input consists of zero or more records.

Return a vector of records where each record is a vector of fields as
strings."
  [:start-rule Start
   :skipper nil
   :trace? false
   :print-err? true
   :ppfn identity]
  (Start (>g [1 3] eol* (<g? (FirstRecord))
             (<g* (>g (Record)
                      check-field-count))
             #(if (= %1 :empty)
                []
                (vec (concat [%1] %2)))))
  (FirstRecord (>g (Record)
                   #(do
                      (reset! field-count (count %&))
                      (vec %&))))
  (Record (<g 0 (<g_ 0 (Field) \,) (g| eol+ eoi)))
  (Field (<g 1 (g! eoi) (<g| (LiteralStr)
                             (<lex (g* (g- <_
                                           (g| \,
                                               eol)))))))
  (LiteralStr (<g 1 blank* (<lex \"
                                 (g* (g| (g \" \")
                                         (g- <_ \")))
                                 \")
                  blank*)))
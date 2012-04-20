(defproject ralik "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]]
  :aot [;; for ralik core
        ralik.RalikException
        ralik.CutException
        ;; for BASIC parser and runner
        ralik.parsers.basic2.BASICException
        ralik.parsers.basic2.ReturnException
        ralik.parsers.basic2.EndException
        ralik.parsers.basic2.NextException])
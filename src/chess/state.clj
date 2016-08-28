(ns tic-tac-toe.state
  "A model of the state in Tic-Tac-Toe and functions for update and lookup."
  (:use [clojure.test :only (is run-tests function?)]
        [clojure.repl :only (doc)]
        [clojure.pprint :only [pprint]]
        [test.core :only [is=]]))


(defn
  ^{:test (fn []
            (is= (my-reverse "ABC") "CBA"))}
  my-reverse [string]
  (clojure.string/join "" (reverse string)))

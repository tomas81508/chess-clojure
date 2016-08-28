(ns chess.core
  "A collection of pure functions for the game Chess."
  (:use [clojure.test :only (is run-tests function?)]
        [clojure.repl :only (doc)]
        [clojure.pprint :only [pprint]]
        [test.core :only [is=]])
  (:require [chess.state :as s]))

(defn
  ^{:test (fn []
            (is= (add 3 4) 9))}
  add [x y]
  (+ 2 x y))








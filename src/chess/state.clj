(ns chess.state
  "A model of the state and functions for update and lookup."
  (:use [clojure.test :only (is run-tests function?)]
        [clojure.repl :only (doc)]
        [clojure.pprint :only [pprint]]
        [test.core :only [is=]]))


(defn
  ^{:doc "Creates a board from the given strings."
    :test (fn []
            (is= (strings->board ".QK."
                                 "...."
                                 ".qk.")
                 {"a1" nil
                  "b1" {:piece :queen :owner "p2"}
                  "c1" {:piece :king :owner "p2"}
                  "d1" nil
                  "a2" nil
                  "b2" nil
                  "c2" nil
                  "d2" nil
                  "a3" nil
                  "b3" {:piece :queen :owner "p1"}
                  "c3" {:piece :king :owner "p1"}
                  "d3" nil}))}
  strings->board [& strings]
  ())

(defn
  ^{:doc "Gets the piece at the given position or nil if none is present."
    :test (fn []
            (is= (-> (strings->board "..K")
                     (get-piece "a1"))
                 nil)
            (is= (-> (strings->board "..K")
                     (get-piece "a3"))
                 {:piece :king :owner "p1"}))}
  get-piece [board position]
  ())

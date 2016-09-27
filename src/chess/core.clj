(ns chess.core
  "A collection of pure functions for the game Chess."
  (:use [clojure.test :only (is run-tests function?)]
        [clojure.repl :only (doc)]
        [clojure.pprint :only [pprint]]
        [test.core :only [is=]])
  (:require [chess.state :as s]))


(defn
  ^{:doc  "Determines all possible moves for a knight."
    :test (fn []
            (is= (get-valid-knight-moves (s/create-board "....."
                                                         ".n..."
                                                         "....."
                                                         "Q.k..")
                                         [1 1])
                 #{[0 3] [2 3] [3 0]}))}
  get-valid-knight-moves [board from-position]
  {:pre [(s/knight? (s/get-piece board from-position))]}
  (->> [[-2 -1] [-2 1] [2 -1] [2 1] [-1 2] [1 2] [-1 -2] [1 -2]]
       (map (fn [p]
              (map + from-position p)))
       (filter (fn [p]
                 (and (s/on-board? board p)
                      (not= (s/get-owner (s/get-piece board from-position))
                            (s/get-owner (s/get-piece board p))))))
       (into #{})))

(defn
  ^{:test (fn []
            (is (-> (s/create-board "n.."
                                    "..."
                                    "...")
                    (valid-knight-move? [0 0] [1 2])))
            (is (not (-> (s/create-board "n.."
                                         "..."
                                         "...")
                         (valid-knight-move? [0 0] [1 1])))))}
  valid-knight-move? [board from-position to-position]
  {:pre [(s/knight? (s/get-piece board from-position))]}
  (contains? (get-valid-knight-moves board from-position) to-position))



























































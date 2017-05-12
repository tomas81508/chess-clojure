(ns chess.games
  "Mutation of the state."
  (:use [clojure.test :only (deftest is run-tests function?)]
        [clojure.repl :only (doc)]
        [clojure.pprint :only [pprint]]
        [test.core :only [is= is-not]])
  (:require [chess.mutator :as m]))

(def game-atom (atom (list)))

(defn create-game! []
  (m/create-game! game-atom))

(defn move! [player-id from-position to-position]
  (m/move! game-atom player-id from-position to-position))

(defn castle! [player-id from-position to-position]
  (m/castle! game-atom player-id from-position to-position))

(defn get-game []
  @game-atom)

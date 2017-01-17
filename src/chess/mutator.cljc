(ns chess.mutator
  "Mutation of the state."
  (:use [clojure.test :only (deftest is run-tests function?)]
        [clojure.repl :only (doc)]
        [clojure.pprint :only [pprint]]
        [test.core :only [is= is-not]])
  (:require [chess.core :as core]
            [chess.state :as s]))

(defn create-game! [game-atom]
  (reset! game-atom (core/create-classic-game-state)))

(defn move! [game-atom player-id from-position to-position]
  (swap! game-atom core/move player-id from-position to-position))

(defn castle! [game-atom player-id from-position to-position]
  (swap! game-atom core/castle player-id from-position to-position))

(defn get-game [game-atom]
  @game-atom)



(deftest A-simple-game
  (let [game-atom (atom {})]
    (create-game! game-atom)
    (move! game-atom :large [6 4] [4 4])
    (is= (:type (s/get-piece (get-game game-atom) [4 4])) :pawn)
    (move! game-atom :small [0 1] [2 0])
    (is= (:type (s/get-piece (get-game game-atom) [2 0])) :knight)))





















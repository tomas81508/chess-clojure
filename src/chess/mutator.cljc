(ns chess.mutator
  "Mutation of the state."
  (:use [clojure.test :only (deftest is run-tests function?)]
        [clojure.repl :only (doc)]
        [clojure.pprint :only [pprint]]
        [test.core :only [is= is-not]])
  (:require [chess.core :as core]
            [chess.state :as s]
            [chess.history :as history]))

(defn create-game! [game-atom]
  (reset! game-atom (history/create-state)))

(defn move! [game-atom player-id from-position to-position]
  (swap! game-atom history/move player-id from-position to-position))

(defn undo! [game-atom player-id]
  (swap! game-atom history/undo))

(defn redo! [game-atom player-id]
  (swap! game-atom history/redo))

(defn get-game [game-atom]
  @game-atom)

(deftest A-simple-game
  (let [game-atom (atom nil)]
    (create-game! game-atom)
    (move! game-atom :large [6 4] [4 4])
    (is= (:type (s/get-piece (history/get-current-game-state (get-game game-atom)) [4 4])) :pawn)
    (move! game-atom :small [0 1] [2 0])
    (is= (:type (s/get-piece (history/get-current-game-state (get-game game-atom)) [2 0])) :knight)
    (undo! game-atom :small)
    (is= (:type (s/get-piece (history/get-current-game-state (get-game game-atom)) [0 1])) :knight)
    (redo! game-atom :small)
    (is= (:type (s/get-piece (history/get-current-game-state (get-game game-atom)) [2 0])) :knight)))

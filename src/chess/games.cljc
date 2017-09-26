(ns chess.games
    "Mutation of the state."
    (:use [clojure.test :only (deftest is run-tests function?)]
      [clojure.repl :only (doc)]
      [clojure.pprint :only [pprint]]
      [test.core :only [is= is-not]])
    (:require [chess.mutator :as m]))

(def game-atom (atom {:game-states   (list)
                      :current-index 0}))

(defn create-game! []
      (m/create-game! game-atom))

(defn move! [player-id from-position to-position]
      (m/move! game-atom player-id from-position to-position))

;(defn castle! [player-id from-position to-position]
;  (m/castle! game-atom player-id from-position to-position))

(defn redo! [player-id]
      (m/redo! game-atom player-id))

(defn undo! [player-id]
      (m/undo! game-atom player-id))

(defn get-game []
      @game-atom)

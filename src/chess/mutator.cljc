(ns chess.mutator
  "Mutation of the state."
  (:use [clojure.test :only (deftest is run-tests function?)]
        [clojure.repl :only (doc)]
        [clojure.pprint :only [pprint]]
        [test.core :only [is= is-not]])
  (:require [chess.core :as core]
            [chess.state :as s]
            [chess.interop :as i]))

(def game-atom (atom (core/create-classic-game-state)))

(defn create-game! []
  (reset! game-atom (core/create-classic-game-state)))

(defn move! [player-id from-position to-position]
  (swap! game-atom core/move player-id from-position to-position))

(defn get-game []
  @game-atom)























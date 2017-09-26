(ns chess.mutator
    "Mutation of the state."
    (:use [clojure.test :only (deftest is run-tests function?)]
      [clojure.repl :only (doc)]
      [clojure.pprint :only [pprint]]
      [test.core :only [is= is-not]])
    (:require [chess.core :as core]
      [chess.state :as s]))

(defn index-inc
      {:test (fn []
                 (is= (-> (index-inc {:game-states [{}] :current-index 0})
                          (:current-index))
                      0)
                 (is= (-> (index-inc {:game-states [{} {} {} {}] :current-index 2})
                          (:current-index))
                      3))}
      [game]
      (update game :current-index (fn [current-index]
                                      (min (inc current-index)
                                           (dec (count (:game-states game)))))))

(defn index-dec
      {:test (fn []
                 (is= (:current-index (index-dec {:current-index 0}))
                      0)
                 (is= (:current-index (index-dec {:current-index 2}))
                      1))}
      [game]
      (update game :current-index (fn [current-index]
                                      (max 0 (dec current-index)))))

(defn get-current-game-state [game]
      (nth (:game-states game) (:current-index game)))

(defn create-game! [game-atom]
      (-> (reset! game-atom {:game-states   (list (core/create-classic-game-state))
                             :current-index 0})
          (get-current-game-state)))

(defn move! [game-atom player-id from-position to-position]
      (-> (swap! game-atom (fn [game]
                               (-> game
                                   (update :game-states (fn [game-states]
                                                            (as-> game-states $
                                                                  (drop (:current-index game) $)
                                                                  (conj $ (core/move (first $)
                                                                                     player-id
                                                                                     from-position
                                                                                     to-position)))))
                                   (assoc :current-index 0))))
          (get-current-game-state)))

;(defn castle! [game-atom player-id from-position to-position]
;      (get-current-game-state @(swap! game-atom conj (core/castle (first @game-atom) player-id from-position to-position))))

(defn undo! [game-atom player-id]
      (-> (swap! game-atom index-inc)
          (get-current-game-state)))

(defn redo! [game-atom player-id]
      (-> (swap! game-atom index-dec)
          (get-current-game-state)))

(defn get-game [game-atom]
      (get-current-game-state @game-atom))



(deftest A-simple-game
         (let [game-atom (atom nil)]
              (create-game! game-atom)
              (move! game-atom :large [6 4] [4 4])
              (is= (:type (s/get-piece (get-game game-atom) [4 4])) :pawn)
              (move! game-atom :small [0 1] [2 0])
              (is= (:type (s/get-piece (get-game game-atom) [2 0])) :knight)
              (undo! game-atom :small)
              (is= (:type (s/get-piece (get-game game-atom) [0 1])) :knight)
              (redo! game-atom :small)
              (is= (:type (s/get-piece (get-game game-atom) [2 0])) :knight)))

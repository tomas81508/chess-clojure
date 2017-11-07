(ns chess.history
  (:require [chess.core :as core]
            [test.core :refer [is= is-not]]
            [chess.state :as state]))

(defn create-state
  []
  {:game-states    (list (core/create-classic-game-state))
   :current-index  0
   :previous-moves []})

(defn undo
  {:test (fn []
           (is= (-> (undo {:game-states [{}] :current-index 0})
                    (:current-index))
                0)
           (is= (-> (undo {:game-states [{} {} {} {}] :current-index 2})
                    (:current-index))
                3))}
  [game]
  (update game :current-index (fn [current-index]
                                (min (inc current-index)
                                     (dec (count (:game-states game)))))))

(defn redo
  {:test (fn []
           (is= (:current-index (redo {:current-index 0}))
                0)
           (is= (:current-index (redo {:current-index 2}))
                1))}
  [game]
  (update game :current-index (fn [current-index]
                                (max 0 (dec current-index)))))

(defn get-current-game-state [game]
  (nth (:game-states game) (:current-index game)))

(defn get-previous-moves
  {:test (fn []
           (is= (get-previous-moves {:game-states    [{}]
                                     :current-index  0
                                     :previous-moves []})
                [])
           (is= (get-previous-moves {:game-states    [{} {} {}]
                                     :current-index  2
                                     :previous-moves [{} {}]})
                [])
           (is= (get-previous-moves {:game-states    [{} {} {}]
                                     :current-index  0
                                     :previous-moves [:b :a]})
                [:a :b]))}
  [game]
  (reverse (drop (:current-index game) (:previous-moves game))))

(defn move
  {:test (fn []
           (let [state (-> (create-state)
                           (move :large [0 1] [0 2])
                           (undo)
                           (move :large [1 1] [1 2]))]
             (is= (count (get-previous-moves state))
                  1)))}
  [game player-id from-position to-position]
  (-> game
      (update :game-states (fn [game-states]
                             (as-> game-states $
                                   (drop (:current-index game) $)
                                   (conj $ (core/move (first $)
                                                      player-id
                                                      from-position
                                                      to-position)))))
      (assoc :current-index 0)
      (update :previous-moves (fn [previous-moves]
                                (as-> previous-moves $
                                      (drop (:current-index game) $)
                                      (conj $ {:piece-type       (:type (state/get-piece (get-current-game-state game) from-position))
                                               :from-coordinates from-position
                                               :to-coordinates   to-position
                                               :owner            player-id}))))))
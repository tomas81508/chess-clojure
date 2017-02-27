(ns chess.mapper
  (:require
    [chess.core :refer [create-classic-game-state]]
    [test.core :refer [is= is-not error?]]))

(defn
  ^{:doc "The board cells are sorted in order by row and then column."
    :test (fn []
            (is= (game->view-game (create-classic-game-state))
                 {:playerInTurn :large
                  :players      [{:id :large} {:id :small}]
                  :board        [{:row 0, :column 0, :piece {:type :rook, :owner :small}}
                                 {:row 0, :column 1, :piece {:type :knight, :owner :small}}
                                 {:row 0, :column 2, :piece {:type :bishop, :owner :small}}
                                 {:row 0, :column 3, :piece {:type :queen, :owner :small}}
                                 {:row 0, :column 4, :piece {:type :king, :owner :small}}
                                 {:row 0, :column 5, :piece {:type :bishop, :owner :small}}
                                 {:row 0, :column 6, :piece {:type :knight, :owner :small}}
                                 {:row 0, :column 7, :piece {:type :rook, :owner :small}}
                                 {:row 1, :column 0, :piece {:type :pawn, :owner :small}}
                                 {:row 1, :column 1, :piece {:type :pawn, :owner :small}}
                                 {:row 1, :column 2, :piece {:type :pawn, :owner :small}}
                                 {:row 1, :column 3, :piece {:type :pawn, :owner :small}}
                                 {:row 1, :column 4, :piece {:type :pawn, :owner :small}}
                                 {:row 1, :column 5, :piece {:type :pawn, :owner :small}}
                                 {:row 1, :column 6, :piece {:type :pawn, :owner :small}}
                                 {:row 1, :column 7, :piece {:type :pawn, :owner :small}}
                                 {:row 2, :column 0, :piece nil}
                                 {:row 2, :column 1, :piece nil}
                                 {:row 2, :column 2, :piece nil}
                                 {:row 2, :column 3, :piece nil}
                                 {:row 2, :column 4, :piece nil}
                                 {:row 2, :column 5, :piece nil}
                                 {:row 2, :column 6, :piece nil}
                                 {:row 2, :column 7, :piece nil}
                                 {:row 3, :column 0, :piece nil}
                                 {:row 3, :column 1, :piece nil}
                                 {:row 3, :column 2, :piece nil}
                                 {:row 3, :column 3, :piece nil}
                                 {:row 3, :column 4, :piece nil}
                                 {:row 3, :column 5, :piece nil}
                                 {:row 3, :column 6, :piece nil}
                                 {:row 3, :column 7, :piece nil}
                                 {:row 4, :column 0, :piece nil}
                                 {:row 4, :column 1, :piece nil}
                                 {:row 4, :column 2, :piece nil}
                                 {:row 4, :column 3, :piece nil}
                                 {:row 4, :column 4, :piece nil}
                                 {:row 4, :column 5, :piece nil}
                                 {:row 4, :column 6, :piece nil}
                                 {:row 4, :column 7, :piece nil}
                                 {:row 5, :column 0, :piece nil}
                                 {:row 5, :column 1, :piece nil}
                                 {:row 5, :column 2, :piece nil}
                                 {:row 5, :column 3, :piece nil}
                                 {:row 5, :column 4, :piece nil}
                                 {:row 5, :column 5, :piece nil}
                                 {:row 5, :column 6, :piece nil}
                                 {:row 5, :column 7, :piece nil}
                                 {:row 6, :column 0, :piece {:type :pawn, :owner :large}}
                                 {:row 6, :column 1, :piece {:type :pawn, :owner :large}}
                                 {:row 6, :column 2, :piece {:type :pawn, :owner :large}}
                                 {:row 6, :column 3, :piece {:type :pawn, :owner :large}}
                                 {:row 6, :column 4, :piece {:type :pawn, :owner :large}}
                                 {:row 6, :column 5, :piece {:type :pawn, :owner :large}}
                                 {:row 6, :column 6, :piece {:type :pawn, :owner :large}}
                                 {:row 6, :column 7, :piece {:type :pawn, :owner :large}}
                                 {:row 7, :column 0, :piece {:type :rook, :owner :large}}
                                 {:row 7, :column 1, :piece {:type :knight, :owner :large}}
                                 {:row 7, :column 2, :piece {:type :bishop, :owner :large}}
                                 {:row 7, :column 3, :piece {:type :queen, :owner :large}}
                                 {:row 7, :column 4, :piece {:type :king, :owner :large}}
                                 {:row 7, :column 5, :piece {:type :bishop, :owner :large}}
                                 {:row 7, :column 6, :piece {:type :knight, :owner :large}}
                                 {:row 7, :column 7, :piece {:type :rook, :owner :large}}]}))}
  game->view-game [game]
  {:playerInTurn (:player-in-turn game)
   :board        (->> (:board game)
                      (map (fn [[[row column] piece]]
                             {:row    row
                              :column column
                              :piece  (when piece
                                        {:type  (:type piece)
                                         :owner (:owner piece)})}))
                      (sort-by (juxt :row :column)))
   :players      (map (fn [player]
                        {:id (:id player)})
                      (:players game))})

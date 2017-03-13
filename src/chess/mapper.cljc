(ns chess.mapper
  (:require
    [chess.core :refer [create-classic-game-state get-valid-moves]]
    [test.core :refer [is= is-not error?]]))

(defn
  ^{:doc  "The board cells are sorted in order by row and then column."
    :test (fn []
            (is= (game->view-game (create-classic-game-state))
                 {:playerInTurn :large
                  :players      [{:id :large} {:id :small}]
                  :board        [{:row 0, :column 0, :piece {:type :rook, :owner :small :valid-moves #{}}}
                                 {:row 0, :column 1, :piece {:type :knight, :owner :small :valid-moves #{}}}
                                 {:row 0, :column 2, :piece {:type :bishop, :owner :small :valid-moves #{}}}
                                 {:row 0, :column 3, :piece {:type :queen, :owner :small :valid-moves #{}}}
                                 {:row 0, :column 4, :piece {:type :king, :owner :small :valid-moves #{}}}
                                 {:row 0, :column 5, :piece {:type :bishop, :owner :small :valid-moves #{}}}
                                 {:row 0, :column 6, :piece {:type :knight, :owner :small :valid-moves #{}}}
                                 {:row 0, :column 7, :piece {:type :rook, :owner :small :valid-moves #{}}}
                                 {:row 1, :column 0, :piece {:type :pawn, :owner :small :valid-moves #{}}}
                                 {:row 1, :column 1, :piece {:type :pawn, :owner :small :valid-moves #{}}}
                                 {:row 1, :column 2, :piece {:type :pawn, :owner :small :valid-moves #{}}}
                                 {:row 1, :column 3, :piece {:type :pawn, :owner :small :valid-moves #{}}}
                                 {:row 1, :column 4, :piece {:type :pawn, :owner :small :valid-moves #{}}}
                                 {:row 1, :column 5, :piece {:type :pawn, :owner :small :valid-moves #{}}}
                                 {:row 1, :column 6, :piece {:type :pawn, :owner :small :valid-moves #{}}}
                                 {:row 1, :column 7, :piece {:type :pawn, :owner :small :valid-moves #{}}}
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
                                 {:row 6, :column 0, :piece {:type :pawn, :owner :large :valid-moves #{[5 0] [4 0]}}}
                                 {:row 6, :column 1, :piece {:type :pawn, :owner :large :valid-moves #{[5 1] [4 1]}}}
                                 {:row 6, :column 2, :piece {:type :pawn, :owner :large :valid-moves #{[5 2] [4 2]}}}
                                 {:row 6, :column 3, :piece {:type :pawn, :owner :large :valid-moves #{[5 3] [4 3]}}}
                                 {:row 6, :column 4, :piece {:type :pawn, :owner :large :valid-moves #{[5 4] [4 4]}}}
                                 {:row 6, :column 5, :piece {:type :pawn, :owner :large :valid-moves #{[5 5] [4 5]}}}
                                 {:row 6, :column 6, :piece {:type :pawn, :owner :large :valid-moves #{[5 6] [4 6]}}}
                                 {:row 6, :column 7, :piece {:type :pawn, :owner :large :valid-moves #{[5 7] [4 7]}}}
                                 {:row 7, :column 0, :piece {:type :rook, :owner :large :valid-moves #{}}}
                                 {:row 7, :column 1, :piece {:type :knight, :owner :large :valid-moves #{[5 0] [5 2]}}}
                                 {:row 7, :column 2, :piece {:type :bishop, :owner :large :valid-moves #{}}}
                                 {:row 7, :column 3, :piece {:type :queen, :owner :large :valid-moves #{}}}
                                 {:row 7, :column 4, :piece {:type :king, :owner :large :valid-moves #{}}}
                                 {:row 7, :column 5, :piece {:type :bishop, :owner :large :valid-moves #{}}}
                                 {:row 7, :column 6, :piece {:type :knight, :owner :large :valid-moves #{[5 5] [5 7]}}}
                                 {:row 7, :column 7, :piece {:type :rook, :owner :large :valid-moves #{}}}]}))}
  game->view-game [game]
  {:playerInTurn (:player-in-turn game)
   :board        (->> (:board game)
                      (map (fn [[[row column] piece]]
                             {:row    row
                              :column column
                              :piece  (when piece
                                        {:type        (:type piece)
                                         :owner       (:owner piece)
                                         :valid-moves (get-valid-moves game [row column])})}))
                      (sort-by (juxt :row :column)))
   :players      (map (fn [player]
                        {:id (:id player)})
                      (:players game))})

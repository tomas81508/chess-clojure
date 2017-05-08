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
                  :board        [{:row 0, :column 0, :piece {:type :rook, :owner :small :valid-moves #{} :id "1"}}
                                 {:row 0, :column 1, :piece {:type :knight, :owner :small :valid-moves #{} :id "2"}}
                                 {:row 0, :column 2, :piece {:type :bishop, :owner :small :valid-moves #{} :id "3"}}
                                 {:row 0, :column 3, :piece {:type :queen, :owner :small :valid-moves #{} :id "4"}}
                                 {:row 0, :column 4, :piece {:type :king, :owner :small :valid-moves #{} :id "5"}}
                                 {:row 0, :column 5, :piece {:type :bishop, :owner :small :valid-moves #{} :id "6"}}
                                 {:row 0, :column 6, :piece {:type :knight, :owner :small :valid-moves #{} :id "7"}}
                                 {:row 0, :column 7, :piece {:type :rook, :owner :small :valid-moves #{} :id "8"}}
                                 {:row 1, :column 0, :piece {:type :pawn, :owner :small :valid-moves #{} :id "9"}}
                                 {:row 1, :column 1, :piece {:type :pawn, :owner :small :valid-moves #{} :id "10"}}
                                 {:row 1, :column 2, :piece {:type :pawn, :owner :small :valid-moves #{} :id "11"}}
                                 {:row 1, :column 3, :piece {:type :pawn, :owner :small :valid-moves #{} :id "12"}}
                                 {:row 1, :column 4, :piece {:type :pawn, :owner :small :valid-moves #{} :id "13"}}
                                 {:row 1, :column 5, :piece {:type :pawn, :owner :small :valid-moves #{} :id "14"}}
                                 {:row 1, :column 6, :piece {:type :pawn, :owner :small :valid-moves #{} :id "15"}}
                                 {:row 1, :column 7, :piece {:type :pawn, :owner :small :valid-moves #{} :id "16"}}
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
                                 {:row 6, :column 0, :piece {:type :pawn, :owner :large :valid-moves #{[5 0] [4 0]} :id "17"}}
                                 {:row 6, :column 1, :piece {:type :pawn, :owner :large :valid-moves #{[5 1] [4 1]} :id "18"}}
                                 {:row 6, :column 2, :piece {:type :pawn, :owner :large :valid-moves #{[5 2] [4 2]} :id "19"}}
                                 {:row 6, :column 3, :piece {:type :pawn, :owner :large :valid-moves #{[5 3] [4 3]} :id "20"}}
                                 {:row 6, :column 4, :piece {:type :pawn, :owner :large :valid-moves #{[5 4] [4 4]} :id "21"}}
                                 {:row 6, :column 5, :piece {:type :pawn, :owner :large :valid-moves #{[5 5] [4 5]} :id "22"}}
                                 {:row 6, :column 6, :piece {:type :pawn, :owner :large :valid-moves #{[5 6] [4 6]} :id "23"}}
                                 {:row 6, :column 7, :piece {:type :pawn, :owner :large :valid-moves #{[5 7] [4 7]} :id "24"}}
                                 {:row 7, :column 0, :piece {:type :rook, :owner :large :valid-moves #{} :id "25"}}
                                 {:row 7, :column 1, :piece {:type :knight, :owner :large :valid-moves #{[5 0] [5 2]} :id "26"}}
                                 {:row 7, :column 2, :piece {:type :bishop, :owner :large :valid-moves #{} :id "27"}}
                                 {:row 7, :column 3, :piece {:type :queen, :owner :large :valid-moves #{} :id "28"}}
                                 {:row 7, :column 4, :piece {:type :king, :owner :large :valid-moves #{} :id "29"}}
                                 {:row 7, :column 5, :piece {:type :bishop, :owner :large :valid-moves #{} :id "30"}}
                                 {:row 7, :column 6, :piece {:type :knight, :owner :large :valid-moves #{[5 5] [5 7]} :id "31"}}
                                 {:row 7, :column 7, :piece {:type :rook, :owner :large :valid-moves #{} :id "32"}}]}))}
  game->view-game [game]
  {:playerInTurn (:player-in-turn game)
   :board        (->> (:board game)
                      (map (fn [[[row column] piece]]
                             {:row    row
                              :column column
                              :piece  (when piece
                                        {:type        (:type piece)
                                         :owner       (:owner piece)
                                         :valid-moves (get-valid-moves game [row column])
                                         :id          (:id piece)})}))
                      (sort-by (juxt :row :column)))
   :players      (map (fn [player]
                        {:id (:id player)})
                      (:players game))})


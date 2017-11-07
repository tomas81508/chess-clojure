(ns chess.mapper
  (:require [chess.spec]
            [clojure.spec :refer [valid? explain]]
            [clojure.test :refer [is]]
            [chess.core :refer [get-valid-moves move]]
            [test.core :refer [is= is-not error?]]
            [chess.history :as history]))

(defn game->view-game
  "The board cells are sorted in order by row and then column."
  {:test (fn []
           (is (valid? :chess.spec/game (game->view-game (history/create-state))))
           (let [previous-moves (-> (history/create-state)
                                    (history/move :large [0 1] [0 2])
                                    (history/get-previous-moves))]
             (is= previous-moves [{:piece-type :pawn :owner :large :from-coordinates [0 1] :to-coordinates [0 2]}])
             (valid? :chess.spec/previous-moves previous-moves))
           (is= (game->view-game (history/create-state))
                {:board          [{:coordinates [0 7]
                                   :piece       {:id          "25"
                                                 :owner       :small
                                                 :type        :rook
                                                 :valid-moves #{}}}
                                  {:coordinates [1 7]
                                   :piece       {:id          "26"
                                                 :owner       :small
                                                 :type        :knight
                                                 :valid-moves #{}}}
                                  {:coordinates [2 7]
                                   :piece       {:id          "27"
                                                 :owner       :small
                                                 :type        :bishop
                                                 :valid-moves #{}}}
                                  {:coordinates [3 7]
                                   :piece       {:id          "28"
                                                 :owner       :small
                                                 :type        :queen
                                                 :valid-moves #{}}}
                                  {:coordinates [4 7]
                                   :piece       {:id          "29"
                                                 :owner       :small
                                                 :type        :king
                                                 :valid-moves #{}}}
                                  {:coordinates [5 7]
                                   :piece       {:id          "30"
                                                 :owner       :small
                                                 :type        :bishop
                                                 :valid-moves #{}}}
                                  {:coordinates [6 7]
                                   :piece       {:id          "31"
                                                 :owner       :small
                                                 :type        :knight
                                                 :valid-moves #{}}}
                                  {:coordinates [7 7]
                                   :piece       {:id          "32"
                                                 :owner       :small
                                                 :type        :rook
                                                 :valid-moves #{}}}
                                  {:coordinates [0 6]
                                   :piece       {:id          "17"
                                                 :owner       :small
                                                 :type        :pawn
                                                 :valid-moves #{}}}
                                  {:coordinates [1 6]
                                   :piece       {:id          "18"
                                                 :owner       :small
                                                 :type        :pawn
                                                 :valid-moves #{}}}
                                  {:coordinates [2 6]
                                   :piece       {:id          "19"
                                                 :owner       :small
                                                 :type        :pawn
                                                 :valid-moves #{}}}
                                  {:coordinates [3 6]
                                   :piece       {:id          "20"
                                                 :owner       :small
                                                 :type        :pawn
                                                 :valid-moves #{}}}
                                  {:coordinates [4 6]
                                   :piece       {:id          "21"
                                                 :owner       :small
                                                 :type        :pawn
                                                 :valid-moves #{}}}
                                  {:coordinates [5 6]
                                   :piece       {:id          "22"
                                                 :owner       :small
                                                 :type        :pawn
                                                 :valid-moves #{}}}
                                  {:coordinates [6 6]
                                   :piece       {:id          "23"
                                                 :owner       :small
                                                 :type        :pawn
                                                 :valid-moves #{}}}
                                  {:coordinates [7 6]
                                   :piece       {:id          "24"
                                                 :owner       :small
                                                 :type        :pawn
                                                 :valid-moves #{}}}
                                  {:coordinates [0 5]
                                   :piece       nil}
                                  {:coordinates [1 5]
                                   :piece       nil}
                                  {:coordinates [2 5]
                                   :piece       nil}
                                  {:coordinates [3 5]
                                   :piece       nil}
                                  {:coordinates [4 5]
                                   :piece       nil}
                                  {:coordinates [5 5]
                                   :piece       nil}
                                  {:coordinates [6 5]
                                   :piece       nil}
                                  {:coordinates [7 5]
                                   :piece       nil}
                                  {:coordinates [0 4]
                                   :piece       nil}
                                  {:coordinates [1 4]
                                   :piece       nil}
                                  {:coordinates [2 4]
                                   :piece       nil}
                                  {:coordinates [3 4]
                                   :piece       nil}
                                  {:coordinates [4 4]
                                   :piece       nil}
                                  {:coordinates [5 4]
                                   :piece       nil}
                                  {:coordinates [6 4]
                                   :piece       nil}
                                  {:coordinates [7 4]
                                   :piece       nil}
                                  {:coordinates [0 3]
                                   :piece       nil}
                                  {:coordinates [1 3]
                                   :piece       nil}
                                  {:coordinates [2 3]
                                   :piece       nil}
                                  {:coordinates [3 3]
                                   :piece       nil}
                                  {:coordinates [4 3]
                                   :piece       nil}
                                  {:coordinates [5 3]
                                   :piece       nil}
                                  {:coordinates [6 3]
                                   :piece       nil}
                                  {:coordinates [7 3]
                                   :piece       nil}
                                  {:coordinates [0 2]
                                   :piece       nil}
                                  {:coordinates [1 2]
                                   :piece       nil}
                                  {:coordinates [2 2]
                                   :piece       nil}
                                  {:coordinates [3 2]
                                   :piece       nil}
                                  {:coordinates [4 2]
                                   :piece       nil}
                                  {:coordinates [5 2]
                                   :piece       nil}
                                  {:coordinates [6 2]
                                   :piece       nil}
                                  {:coordinates [7 2]
                                   :piece       nil}
                                  {:coordinates [0 1]
                                   :piece       {:id          "9"
                                                 :owner       :large
                                                 :type        :pawn
                                                 :valid-moves #{[0 2] [0 3]}}}
                                  {:coordinates [1 1]
                                   :piece       {:id          "10"
                                                 :owner       :large
                                                 :type        :pawn
                                                 :valid-moves #{[1 2] [1 3]}}}
                                  {:coordinates [2 1]
                                   :piece       {:id          "11"
                                                 :owner       :large
                                                 :type        :pawn
                                                 :valid-moves #{[2 2] [2 3]}}}
                                  {:coordinates [3 1]
                                   :piece       {:id          "12"
                                                 :owner       :large
                                                 :type        :pawn
                                                 :valid-moves #{[3 2] [3 3]}}}
                                  {:coordinates [4 1]
                                   :piece       {:id          "13"
                                                 :owner       :large
                                                 :type        :pawn
                                                 :valid-moves #{[4 2] [4 3]}}}
                                  {:coordinates [5 1]
                                   :piece       {:id          "14"
                                                 :owner       :large
                                                 :type        :pawn
                                                 :valid-moves #{[5 2] [5 3]}}}
                                  {:coordinates [6 1]
                                   :piece       {:id          "15"
                                                 :owner       :large
                                                 :type        :pawn
                                                 :valid-moves #{[6 2] [6 3]}}}
                                  {:coordinates [7 1]
                                   :piece       {:id          "16"
                                                 :owner       :large
                                                 :type        :pawn
                                                 :valid-moves #{[7 2] [7 3]}}}
                                  {:coordinates [0 0]
                                   :piece       {:id          "1"
                                                 :owner       :large
                                                 :type        :rook
                                                 :valid-moves #{}}}
                                  {:coordinates [1 0]
                                   :piece       {:id          "2"
                                                 :owner       :large
                                                 :type        :knight
                                                 :valid-moves #{[0 2] [2 2]}}}
                                  {:coordinates [2 0]
                                   :piece       {:id          "3"
                                                 :owner       :large
                                                 :type        :bishop
                                                 :valid-moves #{}}}
                                  {:coordinates [3 0]
                                   :piece       {:id          "4"
                                                 :owner       :large
                                                 :type        :queen
                                                 :valid-moves #{}}}
                                  {:coordinates [4 0]
                                   :piece       {:id          "5"
                                                 :owner       :large
                                                 :type        :king
                                                 :valid-moves #{}}}
                                  {:coordinates [5 0]
                                   :piece       {:id          "6"
                                                 :owner       :large
                                                 :type        :bishop
                                                 :valid-moves #{}}}
                                  {:coordinates [6 0]
                                   :piece       {:id          "7"
                                                 :owner       :large
                                                 :type        :knight
                                                 :valid-moves #{[5 2] [7 2]}}}
                                  {:coordinates [7 0]
                                   :piece       {:id          "8"
                                                 :owner       :large
                                                 :type        :rook
                                                 :valid-moves #{}}}]
                 :playerInTurn   :large
                 :players        [{:id :large}
                                  {:id :small}]
                 :previous-moves []}))}
  [game]
  (let [game-state (history/get-current-game-state game)]
    {:playerInTurn   (:player-in-turn game-state)
     :board          (->> (:board game-state)
                          (map (fn [[coordinates piece]]
                                 {:coordinates coordinates
                                  :piece       (when piece
                                                 {:type        (:type piece)
                                                  :owner       (:owner piece)
                                                  :valid-moves (get-valid-moves game-state coordinates)
                                                  :id          (:id piece)})}))
                          (sort-by (juxt (comp - second :coordinates) (comp first :coordinates))))
     :previous-moves (history/get-previous-moves game)
     :players        (map (fn [player]
                            {:id (:id player)})
                          (:players game-state))}))

(ns chess.spec
  (:require [clojure.spec :as s]))

(s/def ::coordinate (s/and int?
                           (fn [x] (<= 0 x 7))))

(s/def ::row ::coordinate)
(s/explain ::row 9)

(s/def ::column ::coordinate)

(s/def ::id (s/or :keyword keyword?
                  :string string?))

(s/valid? ::id :white)
(s/valid? ::id :small)
(s/explain ::id "kaka")
(s/explain ::id 2)

(s/def ::playerInTurn ::id)

(s/def ::player (s/keys :req-un [::id]))

(s/valid? ::player {:id :small})

(s/def ::players (s/and (s/+ ::player)
                        (fn [x] (= (count x) 2))))

(s/valid? ::players [{:id :large} {:id :large}])

(s/def ::type #{:rook :knight :bishop :pawn :queen :king})

(s/def ::owner ::id)

(s/def ::coordinates (s/and (s/+ ::coordinate)
                            (fn [x] (= (count x) 2))))

(s/explain ::coordinates [1 2 3])

(s/def ::valid-moves (s/and set?
                            (s/* ::coordinates)))

(s/def ::piece (s/or :something (s/keys :req-un [::type ::owner ::valid-moves ::id])
                     :nothing nil?))


(s/def ::cell (s/keys :req-un [::row ::column ::piece]))

(s/def ::board (s/+ ::cell))

(s/def ::game (s/keys :req-un [::playerInTurn ::players ::board]))

(s/valid? ::game {:playerInTurn :large
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
                          {:row 1, :column 3, :piece {:type :pawn, :owner :small :valid-moves #{[5 0]}}}
                          {:row 1, :column 4, :piece {:type :pawn, :owner :small :valid-moves #{}}}
                          {:row 1, :column 5, :piece {:type :pawn, :owner :small :valid-moves #{}}}
                          {:row 1, :column 6, :piece {:type :pawn, :owner :small :valid-moves #{}}}
                          {:row 1, :column 7, :piece {:type :pawn, :owner :small :valid-moves #{}}}
                          {:row 2, :column 0, :piece nil}
                          {:row 2, :column 1, :piece nil}
                          {:row 2, :column 2, :piece nil}]})

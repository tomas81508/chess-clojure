(ns chess.spec
  (:require [clojure.spec :as s]))

(s/def ::coordinate (s/and int?
                           (fn [x] (<= 0 x 7))))

(s/def ::row ::coordinate)

(s/def ::column ::coordinate)

(s/def ::id (s/or :keyword keyword?
                  :string string?))

(s/def ::playerInTurn ::id)

(s/def ::player (s/keys :req-un [::id]))

(s/def ::players (s/and (s/+ ::player)
                        (fn [x] (= (count x) 2))))

(s/def ::type #{:rook :knight :bishop :pawn :queen :king})

(s/def ::owner ::id)

(s/def ::coordinates (s/and (s/+ ::coordinate)
                            (fn [x] (= (count x) 2))))

(s/def ::valid-moves (s/and set?
                            (s/* ::coordinates)))

(s/def ::piece (s/or :something (s/keys :req-un [::type ::owner ::valid-moves ::id])
                     :nothing nil?))


(s/def ::cell (s/keys :req-un [::row ::column ::piece]))

(s/def ::board (s/+ ::cell))

(s/def ::game (s/keys :req-un [::playerInTurn ::players ::board]))

(s/explain ::game {:playerInTurn :large
                   :players      [{:id :large} {:id :small}]
                   :board        [{:row 0, :column 0, :piece {:type :rook, :owner :small :valid-moves #{} :id "1"}}
                                  {:row 1, :column 3, :piece {:type :pawn, :owner :small :valid-moves #{[5 0]} :id "2"}}
                                  {:row 2, :column 0, :piece nil}
                                  {:row 2, :column 1, :piece nil}
                                  {:row 2, :column 2, :piece nil}]})

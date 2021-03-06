(ns chess.spec
  (:require [clojure.spec :as s]))

(s/def ::coordinate (s/and int?
                           (fn [x] (<= 0 x 7))))

(s/def ::id (s/or :keyword keyword?
                  :string string?))

(s/def ::playerInTurn ::id)

(s/def ::player (s/keys :req-un [::id]))

(s/def ::players (s/and (s/+ ::player)
                        (fn [x] (= (count x) 2))))

(s/def :chess.piece/type #{:rook :knight :bishop :pawn :queen :king})

(s/def ::owner ::id)

(s/def ::coordinates (s/and (s/+ ::coordinate)
                            (fn [x] (= (count x) 2))))

(s/def ::valid-moves (s/and set?
                            (s/* ::coordinates)))

(s/def ::piece (s/or :something (s/keys :req-un [:chess.piece/type ::owner ::valid-moves ::id])
                     :nothing nil?))


(s/def ::cell (s/keys :req-un [::coordinates ::piece]))

(s/def ::board (s/+ ::cell))

(s/def ::piece-type :chess.piece/type)

(s/def ::from-coordinates ::coordinates)

(s/def ::to-coordinates ::coordinates)

(s/def ::move (s/keys :req-un [::piece-type ::from-coordinates ::to-coordinates ::owner]))

(s/def ::previous-moves (s/* ::move))

(s/def ::game (s/keys :req-un [::playerInTurn ::players ::board ::previous-moves]))
(ns chess.state
  "A model of the state and functions for update and lookup."
  (:use [clojure.test :only (is run-tests function?)]
        [clojure.repl :only (doc)]
        [clojure.string :only [lower-case]]
        [clojure.pprint :only [pprint]]
        [test.core :only [is=]]))


(defn
  ^{:doc  "Creates a board from the given strings. Small letters will be for player 1."
    :test (fn []
            (is= (strings->board ".QR."
                                 "bhrp"
                                 ".qk.")
                 {[0 0] nil
                  [0 1] {:piece :queen :owner "p2"}
                  [0 2] {:piece :rook :owner "p2"}
                  [0 3] nil
                  [1 0] {:piece :bishop :owner "p1"}
                  [1 1] {:piece :knight :owner "p1"}
                  [1 2] {:piece :rook :owner "p1"}
                  [1 3] {:piece :pawn :owner "p1"}
                  [2 0] nil
                  [2 1] {:piece :queen :owner "p1"}
                  [2 2] {:piece :king :owner "p1"}
                  [2 3] nil}))}
  strings->board [& strings]
  (let [letter->value (fn [letter]
                        (if (= letter ".")
                          nil
                          {:piece (condp = (lower-case letter)
                                    "b" :bishop
                                    "h" :knight ; h from horse
                                    "k" :king
                                    "p" :pawn
                                    "q" :queen
                                    "r" :rook)
                          :owner (if (= letter (lower-case letter))
                                   "p1"
                                   "p2")}))]
    (->> (map-indexed (fn [row-index string]
                       (map-indexed (fn [column-index letter]
                                      {:square [row-index column-index]
                                       :value (letter->value (str letter))})
                                    string))
                     strings)
         (flatten)
         (reduce (fn [a v]
                   (assoc a (:square v) (:value v)))
                 {}))))

(defn
  ^{:doc  "Gets the piece at the given position or nil if none is present."
    :test (fn []
            (is= (-> (strings->board "..K")
                     (get-piece-by-coordinate [0 0]))
                 nil)
            (is= (-> (strings->board "..K")
                     (get-piece-by-coordinate [0 2]))
                 {:piece :king :owner "p2"}))}
  get-piece [board position]
  (get board position))

(defn
  ^{:doc  "Mark a player and a piece on the board."
    :test (fn []
            (is= (mark (strings->board "..K..k")
                       {:piece :rook :owner "p1"}
                       [0 1])
                 (strings->board ".rK..k")))}
  mark [board piece position]
  (assoc board position piece))

(defn
  ^{:doc  "Mark a player and a piece on the board."
    :test (fn []
            (is= (unmark (strings->board ".pK..k") [0 1])
                 (strings->board "..K..k")))}
  unmark [board position]
  (assoc board position nil))

(defn
  ^{:test (fn []
            (is= (-> (strings->board "K.q.")
                     (move [0 0] [0 3]))
                 (strings->board "..qK")))}
  move [board from-position to-position]
  (let [piece (get-piece board from-position)]
    (-> board
        (unmark from-position)
        (mark piece to-position))))
(ns chess.core
  "A collection of pure functions for the game Chess."
  (:use [clojure.test :only (deftest is run-tests function?)]
        [clojure.repl :only (doc)]
        [clojure.pprint :only [pprint]]
        [test.core :only [is= is-not error?]])
  (:require [chess.state :as s]
            [chess.interop :as i]))

(defn create-classic-game-state []
  (s/create-state "rnbqkbnr"
                  "pppppppp"
                  "........"
                  "........"
                  "........"
                  "........"
                  "PPPPPPPP"
                  "RNBQKBNR"))


(defn
  ^{:doc  "Checks if x is part of the collection."
    :test (fn []
            (is= (seq-contains? [1 3 5 7 9] 3) true)
            (is= (seq-contains? [1 3 5 7 9] 2) false))}
  seq-contains? [coll x]
  (let [get-first (comp (filter (fn [y] (= x y)))
                        (take 1))]
    (not (empty? (sequence get-first coll)))))


(defn
  ^{:doc  "Determines all potential moves for a knight."
    :test (fn []
            (is= (get-potential-knight-moves (s/create-state "....."
                                                             ".n..."
                                                             "....."
                                                             "Q.p..")
                                             [1 1])
                 #{[0 3] [2 3] [3 0]}))}
  get-potential-knight-moves [state from-position]
  {:pre [(s/knight? (s/get-piece state from-position))]}
  (->> [[-2 -1] [-2 1] [2 -1] [2 1] [-1 2] [1 2] [-1 -2] [1 -2]]
       (map (fn [p]
              (map + from-position p)))
       (filter (fn [p]
                 (and (s/on-board? state p)
                      (or (nil? (s/get-piece state p))
                          (not= (s/get-owner (s/get-piece state from-position))
                                (s/get-owner (s/get-piece state p)))))))
       (into #{})))


(defn-
  ^{:doc  "Returns a set of potentials move in the given direction at most of the distance steps from the given position."
    :test (fn []
            ; Movement ending by out of board
            (is= (get-potential-moves-in-directions (s/create-state "q..") [0 0] [[0 1]])
                 #{[0 1] [0 2]})
            ; Movement ending on enemy player's piece
            (is= (get-potential-moves-in-directions (s/create-state "q.K.") [0 0] [[0 1]])
                 #{[0 1] [0 2]})
            ; Movement ending on own player's piece
            (is= (get-potential-moves-in-directions (s/create-state "q..k.") [0 0] [[0 1]])
                 #{[0 1] [0 2]})
            ; Assuming that the queen only could move two steps
            (is= (get-potential-moves-in-directions (s/create-state "q...k.") [0 0] [[0 1]] 2)
                 #{[0 1] [0 2]})
            ; Assuming that the queen only could move two steps
            (is= (get-potential-moves-in-directions (s/create-state "q..Pk.") [0 0] [[0 1]] 2)
                 #{[0 1] [0 2]}))}
  get-potential-moves-in-directions
  ([state from-position directions]
   (get-potential-moves-in-directions state from-position directions :unlimited))
  ([state from-position directions steps]
   (into #{} (reduce (fn [valid-moves direction]
                       (concat valid-moves
                               (loop [valid-moves-in-this-direction []
                                      length 1]
                                 (if (and (not= steps :unlimited)
                                          (> length steps))
                                   valid-moves-in-this-direction
                                   (let [test-position (map + from-position (map (fn [v] (* length v)) direction))]
                                     (cond
                                       ; If not on board answer with the list that we currenly have
                                       (not (s/on-board? state test-position)) valid-moves-in-this-direction

                                       ; If we hit our own piece ...
                                       (= (s/get-owner state test-position) (s/get-owner state from-position))
                                       valid-moves-in-this-direction

                                       ; If marked by another player
                                       (s/marked? state test-position)
                                       (conj valid-moves-in-this-direction test-position)

                                       :else
                                       (recur (conj valid-moves-in-this-direction test-position)
                                              (inc length))))))))
                     []
                     directions))))


(defn
  ^{:doc  "Determines all potential moves for a queen."
    :test (fn []
            (is= (get-potential-queen-moves (s/create-state "....."
                                                            ".q.k."
                                                            "..B.."
                                                            ".....")
                                            [1 1])
                 #{[0 0] [0 1] [0 2]
                   [1 0] [1 2]
                   [2 0] [2 1] [2 2]
                   [3 1]}))}
  get-potential-queen-moves [state from-position]
  {:pre [(s/queen? (s/get-piece state from-position))]}
  (get-potential-moves-in-directions state
                                     from-position
                                     [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]))

(defn
  ^{:doc  "Determines all potential moves for a rook."
    :test (fn []
            (is= (get-potential-rook-moves (s/create-state "....."
                                                           ".r.k."
                                                           "..B.."
                                                           ".....")
                                           [1 1])
                 #{[0 1] [1 0] [1 2] [2 1] [3 1]}))}
  get-potential-rook-moves [state from-position]
  {:pre [(s/rook? (s/get-piece state from-position))]}
  (get-potential-moves-in-directions state
                                     from-position
                                     [[0 -1] [-1 0] [1 0] [0 1]]))


(defn
  ^{:doc  "Determines all potential moves for a bishop."
    :test (fn []
            (is= (get-potential-bishop-moves (s/create-state "....."
                                                             ".b.k."
                                                             "B...."
                                                             ".....")
                                             [1 1])
                 #{[0 0] [0 2] [2 0] [2 2] [3 3]}))}
  get-potential-bishop-moves [state from-position]
  {:pre [(s/bishop? (s/get-piece state from-position))]}
  (get-potential-moves-in-directions state
                                     from-position
                                     [[1 1] [1 -1] [-1 1] [-1 -1]]))


(defn
  ^{:doc  "Determines all potential moves for a pawn."
    :test (fn []
            (is= (get-potential-pawn-moves (s/create-state "..."
                                                           "..."
                                                           ".P."
                                                           "...")
                                           [2 1])
                 #{[1 1]})
            (is= (get-potential-pawn-moves (s/create-state "..."
                                                           ".q."
                                                           ".P."
                                                           "...")
                                           [2 1])
                 #{})
            (is= (get-potential-pawn-moves (s/create-state "..."
                                                           "qk."
                                                           ".P."
                                                           "...")
                                           [2 1])
                 #{[1 0]})
            (is= (get-potential-pawn-moves (s/create-state "k"
                                                           "P")
                                           [1 0])
                 #{}))}
  get-potential-pawn-moves [state from-position]
  {:pre [(s/pawn? (s/get-piece state from-position))]}
  (let [pawn (s/get-piece state from-position)
        test-forward-position (map + from-position (s/get-direction state (:owner pawn)))]
    (-> (if (s/marked? state test-forward-position)
          #{}
          #{test-forward-position})
        ((fn [positions]
           (let [test-positions (map (fn [d] (map + test-forward-position d)) [[0 1] [0 -1]])]
             (reduce (fn [positions test-position]
                       (if (and (s/marked? state test-position)
                                (not= (:owner pawn)
                                      (s/get-owner state test-position)))
                         (conj positions test-position)
                         positions))
                     positions
                     test-positions)))))))

(defn
  ^{:doc  "Determines all potential moves for a pawn."
    :test (fn []
            (is= (get-potential-king-moves (s/create-state "p..."
                                                           "pk.."
                                                           ".qQ.")
                                           [1 1])
                 #{[0 1] [0 2] [1 2] [2 0] [2 2]}))}
  get-potential-king-moves [state from-position]
  {:pre [(s/king? (s/get-piece state from-position))]}
  (let [directions [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]]
    (get-potential-moves-in-directions state from-position directions 1)))


(defmulti get-potential-moves
          ^{:doc "Returns all potentials moves for the piece at the given position"}
          (fn [state from-position]
            (:type (s/get-piece state from-position))))

(defmethod get-potential-moves :bishop [state from-position]
  (get-potential-bishop-moves state from-position))

(defmethod get-potential-moves :king [state from-position]
  (get-potential-king-moves state from-position))

(defmethod get-potential-moves :knight [state from-position]
  (get-potential-knight-moves state from-position))

(defmethod get-potential-moves :queen [state from-position]
  (get-potential-queen-moves state from-position))

(defmethod get-potential-moves :pawn [state from-position]
  (get-potential-pawn-moves state from-position))

(defmethod get-potential-moves :rook [state from-position]
  (get-potential-rook-moves state from-position))


(defn
  ^{:doc  "Checks if the given player is in check."
    :test (fn []
            (is (in-check? (s/create-state "R..k") :small))
            (is-not (in-check? (s/create-state "R.qk") :small)))}
  in-check? [state player-id]
  (let [enemy-positions (s/get-position-of-pieces state (s/get-opponent-player-id state player-id))
        king-position (s/get-king-position state player-id)]
    (some (fn [p]
            (seq-contains? (get-potential-moves state p) king-position))
          enemy-positions)))

(defn
  ^{:test (fn []
            (is= (get-valid-moves (s/create-state "KR.q"
                                                  "....")
                                  [0 1])
                 #{[0 2] [0 3]})
            (is= (get-valid-moves (s/create-state "K..q"
                                                  ".R..")
                                  [1 1])
                 #{[0 1]}))}
  get-valid-moves [state from-position]
  (let [moving-player (s/get-owner state from-position)]
    (->> (get-potential-moves state from-position)
         (filter (fn [p]
                   (let [potential-state (s/update-position state from-position p)]
                     (not (in-check? potential-state moving-player)))))
         (into #{}))))


(defn
  ^{:doc  "Determines if the given move is valid."
    :test (fn []
            (is (-> (s/create-state "n.."
                                    "..."
                                    "...")
                    (valid-move? [0 0] [1 2])))
            (is-not (-> (s/create-state "n.."
                                        "..."
                                        "...")
                        (valid-move? [0 0] [1 1]))))}
  valid-move? [state from-position to-position]
  {:pre [(s/marked? state from-position)]}
  (contains? (get-valid-moves state from-position) to-position))




(defn
  ^{:doc  "Makes a move for the given player."
    :test (fn []
            (is= (-> (s/create-state "R..")
                     (move :large [0 0] [0 2])
                     (s/get-board))
                 (s/create-board "..R"))
            (error? (-> (s/create-state "K..k")
                        (move :small [0 3] [0 2]))))}
  move [state player-id from-position to-position]
  (when (not= (:player-in-turn state) player-id)
    (i/error "The player " player-id " is not in turn."))
  (when-not (valid-move? state from-position to-position)
    (i/error "The move is not valid."))
  (-> state
      (s/update-position from-position to-position)
      (s/update-player-in-turn)))


(defn
  ^{:doc  "..."
    :test (fn []
            (let [state (-> (s/create-state "R...K..R")
                            (castle [0 4] [0 2]))
                  king-piece (s/get-piece state [0 2])
                  rook-piece (s/get-piece state [0 3])]
              (is= (:type king-piece) :king)
              (is (:moved? king-piece))
              (is= (:type rook-piece) :rook)
              (is (:moved? rook-piece))))}
  castle [state from-position to-posisiton]
  )





(deftest a-simple-game
  (let [state (-> (s/create-state "rkqr"
                                  "pppp"
                                  "...."
                                  "PPPP"
                                  "RKQR")
                  (move :large [3 0] [2 0])
                  (move :small [1 1] [2 0])
                  (move :large [4 0] [2 0]))]
    (is= (s/get-board state)
         (s/create-board "rkqr"
                         "p.pp"
                         "R..."
                         ".PPP"
                         ".KQR"))))

(deftest another-simple-game
  (let [state-atom (atom (s/create-state "rkqr"
                                         "pppp"
                                         "...."
                                         "PPPP"
                                         "RKQR"))]
    (swap! state-atom move :large [3 0] [2 0])
    (is= (s/get-board @state-atom) (s/create-board "rkqr"
                                                   "pppp"
                                                   "P..."
                                                   ".PPP"
                                                   "RKQR"))
    (swap! state-atom move :small [1 1] [2 0])
    (is= (s/get-board @state-atom) (s/create-board "rkqr"
                                                   "p.pp"
                                                   "p..."
                                                   ".PPP"
                                                   "RKQR"))
    (swap! state-atom move :large [4 0] [2 0])
    (is= (s/get-board @state-atom) (s/create-board "rkqr"
                                                   "p.pp"
                                                   "R..."
                                                   ".PPP"
                                                   ".KQR"))))


















































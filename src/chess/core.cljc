(ns chess.core
  "A collection of pure functions for the game Chess."
  (:use [clojure.test :only (deftest is run-tests function?)]
        [clojure.repl :only (doc)]
        [clojure.pprint :only [pprint]]
        [test.core :only [is= is-not error?]]
        [chess.Fisher])
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
  {pre [(s/queen? (s/get-piece state from-position))]}
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
            ; The first move can be two steps.
            (is= (get-potential-pawn-moves (s/create-state "..."
                                                           "..."
                                                           "..."
                                                           ".P."
                                                           "...")
                                           [3 1])
                 #{[1 1] [2 1]})
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
        forward-position (map + from-position (s/get-direction state (:owner pawn)))
        double-forward-position (map + forward-position (s/get-direction state (:owner pawn)))]
    (-> (cond
          (s/marked? state forward-position)
          #{}

          (and (not (s/marked? state double-forward-position))
               (not (:moved? pawn)))
          #{forward-position double-forward-position}

          :else
          #{forward-position})
        ((fn [positions]
           (let [test-positions (map (fn [d] (map + forward-position d)) [[0 1] [0 -1]])]
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
  ^{:test (fn []
            (is (player-in-turn? (s/create-state "p..P") :large))
            (is-not (player-in-turn? (s/create-state "p..P") :small)))}
  player-in-turn? [state player-id]
  (= (s/player-in-turn state) player-id))

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
                 #{[0 1]})
            ; Pieces of players that are not in turn should not be able to move.
            (is= (get-valid-moves (s/create-state "K..k")
                                  [0 3])
                 #{}))}
  get-valid-moves [state from-position]
  (let [moving-player (s/get-owner state from-position)]
    (if (not (player-in-turn? state moving-player))
      #{}
      (->> (get-potential-moves state from-position)
           (filter (fn [p]
                     (let [potential-state (s/update-position state from-position p)]
                       (not (in-check? potential-state moving-player)))))
           (into #{})))))


(defn
  ^{:doc  "Determines if the given move is valid."
    :test (fn []
            (is (-> (s/create-state "N.."
                                    "..."
                                    "...")
                    (valid-move? [0 0] [1 2])))
            (is-not (-> (s/create-state "N.."
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
                     (s/get-board)
                     (s/board->string))
                 "..R")
            ; Moved piece should be marked as moved
            (is (-> (s/create-state "R..")
                    (move :large [0 0] [0 2])
                    (s/get-piece [0 2])
                    (:moved?)))
            (error? (-> (s/create-state "K..k")
                        (move :small [0 3] [0 2]))))}
  move [state player-id from-position to-position]
  (when-not (player-in-turn? state player-id)
    (i/error "The player " player-id " is not in turn."))
  (when-not (valid-move? state from-position to-position)
    (i/error "The move is not valid."))
  (-> state
      (s/update-position from-position to-position)
      (s/mark-piece-as-moved to-position)
      (s/update-player-in-turn)))


(defn
  ^{:doc  "Determines if a castle is valid."
    :test (fn []
            (is (-> (s/create-state "R...K...")
                    (valid-castle? [0 4] [0 2])))
            (is-not (-> (s/create-state "R...K...")
                        (s/mark-piece-as-moved [0 0])
                        (valid-castle? [0 4] [0 2]))))}
  valid-castle? [state king-position king-to-position]
  {:pre [(s/king? state king-position)]}
  (let [direction (if (pos? (second (map - king-position king-to-position)))
                    [0 -1]
                    [0 1])
        rook-position (if (= direction [0 -1])
                        [(first king-position) 0]
                        [(first king-position) 7])]
    (and (not (in-check? state (:player-in-turn state)))
         (not (:moved? (s/get-piece state king-position)))
         (not (:moved? (s/get-piece state rook-position)))
         (reduce (fn [valid? p]
                   (and valid?
                        (not (s/marked? state p))
                        (not (in-check? (s/update-position state king-position p)
                                        (:player-in-turn state)))))
                 true
                 [(map + king-position direction)
                  (map + king-position direction direction)])
         (or (= direction [0 1])
             (not (s/marked? state [(first king-position) 1]))))))


(defn
  ^{:doc  "..."
    :test (fn []
            (let [state (-> (s/create-state "R...K..R")
                            (castle :large [0 4] [0 2]))
                  king-piece (s/get-piece state [0 2])
                  rook-piece (s/get-piece state [0 3])]
              (is= (:type king-piece) :king)
              (is (:moved? king-piece))
              (is= (:type rook-piece) :rook)
              (is (:moved? rook-piece))))}
  castle [state player-id king-position king-to-position]
  (when (not= (s/player-in-turn state) player-id)
    (i/error "The player " player-id " is not in turn."))
  (when-not (valid-castle? state king-position king-to-position)
    (i/error "The move is not valid."))
  (let [direction (if (pos? (second (map - king-position king-to-position)))
                    [0 -1]
                    [0 1])
        rook-position (if (= direction [0 -1])
                        [(first king-position) 0]
                        [(first king-position) 7])
        rook-to-position (if (= direction [0 -1])
                           [(first king-position) 3]
                           [(first king-position) 5])]
    (-> state
        (s/update-position king-position king-to-position)
        (s/mark-piece-as-moved king-to-position)
        (s/update-position rook-position rook-to-position)
        (s/mark-piece-as-moved rook-to-position)
        (s/update-player-in-turn))))


















(defn
  ^{:test (fn []
            (is= (get-pieces-by (s/create-state ".Kq.pp.") {:player-id :small :type :pawn})
                 {[0 4] {:type :pawn :owner :small :moved? false}
                  [0 5] {:type :pawn :owner :small :moved? false}}))}
  get-pieces-by [state {player-id :player-id type :type}]
  (reduce (fn [map key]
            (if (or (nil? (get map key))
                    (let [piece (get map key)]
                      (not (and (= player-id (:owner piece))
                                (= type (:type piece))))))
              (dissoc map key)
              map))
          (s/get-board state)
          (keys (s/get-board state))))


(defn-
  ^{:test (fn []
            (is= (letter-column->int "a") 0)
            (is= (letter-column->int "c") 2)
            (is= (letter-column->int \c) 2))}
  letter-column->int [letter-row]
  (let [char (if (string? letter-row) (first (seq letter-row)) letter-row)]
    (- (int char) 97)))

(defn
  ^{:doc  "This notation ..."
    :test (fn []
            (is= (algebraic-notation->coordinates "a8") [0 0])
            (is= (algebraic-notation->coordinates "h1") [7 7])
            (is= (algebraic-notation->coordinates "e4") [4 4])
            (is= (algebraic-notation->coordinates "Nf6") [2 5])
            (is= (algebraic-notation->coordinates "Rfe8") [0 4]))} ; moving a Rook to e8 from the column f

  algebraic-notation->coordinates [algebraic-move]
  (let [position (re-find #"[a-h][1-8]" algebraic-move)]
    [(- 7 (- (int (second position)) 49)) (letter-column->int (first position))]))


(defn
  ^{:doc  ""
    :test (fn []
            (is= (an-algebraic-notation->move-data (create-classic-game-state) "e4")
                 {:type :move :from-position [6 4] :to-position [4 4]})
            (is= (an-algebraic-notation->move-data (-> (create-classic-game-state)
                                                       (move :large [6 4] [4 4]))
                                                   "Nf6")
                 {:type :move :from-position [0 6] :to-position [2 5]})
            (is= (an-algebraic-notation->move-data (s/create-state ".." ".." ".." ".." ".." ".." "R." ".R")
                                                   "Raa1")
                 {:type :move :from-position [6 0] :to-position [7 0]})
            (is= (an-algebraic-notation->move-data (s/create-state ".." ".." ".." ".." ".." ".." "R." ".R")
                                                   "R1a1")
                 {:type :move :from-position [7 1] :to-position [7 0]})
            (is= (an-algebraic-notation->move-data (s/create-state "R...K...") "O-O")
                 {:type :castle :from-position [0 4] :to-position [0 6]})
            (is= (an-algebraic-notation->move-data (s/create-state "R...K...") "O-O-O")
                 {:type :castle :from-position [0 4] :to-position [0 2]}))}
  an-algebraic-notation->move-data [state algebraic-notation]
  (let [player-id (:player-in-turn state)
        ; row for castle
        row (if (= (s/get-direction state player-id) [1 0]) 7 0)]
    (cond (= algebraic-notation "O-O-O")
          {:type :castle :from-position [row 4] :to-position [row 2]}

          (= algebraic-notation "O-O")
          {:type :castle :from-position [row 4] :to-position [row 6]}

          :else
          (let [piece-type (condp = (str (first algebraic-notation))
                             "B" :bishop
                             "K" :king
                             "N" :knight
                             "Q" :queen
                             "R" :rook
                             :pawn)
                to-position (algebraic-notation->coordinates algebraic-notation)
                pieces (get-pieces-by state {:type      piece-type
                                             :player-id (:player-in-turn state)})
                from-positions (->> (keys pieces)
                                    (filter (fn [p]
                                              (contains? (get-valid-moves state p) to-position))))
                from-position (if (= (count from-positions) 1)
                                (first from-positions)
                                (let [row-or-column (re-find #"[a-z]|[0-9]" algebraic-notation)]
                                  (cond (re-find #"[1-8]" row-or-column)
                                        (let [row (- 7 (dec (read-string row-or-column)))]
                                          (->> from-positions
                                               (filter (fn [p] (= (first p) row)))
                                               (first)))

                                        (re-find #"[a-h]" row-or-column)
                                        (let [column (letter-column->int row-or-column)]
                                          (->> from-positions
                                               (filter (fn [p] (= (second p) column)))
                                               (first)))

                                        :else
                                        (println "This does not match:" row-or-column))))]
            {:type :move :from-position from-position :to-position to-position}))))


(defn
  ^{:test (fn []
            (is= (algebraic-notation->move-data ["e4" "Nf6"])
                 [{:type :move :from-position [6 4] :to-position [4 4]}
                  {:type :move :from-position [0 6] :to-position [2 5]}])
            (is= (algebraic-notation->move-data (take 15 (get-game-moves (get-Ficher-Spassky-game))))
                 [{:type :move :from-position [6 4] :to-position [4 4]}
                  {:type :move :from-position [0 6] :to-position [2 5]}
                  {:type :move :from-position [4 4] :to-position [3 4]}
                  {:type :move :from-position [2 5] :to-position [3 3]}
                  {:type :move :from-position [6 3] :to-position [4 3]}
                  {:type :move :from-position [1 3] :to-position [2 3]}
                  {:type :move :from-position [7 6] :to-position [5 5]}
                  {:type :move :from-position [1 6] :to-position [2 6]}
                  {:type :move :from-position [7 5] :to-position [4 2]}
                  {:type :move :from-position [3 3] :to-position [2 1]}
                  {:type :move :from-position [4 2] :to-position [5 1]}
                  {:type :move :from-position [0 5] :to-position [1 6]}
                  {:type :move :from-position [7 1] :to-position [6 3]}
                  {:type :castle :from-position [7 4] :to-position [7 6]}
                  {:type :move :from-position [6 7] :to-position [5 7]}]))}
  algebraic-notation->move-data [algebraic-moves]
  (second
    (let [state (create-classic-game-state)]
      (reduce (fn [[state moves] algebraic-move]
                ;(println "----------------------")
                ;(println algebraic-move)
                ;(println (s/board->string (s/get-board state)))
                (let [move-data (an-algebraic-notation->move-data state algebraic-move)]
                  [(condp = (:type move-data)
                     :move
                     (move state (:player-in-turn state) (:from-position move-data) (:to-position move-data))
                     :castle
                     (castle state (:player-in-turn state) (:from-position move-data) (:to-position move-data)))
                   (conj moves move-data)]))
              [state []]
              algebraic-moves))))


(deftest Fisher-Spassky-game
  (is
    (let [state (create-classic-game-state)]
      (reduce (fn [state move-data]
                (cond (= (:type move-data) :move)
                      (move state (:player-in-turn state) (:from-position move-data) (:to-position move-data))

                      (= (:type move-data) :castle)
                      (castle state (:player-in-turn state) (:from-position move-data) (:to-position move-data))))
              state
              (algebraic-notation->move-data (take 148 (get-game-moves (get-Ficher-Spassky-game))))))))

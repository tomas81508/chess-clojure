(ns chess.core
  "A collection of pure functions for the game Chess."
  (:use [clojure.test :only (deftest is run-tests function?)]
        [clojure.repl :only (doc)]
        [clojure.pprint :only [pprint]]
        [test.core :only [is= is-not]])
  (:require [chess.state :as s]
            [chess.interop :as i]))


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
  ^{:doc  "Determines all possible moves for a knight."
    :test (fn []
            (is= (get-valid-knight-moves (s/create-state "....."
                                                         ".n..."
                                                         "....."
                                                         "Q.k..")
                                         [1 1])
                 #{[0 3] [2 3] [3 0]}))}
  get-valid-knight-moves [state from-position]
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
  ^{:test (fn []
            (is= (get-valid-moves-in-directions (s/create-state "q..") [0 0] [[0 1]])
                 #{[0 1] [0 2]})
            (is= (get-valid-moves-in-directions (s/create-state "q.K.") [0 0] [[0 1]])
                 #{[0 1] [0 2]})
            (is= (get-valid-moves-in-directions (s/create-state "q..k.") [0 0] [[0 1]])
                 #{[0 1] [0 2]}))}
  get-valid-moves-in-directions [state from-position directions]
  (into #{} (reduce (fn [valid-moves direction]
                      (concat valid-moves
                              (loop [valid-moves-in-this-direction []
                                     length 1]
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
                                           (inc length)))))))
                    []
                    directions)))


(defn
  ^{:doc  "Determines all possible moves for a queen."
    :test (fn []
            (is= (get-valid-queen-moves (s/create-state "....."
                                                        ".q.k."
                                                        "..B.."
                                                        ".....")
                                        [1 1])
                 #{[0 0] [0 1] [0 2]
                   [1 0] [1 2]
                   [2 0] [2 1] [2 2]
                   [3 1]}))}
  get-valid-queen-moves [state from-position]
  {:pre [(s/queen? (s/get-piece state from-position))]}
  (get-valid-moves-in-directions state
                                 from-position
                                 [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]))

(defn
  ^{:doc  "Determines all possible moves for a rook."
    :test (fn []
            (is= (get-valid-rook-moves (s/create-state "....."
                                                       ".r.k."
                                                       "..B.."
                                                       ".....")
                                       [1 1])
                 #{[0 1] [1 0] [1 2] [2 1] [3 1]}))}
  get-valid-rook-moves [state from-position]
  {:pre [(s/rook? (s/get-piece state from-position))]}
  (get-valid-moves-in-directions state
                                 from-position
                                 [[0 -1] [-1 0] [1 0] [0 1]]))


(defn
  ^{:doc  "Determines all possible moves for a bishop."
    :test (fn []
            (is= (get-valid-bishop-moves (s/create-state "....."
                                                         ".b.k."
                                                         "B...."
                                                         ".....")
                                         [1 1])
                 #{[0 0] [0 2] [2 0] [2 2] [3 3]}))}
  get-valid-bishop-moves [state from-position]
  {:pre [(s/bishop? (s/get-piece state from-position))]}
  (get-valid-moves-in-directions state
                                 from-position
                                 [[1 1] [1 -1] [-1 1] [-1 -1]]))


(defn
  ^{:doc  "Determines all possible moves for a pawn."
    :test (fn []
            (is= (get-valid-pawn-moves (s/create-state "..."
                                                       "..."
                                                       ".P."
                                                       "...")
                                       [2 1])
                 #{[1 1]})
            (is= (get-valid-pawn-moves (s/create-state "..."
                                                       ".q."
                                                       ".P."
                                                       "...")
                                       [2 1])
                 #{})
            (is= (get-valid-pawn-moves (s/create-state "..."
                                                       "qk."
                                                       ".P."
                                                       "...")
                                       [2 1])
                 #{[1 0]})
            (is= (get-valid-pawn-moves (s/create-state "k"
                                                       "P")
                                       [1 0])
                 #{}))}
  get-valid-pawn-moves [state from-position]
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


(defmulti get-valid-moves
          ^{:doc "Returns all valid moves for the piece at the given position"}
          (fn [state from-position]
            (:type (s/get-piece state from-position))))

(defmethod get-valid-moves :bishop [state from-position]
  (get-valid-bishop-moves state from-position))

(defmethod get-valid-moves :knight [state from-position]
  (get-valid-knight-moves state from-position))

(defmethod get-valid-moves :queen [state from-position]
  (get-valid-queen-moves state from-position))

(defmethod get-valid-moves :pawn [state from-position]
  (get-valid-pawn-moves state from-position))

(defmethod get-valid-moves :rook [state from-position]
  (get-valid-rook-moves state from-position))


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
            (is= (-> (move (s/create-state "R..") :white [0 0] [0 2])
                     (s/get-board))
                 (s/create-board "..R")))}
  move [state player-id from-position to-position]
  (when-not (valid-move? state from-position to-position)
    (i/error "The move is not valid."))
  (s/update-position state from-position to-position))







(deftest a-game
  (let [state (-> (s/create-state "rr"
                                  ".."
                                  ".."
                                  "RR")
                  (move :white [3 0] [0 0])
                  (move :black [0 1] [0 0])
                  (move :white [3 1] [0 1])
                  (move :black [0 0] [0 1]))]
    (is= (s/get-board state)
         (s/create-board ".r"
                         ".."
                         ".."
                         ".."))))


















































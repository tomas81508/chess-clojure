(ns chess.core
  "A collection of pure functions for the game Chess."
  (:use [clojure.test :only (is run-tests function?)]
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
            (is= (get-valid-knight-moves (s/create-board "....."
                                                         ".n..."
                                                         "....."
                                                         "Q.k..")
                                         [1 1])
                 #{[0 3] [2 3] [3 0]}))}
  get-valid-knight-moves [board from-position]
  {:pre [(s/knight? (s/get-piece board from-position))]}
  (->> [[-2 -1] [-2 1] [2 -1] [2 1] [-1 2] [1 2] [-1 -2] [1 -2]]
       (map (fn [p]
              (map + from-position p)))
       (filter (fn [p]
                 (and (s/on-board? board p)
                      (or (nil? (s/get-piece board p))
                          (not= (s/get-owner (s/get-piece board from-position))
                                (s/get-owner (s/get-piece board p)))))))
       (into #{})))

(defn
  ^{:test (fn []
            (is (-> (s/create-board "n.."
                                    "..."
                                    "...")
                    (valid-knight-move? [0 0] [1 2])))
            (is-not (-> (s/create-board "n.."
                                        "..."
                                        "...")
                        (valid-knight-move? [0 0] [1 1]))))}
  valid-knight-move? [board from-position to-position]
  {:pre [(s/knight? (s/get-piece board from-position))]}
  (contains? (get-valid-knight-moves board from-position) to-position))


(defn-
  ^{:test (fn []
            (is= (get-valid-moves-in-directions (s/create-board "q..") [0 0] [[0 1]])
                 #{[0 1] [0 2]})
            (is= (get-valid-moves-in-directions (s/create-board "q.K.") [0 0] [[0 1]])
                 #{[0 1] [0 2]})
            (is= (get-valid-moves-in-directions (s/create-board "q..k.") [0 0] [[0 1]])
                 #{[0 1] [0 2]}))}
  get-valid-moves-in-directions [board from-position directions]
  (into #{} (reduce (fn [valid-moves direction]
                      (concat valid-moves
                              (loop [valid-moves-in-this-direction []
                                     length 1]
                                (let [test-position (map + from-position (map (fn [v] (* length v)) direction))]
                                  (cond
                                    ; If not on board answer with the list that we currenly have
                                    (not (s/on-board? board test-position)) valid-moves-in-this-direction

                                    ; If we hit our own piece ...
                                    (= (s/get-owner board test-position) (s/get-owner board from-position))
                                    valid-moves-in-this-direction

                                    ; If marked by another player
                                    (s/marked? board test-position)
                                    (conj valid-moves-in-this-direction test-position)

                                    :else
                                    (recur (conj valid-moves-in-this-direction test-position)
                                           (inc length)))))))
                    []
                    directions)))


(defn
  ^{:doc  "Determines all possible moves for a queen."
    :test (fn []
            (is= (get-valid-queen-moves (s/create-board "....."
                                                        ".q.k."
                                                        "..B.."
                                                        ".....")
                                        [1 1])
                 #{[0 0] [0 1] [0 2]
                   [1 0] [1 2]
                   [2 0] [2 1] [2 2]
                   [3 1]}))}
  get-valid-queen-moves [board from-position]
  {:pre [(s/queen? (s/get-piece board from-position))]}
  (get-valid-moves-in-directions board
                                 from-position
                                 [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]))

(defn
  ^{:doc  "Determines all possible moves for a rook."
    :test (fn []
            (is= (get-valid-rook-moves (s/create-board "....."
                                                       ".r.k."
                                                       "..B.."
                                                       ".....")
                                       [1 1])
                 #{[0 1] [1 0] [1 2] [2 1] [3 1]}))}
  get-valid-rook-moves [board from-position]
  {:pre [(s/rook? (s/get-piece board from-position))]}
  (get-valid-moves-in-directions board
                                 from-position
                                 [[0 -1] [-1 0] [1 0] [0 1]]))


(defn
  ^{:doc  "Determines all possible moves for a bishop."
    :test (fn []
            (is= (get-valid-bishop-moves (s/create-board "....."
                                                         ".b.k."
                                                         "B...."
                                                         ".....")
                                         [1 1])
                 #{[0 0] [0 2] [2 0] [2 2] [3 3]}))}
  get-valid-bishop-moves [board from-position]
  {:pre [(s/bishop? (s/get-piece board from-position))]}
  (get-valid-moves-in-directions board
                                 from-position
                                 [[1 1] [1 -1] [-1 1] [-1 -1]]))








































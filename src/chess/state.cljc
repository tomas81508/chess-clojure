(ns chess.state
  "A model of the state and functions for update and lookup."
  (:use [clojure.test :only (is run-tests function?)]
        [clojure.repl :only (doc)]
        [clojure.string :only [lower-case]]
        [clojure.pprint :only [pprint]]
        [test.core :only [is= is-not]]))


(defn-
  ^{:doc  "Constructs a piece given a letter. Small letter for the black player."
    :test (fn []
            (is= (letter->value "b") {:type :bishop :owner :black})
            (is= (letter->value "p") {:type :pawn :owner :black})
            (is= (letter->value "K") {:type :king :owner :white})
            (is= (letter->value "Q") {:type :queen :owner :white})
            (is= (letter->value "r") {:type :rook :owner :black})
            (is= (letter->value "n") {:type :knight :owner :black})
            (is= (letter->value ".") nil)
            (is (thrown? Exception (letter->value "x"))))}
  letter->value [letter]
  (if (= letter ".")
    nil
    {:type (condp = (lower-case letter)
              "b" :bishop                                   ; löpare
              "n" :knight                                   ; häst eller springare
              "k" :king                                     ; kung
              "p" :pawn                                     ; bonde
              "q" :queen                                    ; drottning
              "r" :rook)                                    ; torn
     :owner (if (= letter (lower-case letter))
              :black
              :white)}))


(defn
  ^{:doc  "Creates a board from the given data."
    :test (fn []
            (is= (create-board ".pn")
                 {[0 0] nil
                  [0 1] {:type :pawn
                         :owner :black}
                  [0 2] {:type :knight
                         :owner :black}})
            (is= (create-board "..kq.r"
                               "......"
                               ".BKQ..")
                 {[0 0] nil
                  [0 1] nil
                  [0 2] {:type :king
                         :owner :black}
                  [0 3] {:type :queen
                         :owner :black}
                  [0 4] nil
                  [0 5] {:type :rook
                         :owner :black}
                  [1 0] nil
                  [1 1] nil
                  [1 2] nil
                  [1 3] nil
                  [1 4] nil
                  [1 5] nil
                  [2 0] nil
                  [2 1] {:type :bishop
                         :owner :white}
                  [2 2] {:type :king
                         :owner :white}
                  [2 3] {:type :queen
                         :owner :white}
                  [2 4] nil
                  [2 5] nil}))}
  create-board [& strings]
  (->> (map-indexed (fn [row-index string]
                      (map-indexed (fn [column-index letter]
                                     {:square [row-index column-index]
                                      :value  (letter->value (str letter))})
                                   string))
                    strings)
       (flatten)
       (reduce (fn [a v]
                 (assoc a (:square v) (:value v)))
               {})))


(defn
  ^{:doc  "Creates a state."
    :test (fn []
            (is= (create-state ".pn")
                 {:board {[0 0] nil
                          [0 1] {:type :pawn :owner :black}
                          [0 2] {:type :knight :owner :black}}
                  :players [{:id :white :direction [-1 0]}
                            {:id :black :direction [1 0]}]
                  :player-in-turn :white}))}
  create-state [& strings]
  {:board (apply create-board strings)
   :players [{:id :white :direction [-1 0]}
             {:id :black :direction [1 0]}]
   :player-in-turn :white})

(defn
  ^{:test (fn []
            (is= (get-direction (create-state ".qQ") :white) [-1 0]))}
  get-direction [state player-id]
  (->> (:players state)
       (filter (fn [p] (= (:id p) player-id)))
       (first)
       (:direction)))


(defn
  ^{:doc  "..."
    :test (fn []
            (is= (get-piece (create-board "..K") [0 0]) nil)
            (is= (get-piece (create-board "..K") [0 2]) {:type :king
                                                         :owner :white}))}
  get-piece [board position]
  (get board position))


(defn
  ^{:doc  "..."
    :test (fn []
            (is= (-> (create-board "..K")
                     (mark [0 0] {:type :queen
                                  :owner :white}))
                 (create-board "Q.K")))}
  mark [board position piece]
  (assoc board position piece))


(defn
  ^{:doc  "..."
    :test (fn []
            (is= (-> (create-board "..QK")
                     (unmark [0 2]))
                 (create-board "...K")))}
  unmark [board position]
  (assoc board position nil))

(defn
  ^{:doc  "..."
    :test (fn []
            (is= (-> (create-board "K..")
                     (move [0 0] [0 2]))
                 (create-board "..K"))
            (is= (-> (create-board "K.q")
                     (move [0 0] [0 2]))
                 (create-board "..K")))}
  move [board from-position to-position]
  (let [piece (get-piece board from-position)]
    (-> board
        (unmark from-position)
        (mark to-position piece))))

(defn
  ^{:test (fn []
            (is (bishop? {:type :bishop}))
            (is-not (bishop? {:type :king})))}
  bishop? [piece]
  (= (:type piece) :bishop))

(defn
  ^{:test (fn []
            (is (knight? {:type :knight}))
            (is-not (knight? {:type :king})))}
  knight? [piece]
  (= (:type piece) :knight))

(defn
  ^{:test (fn []
            (is (pawn? {:type :pawn}))
            (is-not (pawn? {:type :king})))}
  pawn? [piece]
  (= (:type piece) :pawn))

(defn
  ^{:test (fn []
            (is (queen? {:type :queen}))
            (is-not (queen? {:type :king})))}
  queen? [piece]
  (= (:type piece) :queen))

(defn
  ^{:test (fn []
            (is (rook? {:type :rook}))
            (is-not (rook? {:type :king})))}
  rook? [piece]
  (= (:type piece) :rook))

(defn
  ^{:doc  "..."
    :test (fn []
            (let [board (create-board ".K")]
              (is (on-board? board [0 0]))
              (is (on-board? board [0 1]))
              (is (not (on-board? board [0 -1])))))}
  on-board? [board position]
  (contains? board position))


(defn
  ^{:test (fn []
            (is= (get-owner (create-board "..K") [0 2]) :white)
            (is= (get-owner (create-board "..K") [0 0]) nil)
            (is= (get-owner {:owner :black}) :black))}
  get-owner
  ([board position]
   (get-owner (get-piece board position)))
  ([piece]
   (:owner piece)))

(defn
  ^{:test (fn []
            (is (marked? (create-board ".K") [0 1]))
            (is-not (marked? (create-board ".K") [0 0]))
            ; Outside the board
            (is-not (marked? (create-board "..") [3 4])))}
  marked? [board position]
  (not (nil? (get-piece board position))))
















































(ns chess.state
  "A model of the state and functions for update and lookup."
  (:use [clojure.test :only (is run-tests function?)]
        [clojure.repl :only (doc)]
        [clojure.string :only [lower-case]]
        [clojure.pprint :only [pprint]]
        [test.core :only [is= is-not]]))


(defn letter->value
  "Constructs a piece given a letter. Small letter for the black player."
  {:test (fn []
           (is= (letter->value "b") {:type :bishop :owner :small :moved? false})
           (is= (letter->value "p") {:type :pawn :owner :small :moved? false})
           (is= (letter->value "K") {:type :king :owner :large :moved? false})
           (is= (letter->value "Q") {:type :queen :owner :large :moved? false})
           (is= (letter->value "r") {:type :rook :owner :small :moved? false})
           (is= (letter->value "n") {:type :knight :owner :small :moved? false})
           (is= (letter->value ".") nil)
           (is (thrown? Exception (letter->value "x"))))}
  [letter]
  (if (= letter ".")
    nil
    {:type   (condp = (lower-case letter)
               "b" :bishop                                  ; löpare
               "n" :knight                                  ; häst eller springare
               "k" :king                                    ; kung
               "p" :pawn                                    ; bonde
               "q" :queen                                   ; drottning
               "r" :rook)                                   ; torn
     :owner  (if (= letter (lower-case letter))
               :small
               :large)
     :moved? false}))


(defn create-board
  "Creates a board from the given data."
  {:test (fn []
           (is= (create-board ".pn")
                {[0 0] nil
                 [1 0] {:type   :pawn
                        :owner  :small
                        :moved? false
                        :id     "1"}
                 [2 0] {:type   :knight
                        :owner  :small
                        :moved? false
                        :id     "2"}})
           (is= (create-board "..kq.r"
                              "......"
                              ".BKQ..")
                {[0 2] nil
                 [1 2] nil
                 [2 2] {:type   :king
                        :owner  :small
                        :moved? false
                        :id     "4"}
                 [3 2] {:type   :queen
                        :owner  :small
                        :moved? false
                        :id     "5"}
                 [4 2] nil
                 [5 2] {:type   :rook
                        :owner  :small
                        :moved? false
                        :id     "6"}
                 [0 1] nil
                 [1 1] nil
                 [2 1] nil
                 [3 1] nil
                 [4 1] nil
                 [5 1] nil
                 [0 0] nil
                 [1 0] {:type   :bishop
                        :owner  :large
                        :moved? false
                        :id     "1"}
                 [2 0] {:type   :king
                        :owner  :large
                        :moved? false
                        :id     "2"}
                 [3 0] {:type   :queen
                        :owner  :large
                        :moved? false
                        :id     "3"}
                 [4 0] nil
                 [5 0] nil}))}
  [& strings]
  (let [counter (atom 0)]
    (->> (reverse strings)
         (map-indexed (fn [row-index string]
                        (map-indexed (fn [column-index letter]
                                       {:square [column-index row-index]
                                        :value  (when-let [value (letter->value (str letter))]
                                                  (swap! counter inc)
                                                  (assoc value :id (str @counter)))})
                                     string)))
         (flatten)
         (reduce (fn [a v]
                   (assoc a (:square v) (:value v)))
                 {}))))

(defn create-state
  "Creates a state."
  {:test (fn []
           (is= (create-state ".pn")
                {:board          {[0 0] nil
                                  [1 0] {:type :pawn :owner :small :moved? false :id "1"}
                                  [2 0] {:type :knight :owner :small :moved? false :id "2"}}
                 :players        [{:id :large :direction [0 1]}
                                  {:id :small :direction [0 -1]}]
                 :player-in-turn :large}))}
  [& strings]
  {:board          (apply create-board strings)
   :players        [{:id :large :direction [0 1]}
                    {:id :small :direction [0 -1]}]
   :player-in-turn :large})


(defn player-in-turn [state]
  (:player-in-turn state))

(defn get-direction
  {:test (fn []
           (is= (get-direction (create-state ".qQ") :large) [0 1]))}
  [state player-id]
  (->> (:players state)
       (filter (fn [p] (= (:id p) player-id)))
       (first)
       (:direction)))

(defn get-board [state]
  (:board state))


(defn get-piece
  {:test (fn []
           (is= (get-piece (create-state "..K") [0 0]) nil)
           (is= (get-piece (create-state "..K") [2 0]) {:type   :king
                                                        :owner  :large
                                                        :moved? false
                                                        :id     "1"}))}
  [state position]
  (get (get-board state) position))

(defn update-piece
  {:test (fn []
           (is= (-> (create-state "R..")
                    (update-piece [0 0] (fn [p]
                                          (assoc p :moved? true)))
                    (get-piece [0 0])
                    (:moved?))
                true))}
  [state position f]
  (update-in state [:board position] f))

(defn mark-piece-as-moved
  {:test (fn []
           (is= (-> (create-state "R..")
                    (mark-piece-as-moved [0 0])
                    (get-piece [0 0])
                    (:moved?))
                true))}
  [state position]
  (update-piece state position (fn [p] (assoc p :moved? true))))


(defn mark
  {:test (fn []
           (is= (-> (create-state "..K")
                    (mark [0 0] {:type :queen})
                    (get-piece [0 0])
                    (:type))
                :queen))}
  [state position piece]
  (assoc-in state [:board position] piece))


(defn unmark
  {:test (fn []
           (is= (-> (create-state "..QK")
                    (unmark [2 0])
                    (get-piece [2 0]))
                nil))}
  [state position]
  (assoc-in state [:board position] nil))

(defn update-position
  "Changes the position of the piece at the from position."
  {:test (fn []
           (is= (-> (create-state "K..")
                    (update-position [0 0] [2 0]))
                (create-state "..K"))
           (is= (-> (create-state "K.q")
                    (update-position [0 0] [2 0]))
                (create-state "..K")))}
  [state from-position to-position]
  (let [piece (get-piece state from-position)]
    (-> state
        (unmark from-position)
        (mark to-position piece))))

(defn bishop?
  {:test (fn []
           (is (bishop? {:type :bishop}))
           (is-not (bishop? {:type :king})))}
  [piece]
  (= (:type piece) :bishop))

(defn king?
  {:test (fn []
           (is (king? {:type :king}))
           (is-not (king? {:type :queen}))
           (is (king? (create-state "K.") [0 0]))
           (is-not (king? (create-state "Q.") [0 0])))}

  ([piece]
   (= (:type piece) :king))
  ([state position]
   (king? (get-piece state position))))

(defn knight?
  {:test (fn []
           (is (knight? {:type :knight}))
           (is-not (knight? {:type :king})))}
  [piece]
  (= (:type piece) :knight))

(defn pawn?
  {:test (fn []
           (is (pawn? {:type :pawn}))
           (is-not (pawn? {:type :king})))}
  [piece]
  (= (:type piece) :pawn))

(defn queen?
  {:test (fn []
           (is (queen? {:type :queen}))
           (is-not (queen? {:type :king})))}
  [piece]
  (= (:type piece) :queen))

(defn rook?
  {:test (fn []
           (is (rook? {:type :rook}))
           (is-not (rook? {:type :king})))}
  [piece]
  (= (:type piece) :rook))

(defn on-board?
  {:test (fn []
           (let [state (create-state ".K")]
             (is (on-board? state [0 0]))
             (is (on-board? state [1 0]))
             (is (not (on-board? state [0 -1])))))}
  [state position]
  (contains? (get-board state) position))


(defn get-owner
  {:test (fn []
           (is= (get-owner (create-state "..K") [2 0]) :large)
           (is= (get-owner (create-state "..K") [0 0]) nil)
           (is= (get-owner {:owner :small}) :small))}
  ([state position]
   (get-owner (get-piece state position)))
  ([piece]
   (:owner piece)))

(defn marked?
  {:test (fn []
           (is (marked? (create-state ".K") [1 0]))
           (is-not (marked? (create-state ".K") [0 0]))
           ; Outside the board
           (is-not (marked? (create-state "..") [3 4])))}
  [state position]
  (not (nil? (get-piece state position))))


(defn get-position-of-pieces
  {:test (fn []
           (is= (get-position-of-pieces (create-state "ppbk"
                                                      "...."
                                                      "NP..")
                                        :large)
                #{[0 0] [1 0]})
           (is= (get-position-of-pieces (create-state "ppbk"
                                                      "...."
                                                      "NP..")
                                        :small)
                #{[0 2] [1 2] [2 2] [3 2]}))}
  [state player-id]
  (->> (keys (:board state))
       (filter (fn [position]
                 (= (get-owner state position)
                    player-id)))
       (into #{})))

(defn get-opponent-player-id
  {:test (fn []
           (is= (get-opponent-player-id (create-state "pP")
                                        :large)
                :small)
           (is= (get-opponent-player-id (create-state "pP")
                                        :small)
                :large))}
  [state player-id]
  (->> (:players state)
       (map :id)
       (filter (fn [p-id]
                 (not= p-id player-id)))
       (first)))


(defn get-king-position
  "Returns the position of the king for the given player."
  {:test (fn []
           (is= (-> (create-state "..qk.")
                    (get-king-position :small))
                [3 0]))}
  [state player-id]
  (->> (get-position-of-pieces state player-id)
       (filter (fn [p] (king? (get-piece state p))))
       (first)))



(defn update-player-in-turn
  "Updates the player in turn."
  {:test (fn []
           (is= (-> (update-player-in-turn (create-state ".."))
                    (:player-in-turn))
                :small))}
  [state]
  (let [change-player (fn [current-player-in-turn players]
                        (->> players
                             (map :id)
                             (filter (fn [id] (not= id current-player-in-turn)))
                             (first)))]
    (update state :player-in-turn change-player (:players state))))

(defn board->string
  "Returns the board visible information of the board."
  {:test (fn []
           (is= (board->string (create-board "..kQ"
                                             "pp.."))
                "..kQ\npp.."))}
  [board]
  (let [get-letter (fn [piece]
                     (if (nil? piece)
                       "."
                       (let [pre-letter (condp = (:type piece)
                                          :king "k"
                                          :queen "q"
                                          :pawn "p"
                                          :bishop "b"
                                          :knight "n"
                                          :rook "r")]
                         (if (= (:owner piece) :small)
                           pre-letter
                           (clojure.string/upper-case pre-letter)))))
        sorted-coordinates (sort-by (juxt (comp - second) first) (keys board))
        max-y (second (first sorted-coordinates))]
    (reduce (fn [string c]
              (let [piece (get board c)
                    piece-string (get-letter piece)]
                (if (and (= (first c) 0)
                         (not= (second c) max-y))
                  (str string "\n" piece-string)
                  (str string piece-string))))
            ""
            sorted-coordinates)))


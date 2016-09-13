(ns chess.state
  "A model of the state and functions for update and lookup."
  (:use [clojure.test :only (is run-tests function?)]
        [clojure.repl :only (doc)]
        [clojure.string :only [lower-case]]
        [clojure.pprint :only [pprint]]
        [test.core :only [is=]]))


(defn-
  ^{:test (fn []
            (is= (letter->value "b") {:piece :bishop :owner :black})
            (is= (letter->value "p") {:piece :pawn :owner :black})
            (is= (letter->value "K") {:piece :king :owner :white})
            (is= (letter->value "Q") {:piece :queen :owner :white})
            (is= (letter->value "r") {:piece :rook :owner :black})
            (is= (letter->value "n") {:piece :knight :owner :black})
            (is= (letter->value ".") nil)
            (is (thrown? Exception (letter->value "x"))))}
  letter->value [letter]
  (if (= letter ".")
    nil
    {:piece (condp = (lower-case letter)
              "b" :bishop
              "n" :knight
              "k" :king
              "p" :pawn
              "q" :queen
              "r" :rook)
     :owner (if (= letter (lower-case letter))
              :black
              :white)}))


(defn
  ^{:doc  "Creates a board from the given data."
    :test (fn []
            (is= (create-board ".pn")
                 {[0 0] nil
                  [0 1] {:piece :pawn
                         :owner :black}
                  [0 2] {:piece :knight
                         :owner :black}})
            (is= (create-board "..kq.r"
                               "......"
                               ".BKQ..")
                 {[0 0] nil
                  [0 1] nil
                  [0 2] {:piece :king
                         :owner :black}
                  [0 3] {:piece :queen
                         :owner :black}
                  [0 4] nil
                  [0 5] {:piece :rook
                         :owner :black}
                  [1 0] nil
                  [1 1] nil
                  [1 2] nil
                  [1 3] nil
                  [1 4] nil
                  [1 5] nil
                  [2 0] nil
                  [2 1] {:piece :bishop
                         :owner :white}
                  [2 2] {:piece :king
                         :owner :white}
                  [2 3] {:piece :queen
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



(def a [1 2 3 4])

(map inc (filter even? (map (fn [v] (* 3 v)) a)))

(->> [1 2 3 4]
     (map (fn [v] (* 3 v)))
     (filter even?)
     (map inc))

(ns chess.Fisher
  (:require [clojure.test :refer (deftest is run-tests function?)]
            [test.core :refer [is= is-not error?]]))

(defn get-Ficher-Spassky-part-game []
  "1. e4 Nf6 2. e5 Nd5 3. d4 d6 4. Nf3 g6 5. Bc4 Nb6")

(defn get-Ficher-Spassky-game []
  "1. e4 Nf6 2. e5 Nd5 3. d4 d6 4. Nf3 g6 5. Bc4 Nb6 6. Bb3 Bg7 7. Nbd2 O-O 8. h3 a5 9. a4 de5 10. de5 Na6 11. O-O Nc5 12. Qe2 Qe8 13. Ne4 Nba4 14. Ba4 Na4 15. Re1 Nb6 16. Bd2 a4 17. Bg5 h6 18. Bh4 Bf5 19. g4 Be6 20. Nd4 Bc4 21. Qd2 Qd7 22. Rad1 Rfe8 23. f4 Bd5 24. Nc5 Qc8 25. Qc3 e6 26. Kh2 Nd7 27. Nd3 c5 28. Nb5 Qc6 29. Nd6 Qd6 30. ed6 Bc3 31. bc3 f6 32. g5 hg5 33. fg5 f5 34. Bg3 Kf7 35. Ne5 Ne5 36. Be5 b5 37. Rf1 Rh8 38. Bf6 a3 39. Rf4 a2 40. c4 Bc4 41. d7 Bd5 42. Kg3 Ra3 43. c3 Rha8 44. Rh4 e5 45. Rh7 Ke6 46. Re7 Kd6 47. Re5 Rc3 48. Kf2 Rc2 49. Ke1 Kd7 50. Red5 Kc6 51. Rd6 Kb7 52. Rd7 Ka6 53. R7d2 Rd2 54. Kd2 b4 55. h4 Kb5 56. h5 c4 57. Ra1 gh5 58. g6 h4 59. g7 h3 60. Be7 Rg8 61. Bf8 h2 62. Kc2 Kc6 63. Rd1 b3 64. Kc3 h1Q 65. Rh1 Kd5 66. Kb2 f4 67. Rd1 Ke4 68. Rc1 Kd3 69. Rd1 Ke2 70. Rc1 f3 71. Bc5 Rg7 72. Rc4 Rd7 73. Re4 Kf1 74. Bd4 f2")


(defn
^{:test (fn []
          (is= (get-game-moves "1. e4 Nf6 2. e5 Nd5 3. d4 d6 4. Nf3 g6 5. Bc4 Nb6")
               ["e4" "Nf6" "e5" "Nd5" "d4" "d6" "Nf3" "g6" "Bc4" "Nb6"]))}
  get-game-moves [algebraic-notation-string]
  (->> (clojure.string/split algebraic-notation-string #" ")
       (partition 3)
       (map (partial drop 1))
       (flatten)))
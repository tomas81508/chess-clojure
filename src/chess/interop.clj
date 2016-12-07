(ns chess.interop)

(defn error [& strings]
  (throw (Exception. (apply str strings))))

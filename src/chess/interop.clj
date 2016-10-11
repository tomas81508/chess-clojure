(ns chess.interop)

(defn error [message]
  (throw (Exception. message)))
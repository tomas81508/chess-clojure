(ns chess.instrumentation
  (:require [clojure.spec :as s]
            [clojure.spec.test :as spec-test]
            [chess.spec]
            [chess.server]))

(s/fdef chess.server/game-response
        :args (s/cat :game :chess.spec/game))

(spec-test/instrument `chess.server/game-response)


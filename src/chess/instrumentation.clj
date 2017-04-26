(ns chess.instrumentation
  (:require [clojure.spec :as s]
            [clojure.spec.test :as spec-test]
            [chess.spec]
            [chess.mapper]))

(s/fdef chess.mapper/game->view-game
        :ret :chess.spec/game)

(spec-test/instrument `chess.mapper/game->view-game)


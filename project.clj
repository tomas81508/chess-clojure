(defproject chess "0.1.0-SNAPSHOT"
  :description "Learning Clojure by constructing the game Chess"
  :license {}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot chess.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

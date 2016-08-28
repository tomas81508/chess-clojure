(ns test.core
  (:use [clojure.test :only (is)]))

(defmacro is= [actual expected]
  `(let [actual# ~actual
         expected# ~expected
         equal# (= actual# expected#)]
     (do
       (when-not equal#
         (println "Actual:\t\t" actual# "\nExpected:\t" expected#))
       (is (= actual# expected#)))))


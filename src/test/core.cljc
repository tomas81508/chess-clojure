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

(defmacro is-not [actual]
  `(is (not ~actual)))

(defmacro error? [actual]
  `(try (do
          ~actual
          (println "An error was expected.")
          (is false))
        (catch #?(:clj Exception :cljs js/Object) e#
          (is true))))

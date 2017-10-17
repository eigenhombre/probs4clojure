(ns probs4clojure.test-util
  (:require [speclj.core :refer :all]
            [clojure.test :refer [deftest testing is]]
            [clojure.walk :refer [postwalk-replace]]))


(defmacro solves
  "
  The `solves` macro presents and tests a solution to a
  [4clojure.com](http://4clojure.com) problem by taking the first
  expression, substituting it for __ in all subsequent expressions,
  and evaluating each resulting expression for truthiness (i.e.,
  evaluating the resulting Speclj assertions).
  "
  [expr & tests]
  (let [replacef# (fn [t] (postwalk-replace {'__ expr} t))
        newtests# (map replacef# tests)]
    #_`(deftest ~(symbol (str "test-" (rand-int 100000)))
       (println '~expr)
       ~@(for [t# newtests#
               :let [should# `(is ~t#)
                     shouldstr# (str t#)]]
           `(testing ~(str shouldstr#)
              ~should#)))))


(defmacro problem
  "
  The `problem` macro presents and tests a solution to a
  [4clojure.com](http://4clojure.com) problem by taking `expr`,
  substituting it for `__` in all subsequent expressions, and evaluating
  each resulting expression for truthiness (i.e., evaluating the
  resulting clojure.test assertions).
  "
  [problem-number expr & tests]
  (let [replacef# (fn [t] (postwalk-replace {'__ expr} t))
        newtests# (map replacef# tests)]
    `(deftest ~(symbol (str "problem-" problem-number))
       (println (str "problem " ~problem-number))
       ~@(for [t# newtests#
               :let [should# `(is ~t#)
                     shouldstr# (str t#)]]
           `(testing ~(str shouldstr#)
              ~should#)))))

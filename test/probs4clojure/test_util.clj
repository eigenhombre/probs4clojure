(ns probs4clojure.test-util
  (:require [speclj.core :refer :all]
            [clojure.test :refer [deftest testing is]]
            [clojure.walk :refer [postwalk-replace]]))


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
       (print (str "problem " ~problem-number "..."))
       (flush)
       (do ;; when (= ~problem-number 134)  ;<- use when debugging specific problem)
         (let [t0# (System/currentTimeMillis)
               result#
               (do
                 ~@(for [t# newtests#
                         :let [should# `(is ~t#)
                               shouldstr# (str t#)]]
                     `(testing ~(str shouldstr#)
                        ~should#)))
               t1# (System/currentTimeMillis)]
           (println (- t1# t0#))
           result#)))))

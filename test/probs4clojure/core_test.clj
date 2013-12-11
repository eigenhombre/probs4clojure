(ns probs4clojure.core-test
  (:use midje.sweet)
  (:require [probs4clojure.core :refer :all]))

;; Redoing these problems for practice now...

(defmacro solves [expr & tests]
  (let [replacef# (fn [t] (clojure.walk/postwalk-replace {'__ expr} t))
        newtests# (map replacef# tests)]
    `(fact (and ~@newtests#) => truthy)))


;; Problem 1:
(solves true (= __ true))

;; Problem 2:
(solves 4
 (= (- 10 (* 2 3)) __))

;; Problem 3:
(solves "HELLO WORLD"
 (= __ (.toUpperCase "hello world")))

;; Problem 4:
(solves
 (= (list :a :b :c) '(:a :b :c)))

;; Problem 5:
(solves '(1 2 3 4)
 (= __ (conj '(2 3 4) 1))
 (= __ (conj '(3 4) 2 1)))

;; Problem 6:
(solves [:a :b :c]
 (= (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c)))

;; Problem 7:
(solves [1 2 3 4]
 (= __ (conj [1 2 3] 4))
 (= __ (conj [1 2] 3 4)))

;; Problem 8:
(solves #{:a :b :c :d}
 (= __ (set '(:a :a :b :c :c :c :c :d :d)))
 (= __ (clojure.set/union #{:a :b :c} #{:b :c :d})))

;; Problem 9:
(solves 2
 (= #{1 2 3 4} (conj #{1 4 3} __)))

;; Problem 10:
(solves 20
 (= __ ((hash-map :a 10, :b 20, :c 30) :b))
 (= __ (:b {:a 10, :b 20, :c 30})))

;; Problem 11:
(solves [:b 2]
 (= {:a 1, :b 2, :c 3} (conj {:a 1} __ [:c 3])))

;; Problem 12:
(solves 3
 (= __ (first '(3 2 1)))
 (= __ (second [2 3 4]))
 (= __ (last (list 1 2 3))))

;; Problem 13:
(solves [20 30 40]
 (= __ (rest [10 20 30 40])))

;; Problem 14:
(solves 8
 (= __ ((fn add-five [x] (+ x 5)) 3))
 (= __ ((fn [x] (+ x 5)) 3))
 (= __ (#(+ % 5) 3))
 (= __ ((partial + 5) 3)))

;; Problem 15:
(solves #(* 2 %)
 (= (__ 2) 4)
 (= (__ 3) 6)
 (= (__ 11) 22)
 (= (__ 7) 14))

;; Problem 16:
(solves #(format "Hello, %s!" %)
 (= (__ "Dave") "Hello, Dave!")
 (= (__ "Jenn") "Hello, Jenn!")
 (= (__ "Rhea") "Hello, Rhea!"))

;; Problem 17:
(solves [6 7 8]
        (= __ (map #(+ % 5) '(1 2 3))))

;; Problem 18:
(solves [6 7]
 (= __ (filter #(> % 5) '(3 4 5 6 7))))

;; Problem 19:
(solves #(first (reverse %))
 (= (__ [1 2 3 4 5]) 5)
 (= (__ '(5 4 3)) 3)
 (= (__ ["b" "c" "d"]) "d"))

;; Problem 20:
(solves #(second (reverse %))
 (= (__ (list 1 2 3 4 5)) 4)
 (= (__ ["a" "b" "c"]) "b")
 (= (__ [[1 2] [3 4]]) [1 2]))

;; Problem 21:
(solves (fn [s n] (first (drop n s)))
 (= (__ '(4 5 6 7) 2) 6)
 (= (__ [:a :b :c] 0) :a)
 (= (__ [1 2 3 4] 1) 2)
 (= (__ '([1 2] [3 4] [5 6]) 2) [5 6]))

;; Problem 22:
(solves #(apply + (map (fn [_] 1) %))
 (= (__ '(1 2 3 3 1)) 5)
 (= (__ "Hello World") 11)
 (= (__ [[1 2] [3 4] [5 6]]) 3)
 (= (__ '(13)) 1)
 (= (__ '(:a :b :c)) 3))

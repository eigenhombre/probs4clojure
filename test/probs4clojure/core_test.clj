(ns probs4clojure.core-test
  (:use midje.sweet)
  (:require [probs4clojure.core :refer :all]))

;; Redoing these problems for practice now...

(defmacro solves
  "
  Present and test a solution to a 4clojure.com problem by taking the
  first expression, substituting it for __ in all subsequent
  expressions, and evaluating each resulting expression for truthiness
  (i.e., evaluating the resulting Midje fact).
  "
  [expr & tests]
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
(solves  ;; solution less amenable to `solves` macro, added inline...
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

;; Problem 23:
(solves (fn [s]
          (let [svec (into [] s)]
            (for [i (range (count svec) 0 -1)]
              (get svec (dec i)))))
 (= (__ [1 2 3 4 5]) [5 4 3 2 1])
 (= (__ (sorted-set 5 7 2 7)) '(7 5 2))
 (= (__ [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]]))

;; Problem 24:
(solves (fn [s] (apply + s))
 (= (__ [1 2 3]) 6)
 (= (__ (list 0 -2 5 5)) 8)
 (= (__ #{4 2 1}) 7)
 (= (__ '(0 0 -1)) -1)
 (= (__ '(1 10 3)) 14))

;; Problem 25:
(solves #(filter odd? %)
 (= (__ #{1 2 3 4 5}) '(1 3 5))
 (= (__ [4 2 1 6]) '(1))
 (= (__ [2 2 4 6]) '())
 (= (__ [1 1 1 3]) '(1 1 1 3)))

;; Problem 26:
(solves (fn [n]
          (take n (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1]))))
 (= (__ 3) '(1 1 2))
 (= (__ 6) '(1 1 2 3 5 8))
 (= (__ 8) '(1 1 2 3 5 8 13 21)))

;; Problem 27:
(solves (fn [s]
          (let [pivot (-> s count (#(/ % 2)) int)
                rhs (take pivot s)
                lhs (take pivot (reverse s))]
            (= rhs lhs)))
 (false? (__ '(1 2 3 4 5)))
 (true? (__ "racecar"))
 (true? (__ [:foo :bar :foo]))
 (true? (__ '(1 1 3 3 1 1)))
 (false? (__ '(:a :b :c))))

;; Problem 28:
(solves (fn flat [[h & t :as l]]
          (when (seq l)
            (if (coll? h)
              (concat (flat h)
                      (flat t))
              (cons h (flat t)))))
 (= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
 (= (__ ["a" ["b"] "c"]) '("a" "b" "c"))
 (= (__ '((((:a))))) '(:a)))

;; Problem 29:
(solves (fn [s] (apply str (filter #(. Character isUpperCase %) s)))
 (= (__ "HeLlO, WoRlD!") "HLOWRD")
 (empty? (__ "nothing"))
 (= (__ "$#A(*&987Zf") "AZ"))

;; Problem 30:
(solves #(->> %
              (partition-by identity)
              (map first))
 (= (apply str (__ "Leeeeeerrroyyy")) "Leroy")
 (= (__ [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
 (= (__ [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2])))

;; Problem 31:
(solves (partial partition-by identity)
 (= (__ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
 (= (__ [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
 (= (__ [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4]))))

;; Problem 32:
(solves (partial mapcat (fn [x] [x x]))
 (= (__ [1 2 3]) '(1 1 2 2 3 3))
 (= (__ [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
 (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
 (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))

;; Problem 33:
(solves (fn [s n] (mapcat #(repeat n %) s))
 (= (__ [1 2 3] 2) '(1 1 2 2 3 3))
 (= (__ [:a :b] 4) '(:a :a :a :a :b :b :b :b))
 (= (__ [4 5 6] 1) '(4 5 6))
 (= (__ [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
 (= (__ [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))

;; Problem 34:
(solves (fn [s e] (take (- e s) (iterate inc s)))
 (= (__ 1 4) '(1 2 3))
 (= (__ -2 2) '(-2 -1 0 1))
 (= (__ 5 8) '(5 6 7)))

;; Problem 35:
(solves 7
 (= __ (let [x 5] (+ 2 x)))
 (= __ (let [x 3, y 10] (- y x)))
 (= __ (let [x 21] (let [y 3] (/ x y)))))

;; Problem 36:
(solves [x 7, y 3, z 1]
 (= 10 (let __ (+ x y)))
 (= 4 (let __ (+ y z)))
 (= 1 (let __ z)))

;; Problem 37:
(solves "ABC"
 (= __ (apply str (re-seq #"[A-Z]+" "bA1B3Ce "))))

;; Problem 38:
(solves (fn [& s] (reduce (fn [a b] (if (> a b) a b)) s))
 (= (__ 1 8 3 4) 8)
 (= (__ 30 20) 30)
 (= (__ 45 67 11) 67))

;; Problem 39:
(solves (partial mapcat vector)
 (= (__ [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
 (= (__ [1 2] [3 4 5 6]) '(1 3 2 4))
 (= (__ [1 2 3 4] [5]) [1 5])
 (= (__ [30 20] [25 15]) [30 25 20 15]))

;; Problem 40:
(solves (fn [d x] (rest (mapcat (fn [xx] [d xx]) x)))
 (= (__ 0 [1 2 3]) [1 0 2 0 3])
 (= (apply str (__ ", " ["one" "two" "three"])) "one, two, three")
 (= (__ :z [:a :b :c :d]) [:a :z :b :z :c :z :d]))

;; Problem 41:
(solves (fn [s n] (mapcat #(if (= (count %) n) (drop-last %) %)
                         (partition-all n s)))
 (= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
 (= (__ [:a :b :c :d :e :f] 2) [:a :c :e])
 (= (__ [1 2 3 4 5 6] 4) [1 2 3 5 6]))

;; Problem 42:
(solves #(apply *' (range 1 (inc %)))
 (= (__ 1) 1)
 (= (__ 3) 6)
 (= (__ 5) 120)
 (= (__ 8) 40320))

;; Problem 43:
(solves (fn [s n]
          (map (fn [i]
                 (map (fn [ss] (nth ss i))
                      (partition n s)))
               (range n)))
 (= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
 (= (__ (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
 (= (__ (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9))))

;; Problem 44:
(solves (fn [pos s]
          (map (fn [i] (nth s (mod (+ pos i) (count s))))
               (range (count s))))
 (= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
 (= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
 (= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1))
 (= (__ 1 '(:a :b :c)) '(:b :c :a))
 (= (__ -4 '(:a :b :c)) '(:c :a :b)))

;; Problem 45:
(solves [1 4 7 10 13]
 (= __ (take 5 (iterate #(+ 3 %) 1))))

;; Problem 46:
(solves (fn [f] (fn [& args] (apply f (reverse args))))
 (= 3 ((__ nth) 2 [1 2 3 4 5]))
 (= true ((__ >) 7 8))
 (= 4 ((__ quot) 2 8))
 (= [1 2 3] ((__ take) [1 2 3 4 5] 3)))

;; Problem 47:
(solves 4
 (contains? #{4 5 6} __)
 (contains? [1 1 1 1 1] __)
 (contains? {4 :a 2 :b} __)
 (not (contains? '(1 2 4) __)))

;; Problem 48:
(solves 6
 (= __ (some #{2 7 6} [5 6 7 8]))
 (= __ (some #(when (even? %) %) [5 6 7 8])))

;; Problem 49:
(solves (fn [n s] (list (take n s) (drop n s)))
 (= (__ 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
 (= (__ 1 [:a :b :c :d]) [[:a] [:b :c :d]])
 (= (__ 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]]))

;; Problem 50:
(solves #(->> %
              (group-by type)
              vals)
 (= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
 (= (set (__ [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
 (= (set (__ [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]}))

;; Problem 51:
(solves [1 2 3 4 5]
        (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] __] [a b c d])))

;; Problem 52:
(solves [c e]
        (= [2 4] (let [[a b c d e f g] (range)] __)))

;; Problem 53:
(solves (fn [s] (let [deltas (map (fn [[a b]] [(- b a) a b]) (partition 2 1 s))
                     series (partition-by first deltas)
                     longest-length (->> series
                                         (group-by count)
                                         keys
                                         (apply max))
                     desired-sequence (->> series
                                           (filter #(= (count %) longest-length))
                                           first)
                     start (second (first desired-sequence))
                     end (int (nth (last desired-sequence) 2))]
                 (range start (inc end))))
 (= (__ [1 0 1 2 3 0 4 5]) [0 1 2 3])
 (= (__ [5 6 1 3 2 7]) [5 6])
 (= (__ [2 3 3 4 5]) [3 4 5])
 (= (__ [7 6 5 4]) []))

;; Problem 54:
(solves (fn f [c s]
          (lazy-seq
           (let [nxt (take c s)]
             (when (and (seq s) (= (count nxt) c))
               (cons (take c s) (f c (drop c s)))))))
 (= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
 (= (__ 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
 (= (__ 3 (range 8)) '((0 1 2) (3 4 5))))

;; Problem 55:
(solves #(->> %
              (group-by identity)
              (map (fn [[k v]] [k (count v)]))
              (into {}))
 (= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
 (= (__ [:b :a :b :a :b]) {:a 2, :b 3})
 (= (__ '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2}))

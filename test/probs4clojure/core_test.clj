(ns probs4clojure.core-test
  (:require [midje.sweet :refer :all]
            [probs4clojure.core :refer :all]))


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


(defmacro dbg
  "
  Handy little debug macro
  "
  [x]
  `(println '~x "=" ~x))


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

;; Problem 56:
(solves (fn [s]
          (loop [ret []
                 s s
                 seen #{}]
            (if-not (seq s)
              ret
              (recur (if-not (seen (first s)) (conj ret (first s)) ret)
                     (rest s)
                     (conj seen (first s))))))
 (= (__ [1 2 1 3 1 2 4]) [1 2 3 4])
 (= (__ [:a :a :b :b :c :c]) [:a :b :c])
 (= (__ '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
 (= (__ (range 50)) (range 50)))

;; Problem 57:
(solves [5 4 3 2 1]
        (= __ ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5)))

;; Problem 58:
(solves (fn outer [f & fs]
          (if fs
            (fn [& x] (f (apply (apply outer fs) x)))
            (fn [& xs] (apply f xs))))
 (= [3 2 1] ((__ rest reverse) [1 2 3 4]))
 (= 5 ((__ (partial + 3) second) [1 2 3 4]))
 (= true ((__ zero? #(mod % 8) +) 3 5 7 9))
 (= "HELLO" ((__ #(.toUpperCase %) #(apply str %) take) 5 "hello world")))

;; Problem 59:
(solves (fn [& fns]
          (fn [& xs]
            (for [f fns] (apply f xs))))
 (= [21 6 1] ((__ + max min) 2 3 5 1 6 4))
 (= ["HELLO" 5] ((__ #(.toUpperCase %) count) "hello"))
 (= [2 6 4] ((__ :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})))

;; Problem 60:
(solves (fn reduxions
          ([op prev s]
             (lazy-seq
              (if (seq s)
                (cons prev (reduxions op (op prev (first s)) (rest s)))
                [prev])))
          ([op s]
             (reduxions op (op (first s)) (rest s))))
 (= (take 5 (__ + (range))) [0 1 3 6 10])
 (= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
 (= (last (__ * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120))


;; Problem 61:
(solves #(into {} (map vector %1 %2))
 (= (__ [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
 (= (__ [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"})
 (= (__ [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"}))


;; Problem 62:
(solves (fn itr [f x]
          (lazy-seq
           (cons x (itr f (f x)))))
 (= (take 5 (__ #(* 2 %) 1)) [1 2 4 8 16])
 (= (take 100 (__ inc 0)) (take 100 (range)))
 (= (take 9 (__ #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3]))))


;; Problem 63:
(solves (fn [pred s]
          (->> s
               (map (fn [x] {(pred x) [x]}))
               (apply (partial merge-with concat))))
 (= (__ #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})
 (= (__ #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
    {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})
 (= (__ count [[1] [1 2] [3] [1 2 3] [2 3]])
    {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]}))


;; Problem 64:
(solves +
  (= 15 (reduce __ [1 2 3 4 5]))
  (=  0 (reduce __ []))
  (=  6 (reduce __ 1 [2 3])))


;; Problem 65
(solves (fn [s]
          (let [e (empty s)]
            (if (identical? '() e)
              :list
              ({#{} :set, {} :map, [] :vector} e))))
        (= :map (__ {:a 1, :b 2}))
        (= :list (__ (range (rand-int 20))))
        (= :vector (__ [1 2 3 4 5 6]))
        (= :set (__ #{10 (rand-int 5)}))
        (= [:map :set :vector :list] (map __ [{} #{} [] ()])))


;; Problem 66
(solves (fn [a b]
          (loop [a a, b b]
            (cond
             (= a b) a
             (> a b) (recur (- a b) b)
             :else (recur a (- b a)))))
        (= (__ 2 4) 2)
        (= (__ 10 5) 5)
        (= (__ 5 7) 1)
        (= (__ 1023 858) 33))


;; Problem 67
(solves
  #(take %
         (remove
          (fn [n]
            (some (fn [x] (= (rem n x) 0))
                  (range 2 n)))
          (drop 2 (range))))
 (= (__ 2) [2 3])
 (= (__ 5) [2 3 5 7 11])
 (= (last (__ 100)) 541))


;; Problem 68
(solves
  [7 6 5 4 3]
  (= __
     (loop [x 5
            result []]
       (if (> x 0)
         (recur (dec x) (conj result (+ 2 x)))
         result))))


;; Problem 69
;; OLD:
;; (fn [fun & maps]
;;   (let [pairs (apply concat
;;                      (for [mm maps]
;;                        (for [[k, v] mm] [k v])))]
;;     (loop [p pairs, ret {}]
;;       (if (seq p)
;;         (let [[k, v] (first p),
;;               lookup (ret k)
;;               insert (if lookup (fun lookup v) v)]
;;           (recur (rest p)
;;                  (conj ret (assoc {} k insert))))
;;         ret))))

(solves
  (fn [op & maps]
    (letfn [(f [op, m, o]
              (let [km (set (keys m))
                    ko (set (keys o))
                    common-keys (clojure.set/intersection km ko)
                    unique-m (clojure.set/difference km common-keys)
                    unique-o (clojure.set/difference ko common-keys)]
                (merge (into {} (for [k common-keys]
                                  [k (op (m k) (o k))]))
                       (into {} (for [k unique-m] [k (m k)]))
                       (into {} (for [k unique-o] [k (o k)])))))]
      (reduce (partial f op) maps)))
  (= (__ * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
     {:a 4, :b 6, :c 20})
  (= (__ - {1 10, 2 20} {1 3, 2 10, 3 15})
     {1 7, 2 10, 3 15})
  (= (__ concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
     {:a [3 4 5], :b [6 7], :c [8 9]}))


;; Problem 70
(solves
 (fn [s]
   (-> s
       (clojure.string/replace #"[\.\!\,\-\_]" "")
       (clojure.string/split #"\s+")
       (->> (sort #(.compareToIgnoreCase %1 %2)))))
 (= (__  "Have a nice day.")
    ["a" "day" "Have" "nice"])
 (= (__  "Clojure is a fun language!")
    ["a" "Clojure" "fun" "is" "language"])
 (= (__  "Fools fall for foolish follies.")
    ["fall" "follies" "foolish" "Fools" "for"]))


;; Problem 71
(solves
  last
  (= (__ (sort (rest (reverse [2 5 4 1 3 6]))))
     (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (__))
     5))


;; Problem 72
(solves
  (partial reduce +)
  (= (__ (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
     (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (__))
     11))


;; Problem 73
(solves
  (fn [b]
    (some (fn [p]
            (if (or
                 ;; diag:
                 (every? #{p}
                         (for [i (range 3)]
                           ((b i) i)))
                 ;; anti-diag:
                 (every? #{p}
                         (for [i (range 3)]
                           ((b i) (- 2 i))))
                 ;; across:
                 (some (fn [i]
                         (every? #{p} (for [j (range 3)] ((b i) j))))
                     (range 3))
                 ;; down:
                 (some (fn [i]
                         (every? #{p} (for [j (range 3)] ((b j) i))))
                       (range 3)))
              p))
          [:x :o]))

  (= nil (__ [[:e :e :e]
              [:e :e :e]
              [:e :e :e]]))
  (= :x (__ [[:x :e :o]
             [:x :e :e]
             [:x :e :o]]))
  (= :o (__ [[:e :x :e]
             [:o :o :o]
             [:x :e :x]]))
  (= nil (__ [[:x :e :o]
              [:x :x :e]
              [:o :x :o]]))
  (= :x (__ [[:x :e :e]
             [:o :x :e]
             [:o :e :x]]))
  (= :o (__ [[:x :e :o]
             [:x :o :e]
             [:o :e :x]]))
  (= nil (__ [[:x :o :x]
              [:x :o :x]
              [:o :x :o]])))


;; Problem 74
(solves
  (fn [s]
    (letfn [(is-square [n]
              (let [r (Math/sqrt n)
                    r (int r)]
                (= (* r r) n)))]
      (-> s
          (clojure.string/split #",")
          (->> (map (fn [s] (Integer/parseInt s)))
               (filter is-square)
               (map str)
               (clojure.string/join ",")))))
  (= (__ "4,5,6,7,8,9") "4,9")
  (= (__ "15,16,25,36,37") "16,25,36"))


;; Problem 75
(solves
  (fn [n]
    (if (= 1 n)
      1
      (letfn [(gcd [a b]
                (loop [a a, b b]
                  (cond
                   (= a b) a
                   (> a b) (recur (- a b) b)
                   :else (recur a (- b a)))))]
        (->> n
             (range 1)
             (map (partial gcd n))
             (filter (partial = 1))
             count))))
  (= (__ 1) 1)
  (= (__ 10) (count '(1 3 7 9)) 4)
  (= (__ 40) 16)
  (= (__ 99) 60))


;; Problem 76
(solves
  (= [1 3 5 7 9 11]
     (letfn
         [(foo [x y] #(bar (conj x y) y))
          (bar [x y] (if (> (last x) 10)
                       x
                       #(foo x (+ 2 y))))]
       (trampoline foo [] 1))))


;; Problem 77
(solves
  (fn [words]
    (->>
      (map (fn [w] (set (filter #(= (sort w) (sort %)) words))) words)
      (remove #(= (count %) 1))
      set))
  (= (__ ["meat" "mat" "team" "mate" "eat"])
     #{#{"meat" "team" "mate"}})
  (= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
     #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}}))


;; Problem 78
(solves
  (fn [f & rest]
    (if (fn? f)
      (recur (apply f rest) nil)
      f))
  (= (letfn [(triple [x] #(sub-two (* 3 x)))
             (sub-two [x] #(stop?(- x 2)))
             (stop? [x] (if (> x 50) x #(triple x)))]
       (__ triple 2))
     82)
  (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
             (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
       (map (partial __ my-even?) (range 6)))
     [true false true false true false]))


;; Problem 79
(solves
  (fn [s]
    (let [nested-paths (loop [[r & rs] (reverse s)
                              ret []]
                         (if-not (seq ret)
                           (recur rs (map vector r))
                           (if r
                             (recur rs
                                    (let [p (partition 2 1 ret)]
                                      (for [[a [l1 l2]] (map vector r p)]
                                        [(conj l1 a) (conj l2 a)])))
                             ret)))
          f (fn f [[r l & a]]
              [(concat r a) (concat l a)])
          g (fn g [x] (if-not (coll? (ffirst x))
                      x
                      (g (mapcat f x))))]
      (->> nested-paths
           g
           (group-by (partial apply +))
           (sort-by first)
           ffirst)))
  (= 7 (__ '([1]
             [2 4]
             [5 1 4]
             [2 3 4 5]))) ; 1->2->1->3
  (= 20 (__ '([3]
              [2 4]
              [1 9 3]
              [9 9 2 4]
              [4 6 6 7 8]
              [5 7 3 5 1 4])))) ; 3->4->3->2->7->1


;; Problem 80:
(solves
  (fn [n]
    (let [subs (filter #(zero? (rem n %)) (range 1 n))
          sum (apply + subs)]
      (= sum n)))
 (= (__ 6) true)
 (= (__ 7) false)
 (= (__ 496) true)
 (= (__ 500) false)
 (= (__ 8128) true))


;; Problem 81:
(solves
  (fn [a b]
    (set (filter #(and (a %) (b %)) (set (concat a b)))))
  (= (__ #{0 1 2 3} #{2 3 4 5}) #{2 3})
  (= (__ #{0 1 2} #{3 4 5}) #{})
  (= (__ #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d}))


;; Problem 82:
(solves
  (fn [s]
    (letfn [(perms [s]  ;; Find all permutations of a sequence
              (if (= (count s) 1)
                (list s)
                (apply concat
                       (for [x s]
                         (map (partial cons x)
                              (perms (remove #{x} s)))))))
            (shrink-half [as bs]  ;; Remove equal leading parts
              (loop [[a & aa :as as] as, [b & bb :as bs] bs]
                (cond
                 (and (nil? a) (nil? b)) [[] []]
                 (= a b) (recur aa bb)
                 :else [as bs])))
            (shrink-both [as bs]  ;; Remove equal leading and trailing parts
              (->> (shrink-half as bs)
                   (map reverse)
                   (apply shrink-half)))
            (valid-delta [as bs]  ;; Deltas are valid if stripped portions
                                  ;; do not vary by more than a character
              (->> (shrink-both as bs)
                   (map count)
                   (apply max)
                   (> 2)))
            (found-one? [s]
              (every? (partial apply valid-delta) s))]
      (let [permutations-in-pairs (map (partial partition 2 1) (perms s))]
        (boolean (some found-one? permutations-in-pairs)))))
  (= true (__ #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))
  (= false (__ #{"cot" "hot" "bat" "fat"}))
  (= false (__ #{"to" "top" "stop" "tops" "toss"}))
  (= true (__ #{"spout" "do" "pot" "pout" "spot" "dot"}))
  (= true (__ #{"share" "hares" "shares" "hare" "are"}))
  (= false (__ #{"share" "hares" "hare" "are"})))


;; Problem 83:
(solves
  (fn [& bs]
    (boolean (and (some true? bs)
                  (not (every? true? bs)))))
  (= false (__ false false))
  (= true (__ true false))
  (= false (__ true))
  (= true (__ false true false))
  (= false (__ true true true))
  (= true (__ true true true false)))


;; ### Problem 86: <a href="http://www.4clojure.com/problem/86">Happy Numbers</a>
(solves
  (fn [n]
    (let [f (fn [x]
              (->> x
                   str
                   (map #(Integer/parseInt (str %)))
                   (map #(* % %))
                   (apply +)))
          s (iterate f n)]
      (loop [seen #{}
             [x & xs] s]
        (cond
         (= x 1) true
         (seen x) false
         :else (recur (conj seen x) xs)))))
  (= (__ 7) true)
  (= (__ 986543210) true)
  (= (__ 2) false)
  (= (__ 3) false))


;; ### Problem 87:
;; Doesn't exist!

;; ### Problem 88: <a href="http://www.4clojure.com/problem/88">Symmetric Difference</a>
;;
;; The problem is simple if you use the `clojure.set` library and the
;; `remove` function to eliminate the intersection elements from both
;; sets before combining them.
(solves
  (fn [s1 s2]
    (let [both (clojure.set/intersection s1 s2)]
      (set (clojure.set/union (remove both s1)
                              (remove both s2)))))

  (= (__ #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
  (= (__ #{:a :b :c} #{}) #{:a :b :c})
  (= (__ #{} #{4 5 6}) #{4 5 6})
  (= (__ #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]}))


;; ### Problem 89: <a href="http://www.4clojure.com/problem/89">Graph Tour</a>
;;
;; The problem is manageable with the following graph theoretical
;; result from [Wikipedia](http://en.wikipedia.org/wiki/Eulerian_path)
;; that "an undirected graph has an Eulerian trail [a walk that uses
;; each edge exactly once] if and only if no more than two vertices
;; has an odd degree [number of edges coming out of it] and it if all
;; its vertices... belong to a simply-connected component."
;;
;; The connectedness property can be obtained by doing a traversal of
;; the graph (in this case, depth-first) and tracking whether each
;; node was visited.  The degree of the vertices can be obtained that
;; way as well by tracking which vertices have which neighbor.
(solves
  (fn [pairs]
    (letfn [(pairs-to-neighbor-list-map [pairs]
              (loop [[[k v] & pairs] pairs
                     g {}]
                (if-not k
                  g
                  (let [g (update-in g [k :neighbors] conj v)
                        g (update-in g [v :neighbors] conj k)]
                    (recur pairs g)))))

            (set-explored [g i]
              (assoc-in g [i :explored] true))

            (explored [g i]
              (get-in g [i :explored]))

            (get-neighbors [g i]
              (get-in g [i :neighbors]))

            (dfs [g i]
              (let [g (set-explored g i)
                    js (get-neighbors g i)]
                (loop [g g, [j & js] js]
                  (cond
                   (not j)        g
                   (explored g j) (recur g js)
                   :else          (recur (dfs g j) js)))))

            (all-connected [g]
              (let [v (first (keys g))
                    exp (dfs g v)]
                (every? (partial = true) (map :explored (vals exp)))))

            (degree [v] (-> v :neighbors count))

            (num-odd-vertices [g]
              (count (filter (comp odd? degree) (vals g))))]
      (let [g (pairs-to-neighbor-list-map pairs)]
        (and (<= (num-odd-vertices g) 2)
             (all-connected g)))))

        (= true (__ [[:a :b]]))
        (= false (__ [[:a :a] [:b :b]]))
        (= false (__ [[:a :b] [:a :b] [:a :c] [:c :a]
                      [:a :d] [:b :d] [:c :d]]))
        (= true (__ [[1 2] [2 3] [3 4] [4 1]]))
        (= true (__ [[:a :b] [:a :c] [:c :b] [:a :e]
                     [:b :e] [:a :d] [:b :d] [:c :e]
                     [:d :e] [:c :f] [:d :f]]))
        (= false (__ [[1 2] [2 3] [2 4] [2 5]])))



;; ### Problem 90: <a href="http://www.4clojure.com/problem/90">Cartesian Product</a>
;;
;; `for` gives us the needed functionality very easily.
(solves
 (fn [sa sb]
   (set (for [a sa, b sb] [a b])))

  (= (__ #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
   #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
     ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
     ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]})
  (= (__ #{1 2 3} #{4 5})
   #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})
  (= 300 (count (__ (into #{} (range 10))
                  (into #{} (range 30))))))


;; ### Problem 103: <a href="http://www.4clojure.com/problem/103">Generating k-combinations</a>
;;
;; Note: these both scale rather badly.  Benchmark with, e.g.,:
(comment (doseq [i (range 12)]
           (print i "...")
           (time (println (f i (range i))))))

;; Engelberg's [math.combinatorics
;; implementation](https://github.com/clojure/math.combinatorics/blob/master/src/main/clojure/clojure/math/combinatorics.clj)
;; relies heavily on Knuth, but seems to have much better time
;; complexity than my solutions.

;; OLD:
(comment (fn [k s]
           (set
            (filter #(= (count %) k)
                    (loop [n k, ret (map hash-set s)]
                      (if (zero? n)
                        ret
                        (recur (dec n)
                               (for [v ret, x s]
                                 (conj v x)))))))))
;; New solution:
(solves
  (fn [k s]
    (set
     (loop [k k, ss (map hash-set s)]
       (if (<= k 1)
         ss
         (recur (dec k)
                (for [en ss, e0 s :when (not (some #{e0} en))]
                  (conj en e0)))))))

  (= (__ 1 #{4 5 6}) #{#{4} #{5} #{6}})
  (= (__ 10 #{4 5 6}) #{})
  (= (__ 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}})
  (= (__ 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                           #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}})
  (= (__ 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}})
  (= (__ 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                        #{:a "abc"} #{:a "efg"} #{"abc" "efg"}}))


;; Problem 137:
(solves (fn [n base]
          (loop [n n, ret []]
            (if (zero? n)
              (if (empty? ret) [0] ret)
              (recur (quot n base) (cons (mod n base) ret)))))
  (= [1 2 3 4 5 0 1] (__ 1234501 10))
  (= [0] (__ 0 11))
  (= [1 0 0 1] (__ 9 2))
  (= [1 0] (let [n (rand-int 100000)](__ n n)))
  (= [16 18 5 24 15 1] (__ Integer/MAX_VALUE 42)))


;; ### Problem 141: <a href="http://www.4clojure.com/problem/141">Tricky card games</a>
;;
;; We want to provide a function which determines the winning card of
;; a sequence of plays, defined by:
;;
;; 1. The highest trump, if applicable;
;; 1. The highest card in the suit that was led, otherwise.
;;
;; We calculate which suit was led, and then order the cards according
;; to their "value" in descending order, with high trumps first,
;; followed by low trumps, followed by high cards in the leading suit,
;; followed by low cards in the leading suit.  While `trump-cards`
;; will be empty when nothing is trump, `suit-led-cards` cannot be,
;; since, by assumption, at least one card was played.
(solves
  (fn [trump]
    (fn [cards]
      (let [suit-led (->> cards first :suit)
            get-cards (fn [suit] (->> cards
                                      (filter #(= (:suit %) suit))
                                      (sort-by :rank)
                                      reverse))
            trump-cards (get-cards trump)
            suit-led-cards (get-cards suit-led)]
        (first (concat trump-cards suit-led-cards)))))

  (let [notrump (__ nil)]
    (and (= {:suit :club :rank 9}  (notrump [{:suit :club :rank 4}
                                             {:suit :club :rank 9}]))
         (= {:suit :spade :rank 2} (notrump [{:suit :spade :rank 2}
                                             {:suit :club :rank 10}]))))
  (= {:suit :club :rank 10} ((__ :club) [{:suit :spade :rank 2}
                                         {:suit :club :rank 10}]))
  (= {:suit :heart :rank 8}
     ((__ :heart) [{:suit :heart :rank 6} {:suit :heart :rank 8}
                   {:suit :diamond :rank 10} {:suit :heart :rank 4}])))



;; ### Problem 150: <a href="http://www.4clojure.com/problem/150">Palindromic Numbers</a>
;;
;; My solution has three parts: (a) to calculate the
;; `nearest-palindrome` to a given number `x` (which will be `x` if it
;; is already a palindromic number); (b) to calculate the
;; `next-palindrome` given an already palindromic number; and finally
;; (c) to iterate `next-palindrome` to provide an infinite lazy
;; sequence of palindromes larger than `n`.  Because
;; `nearest-palindrome` can provide a result less than `x`, we use
;; `drop-while` in the final sequence to choose only values >= `n`.
;;
;; For both `nearest-palindrome` and `next-palindrome`, the approach
;; is to split the number into left and right halves based on its
;; length in digits (the `rl-pair` function); the left half is then
;; reflected and concatenated onto itself using `recombine` (e.g.,
;; 1234 -> 1221).  The twist in `next-palindrome` is to increment the
;; left half of the number before recombining.
(solves

  (fn [n]
    (letfn [(rl-pair [n]
              [(quot (inc n) 2), (quot n 2)])

            (recombine [s k1 k2]
              (Long/parseLong (apply str (concat (take k1 s)
                                                 (reverse (take k2 s))))))
            (nearest-palindrome [x]
              (let [sx (str x)
                    [k1 k2] (rl-pair (count sx))]
                (recombine sx k1 k2)))

            (next-palindrome [x]
              (let [sx (str x)
                    [k1 _] (rl-pair (count sx))
                    x' (inc (Long/parseLong (apply str (take k1 sx))))
                    [kk1 kk2] (rl-pair (count (str (inc x))))]
                (recombine (str x') kk1 kk2)))]

      (drop-while #(< % n) (iterate next-palindrome
                                    (nearest-palindrome n)))))

  (= (take 26 (__ 0))
     [0 1 2 3 4 5 6 7 8 9
      11 22 33 44 55 66 77 88 99
      101 111 121 131 141 151 161])
  (= (take 16 (__ 162))
     [171 181 191 202
      212 222 232 242
      252 262 272 282
      292 303 313 323])
  (= (take 6 (__ 1234550000))
     [1234554321 1234664321 1234774321
      1234884321 1234994321 1235005321])
  (= (first (__ (* 111111111 111111111)))
     (* 111111111 111111111))
  (= (set (take 199 (__ 0)))
     (set (map #(first (__ %)) (range 0 10000))))
  (= true
     (apply < (take 6666 (__ 9999999))))
  (= (nth (__ 0) 10101)
     9102019))


;; Problem 177:
(solves (fn [s]
          (empty?
           (loop [s (clojure.string/replace s #"[^\(\)\{\}\[\]]" "")]
             (let [r (clojure.string/replace s #"\(\)|\{\}|\[\]" "")]
               (if (= r s) s (recur r))))))
  (__ "This string has no brackets.")
  (__ "class Test {
      public static void main(String[] args) {
        System.out.println(\"Hello world.\");
      }
    }")
  (not (__ "(start, end]"))
  (not (__ "())"))
  (not (__ "[ { ] } "))
  (__ "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))")
  (not (__ "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))"))
  (not (__ "[")))

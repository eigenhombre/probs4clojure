(ns probs4clojure.core-test
  (:require [probs4clojure.test-util :refer [problem]]))

;; ### Solutions to 4clojure.com problems
;;
;; Worked and (towards the end, at least) documented with
;; [Marginalia](http://gdeer81.github.io/marginalia/).
;;
;; **Spoiler Alert** -- at least *try* to solve the problems yourself
;; first!!!  They are much better that way.


;; <script type="text/javascript"
;;  src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
;; </script>


(problem 1
  true (= __ true))


(problem 2
  4
  (= (- 10 (* 2 3)) __))


(problem 3
  "HELLO WORLD"
  (= __ (.toUpperCase "hello world")))


;; This is the one problem not amenable to `problem` off-the-shelf, so
;; solution is added inline.
(problem 4
 (= (list :a :b :c) '(:a :b :c)))


(problem 5
  '(1 2 3 4)
  (= __ (conj '(2 3 4) 1))
  (= __ (conj '(3 4) 2 1)))


(problem 6
  [:a :b :c]
  (= (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c)))


(problem 7
  [1 2 3 4]
  (= __ (conj [1 2 3] 4))
  (= __ (conj [1 2] 3 4)))


(problem 8
  #{:a :b :c :d}
  (= __ (set '(:a :a :b :c :c :c :c :d :d)))
  (= __ (clojure.set/union #{:a :b :c} #{:b :c :d})))


(problem 9
  2
  (= #{1 2 3 4} (conj #{1 4 3} __)))


(problem 10
  20
  (= __ ((hash-map :a 10, :b 20, :c 30) :b))
  (= __ (:b {:a 10, :b 20, :c 30})))


(problem 11
  [:b 2]
  (= {:a 1, :b 2, :c 3} (conj {:a 1} __ [:c 3])))


(problem 12
  3
  (= __ (first '(3 2 1)))
  (= __ (second [2 3 4]))
  (= __ (last (list 1 2 3))))


(problem 13
  [20 30 40]
  (= __ (rest [10 20 30 40])))


(problem 14
  8
  (= __ ((fn add-five [x] (+ x 5)) 3))
  (= __ ((fn [x] (+ x 5)) 3))
  (= __ (#(+ % 5) 3))
  (= __ ((partial + 5) 3)))


(problem 15
  #(* 2 %)
  (= (__ 2) 4)
  (= (__ 3) 6)
  (= (__ 11) 22)
  (= (__ 7) 14))


(problem 16
  #(format "Hello, %s!" %)
  (= (__ "Dave") "Hello, Dave!")
  (= (__ "Jenn") "Hello, Jenn!")
  (= (__ "Rhea") "Hello, Rhea!"))


(problem 17
  [6 7 8]
  (= __ (map #(+ % 5) '(1 2 3))))


(problem 18
  [6 7]
  (= __ (filter #(> % 5) '(3 4 5 6 7))))


(problem 19
  #(first (reverse %))
  (= (__ [1 2 3 4 5]) 5)
  (= (__ '(5 4 3)) 3)
  (= (__ ["b" "c" "d"]) "d"))


(problem 20
  #(second (reverse %))
  (= (__ (list 1 2 3 4 5)) 4)
  (= (__ ["a" "b" "c"]) "b")
  (= (__ [[1 2] [3 4]]) [1 2]))


(problem 21
  (fn [s n] (first (drop n s)))
  (= (__ '(4 5 6 7) 2) 6)
  (= (__ [:a :b :c] 0) :a)
  (= (__ [1 2 3 4] 1) 2)
  (= (__ '([1 2] [3 4] [5 6]) 2) [5 6]))


(problem 22
  #(apply + (map (fn [_] 1) %))
  (= (__ '(1 2 3 3 1)) 5)
  (= (__ "Hello World") 11)
  (= (__ [[1 2] [3 4] [5 6]]) 3)
  (= (__ '(13)) 1)
  (= (__ '(:a :b :c)) 3))


(problem 23
  (fn [s]
    (let [svec (into [] s)]
      (for [i (range (count svec) 0 -1)]
        (get svec (dec i)))))
  (= (__ [1 2 3 4 5]) [5 4 3 2 1])
  (= (__ (sorted-set 5 7 2 7)) '(7 5 2))
  (= (__ [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]]))


(problem 24
  (fn [s] (apply + s))
  (= (__ [1 2 3]) 6)
  (= (__ (list 0 -2 5 5)) 8)
  (= (__ #{4 2 1}) 7)
  (= (__ '(0 0 -1)) -1)
  (= (__ '(1 10 3)) 14))


(problem 25
  #(filter odd? %)
  (= (__ #{1 2 3 4 5}) '(1 3 5))
  (= (__ [4 2 1 6]) '(1))
  (= (__ [2 2 4 6]) '())
  (= (__ [1 1 1 3]) '(1 1 1 3)))


(problem 26
  (fn [n]
    (take n (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1]))))
  (= (__ 3) '(1 1 2))
  (= (__ 6) '(1 1 2 3 5 8))
  (= (__ 8) '(1 1 2 3 5 8 13 21)))


(problem 27
  (fn [coll]
    (= (seq coll)
       (reverse coll)))
  (false? (__ '(1 2 3 4 5)))
  (true? (__ "racecar"))
  (true? (__ [:foo :bar :foo]))
  (true? (__ '(1 1 3 3 1 1)))
  (false? (__ '(:a :b :c))))


(problem 28
  (fn flat [[h & t :as l]]
    (when (seq l)
      (if (coll? h)
        (concat (flat h)
                (flat t))
        (cons h (flat t)))))
  (= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
  (= (__ ["a" ["b"] "c"]) '("a" "b" "c"))
  (= (__ '((((:a))))) '(:a)))


(problem 29
  (fn [s] (apply str (filter #(. Character isUpperCase %) s)))
  (= (__ "HeLlO, WoRlD!") "HLOWRD")
  (empty? (__ "nothing"))
  (= (__ "$#A(*&987Zf") "AZ"))


(problem 30
  #(->> %
        (partition-by identity)
        (map first))
  (= (apply str (__ "Leeeeeerrroyyy")) "Leroy")
  (= (__ [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
  (= (__ [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2])))


(problem 31
  (partial partition-by identity)
  (= (__ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
  (= (__ [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
  (= (__ [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4]))))


(problem 32
  (partial mapcat (fn [x] [x x]))
  (= (__ [1 2 3]) '(1 1 2 2 3 3))
  (= (__ [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
  (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
  (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))


(problem 33
  (fn [s n] (mapcat #(repeat n %) s))
  (= (__ [1 2 3] 2) '(1 1 2 2 3 3))
  (= (__ [:a :b] 4) '(:a :a :a :a :b :b :b :b))
  (= (__ [4 5 6] 1) '(4 5 6))
  (= (__ [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
  (= (__ [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))


(problem 34
  (fn [s e] (take (- e s) (iterate inc s)))
  (= (__ 1 4) '(1 2 3))
  (= (__ -2 2) '(-2 -1 0 1))
  (= (__ 5 8) '(5 6 7)))


(problem 35
  7
  (= __ (let [x 5] (+ 2 x)))
  (= __ (let [x 3, y 10] (- y x)))
  (= __ (let [x 21] (let [y 3] (/ x y)))))


(problem 36
  [x 7, y 3, z 1]
  (= 10 (let __ (+ x y)))
  (= 4 (let __ (+ y z)))
  (= 1 (let __ z)))


(problem 37
  "ABC"
  (= __ (apply str (re-seq #"[A-Z]+" "bA1B3Ce "))))


(problem 38
  (fn [& s] (reduce (fn [a b] (if (> a b) a b)) s))
  (= (__ 1 8 3 4) 8)
  (= (__ 30 20) 30)
  (= (__ 45 67 11) 67))


(problem 39
  (partial mapcat vector)
  (= (__ [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
  (= (__ [1 2] [3 4 5 6]) '(1 3 2 4))
  (= (__ [1 2 3 4] [5]) [1 5])
  (= (__ [30 20] [25 15]) [30 25 20 15]))


(problem 40
  (fn [d x] (rest (mapcat (fn [xx] [d xx]) x)))
  (= (__ 0 [1 2 3]) [1 0 2 0 3])
  (= (apply str (__ ", " ["one" "two" "three"])) "one, two, three")
  (= (__ :z [:a :b :c :d]) [:a :z :b :z :c :z :d]))


(problem 41
  (fn [s n] (mapcat #(if (= (count %) n) (drop-last %) %)
                    (partition-all n s)))
  (= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
  (= (__ [:a :b :c :d :e :f] 2) [:a :c :e])
  (= (__ [1 2 3 4 5 6] 4) [1 2 3 5 6]))


(problem 42
  #(apply *' (range 1 (inc %)))
  (= (__ 1) 1)
  (= (__ 3) 6)
  (= (__ 5) 120)
  (= (__ 8) 40320))


(problem 43
  (fn [s n]
    (map (fn [i]
           (map (fn [ss] (nth ss i))
                (partition n s)))
         (range n)))
  (= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
  (= (__ (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
  (= (__ (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9))))


(problem 44
  (fn [pos s]
    (map (fn [i] (nth s (mod (+ pos i) (count s))))
         (range (count s))))
  (= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
  (= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
  (= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1))
  (= (__ 1 '(:a :b :c)) '(:b :c :a))
  (= (__ -4 '(:a :b :c)) '(:c :a :b)))


(problem 45
  [1 4 7 10 13]
  (= __ (take 5 (iterate #(+ 3 %) 1))))


(problem 46
  (fn [f] (fn [& args] (apply f (reverse args))))
  (= 3 ((__ nth) 2 [1 2 3 4 5]))
  (= true ((__ >) 7 8))
  (= 4 ((__ quot) 2 8))
  (= [1 2 3] ((__ take) [1 2 3 4 5] 3)))


(problem 47
  4
  (contains? #{4 5 6} __)
  (contains? [1 1 1 1 1] __)
  (contains? {4 :a 2 :b} __)
  #_(not (contains? '(1 2 4) __)))  ;; Lists not supported in 1.6.


(problem 48
  6
  (= __ (some #{2 7 6} [5 6 7 8]))
  (= __ (some #(when (even? %) %) [5 6 7 8])))


(problem 49
  (fn [n s] (list (take n s) (drop n s)))
  (= (__ 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
  (= (__ 1 [:a :b :c :d]) [[:a] [:b :c :d]])
  (= (__ 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]]))


(problem 50
  #(->> %
        (group-by type)
        vals)
  (= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
  (= (set (__ [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
  (= (set (__ [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]}))


(problem 51
  [1 2 3 4 5]
  (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] __] [a b c d])))


(problem 52
  [c e]
  (= [2 4] (let [[a b c d e f g] (range)] __)))


(problem 53
  (fn [s] (let [deltas (map (fn [[a b]] [(- b a) a b]) (partition 2 1 s))
                series (partition-by first deltas)
                longest-length (->> series
                                    (group-by count)
                                    keys
                                    (apply max))
                desired-sequence (->> series
                                      (filter #(= (count %)
                                                  longest-length))
                                      first)
                start (second (first desired-sequence))
                end (int (nth (last desired-sequence) 2))]
            (range start (inc end))))
  (= (__ [1 0 1 2 3 0 4 5]) [0 1 2 3])
  (= (__ [5 6 1 3 2 7]) [5 6])
  (= (__ [2 3 3 4 5]) [3 4 5])
  (= (__ [7 6 5 4]) []))


(problem 54
  (fn f [c s]
    (lazy-seq
     (let [nxt (take c s)]
       (when (and (seq s) (= (count nxt) c))
         (cons (take c s) (f c (drop c s)))))))
  (= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
  (= (__ 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
  (= (__ 3 (range 8)) '((0 1 2) (3 4 5))))


(problem 55
  #(->> %
        (group-by identity)
        (map (fn [[k v]] [k (count v)]))
        (into {}))
  (= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
  (= (__ [:b :a :b :a :b]) {:a 2, :b 3})
  (= (__ '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2}))


(problem 56
  (fn [s]
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


(problem 57
  [5 4 3 2 1]
  (= __ ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5)))


(problem 58
  (fn outer [f & fs]
    (if fs
      (fn [& x] (f (apply (apply outer fs) x)))
      (fn [& xs] (apply f xs))))
  (= [3 2 1] ((__ rest reverse) [1 2 3 4]))
  (= 5 ((__ (partial + 3) second) [1 2 3 4]))
  (= true ((__ zero? #(mod % 8) +) 3 5 7 9))
  (= "HELLO" ((__ #(.toUpperCase %) #(apply str %) take) 5 "hello world")))


(problem 59
  (fn [& fns]
    (fn [& xs]
      (for [f fns] (apply f xs))))
  (= [21 6 1] ((__ + max min) 2 3 5 1 6 4))
  (= ["HELLO" 5] ((__ #(.toUpperCase %) count) "hello"))
  (= [2 6 4] ((__ :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})))


(problem 60
  (fn reduxions
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


(problem 61
  #(into {} (map vector %1 %2))
  (= (__ [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
  (= (__ [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"})
  (= (__ [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"}))


(problem 62
  (fn itr [f x]
    (lazy-seq
     (cons x (itr f (f x)))))
  (= (take 5 (__ #(* 2 %) 1)) [1 2 4 8 16])
  (= (take 100 (__ inc 0)) (take 100 (range)))
  (= (take 9 (__ #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3]))))


(problem 63
  (fn [pred s]
    (->> s
         (map (fn [x] {(pred x) [x]}))
         (apply (partial merge-with concat))))
  (= (__ #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})
  (= (__ #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
     {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})
  (= (__ count [[1] [1 2] [3] [1 2 3] [2 3]])
     {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]}))


(problem 64
  +
  (= 15 (reduce __ [1 2 3 4 5]))
  (=  0 (reduce __ []))
  (=  6 (reduce __ 1 [2 3])))


(problem 65
  (fn [s]
    (let [e (empty s)]
      (if (identical? '() e)
        :list
        ({#{} :set, {} :map, [] :vector} e))))
  (= :map (__ {:a 1, :b 2}))
  (= :list (__ (range (rand-int 20))))
  (= :vector (__ [1 2 3 4 5 6]))
  (= :set (__ #{10 (rand-int 5)}))
  (= [:map :set :vector :list] (map __ [{} #{} [] ()])))


(problem 66
  (fn [a b]
    (loop [a a, b b]
      (cond
        (= a b) a
        (> a b) (recur (- a b) b)
        :else (recur a (- b a)))))
  (= (__ 2 4) 2)
  (= (__ 10 5) 5)
  (= (__ 5 7) 1)
  (= (__ 1023 858) 33))


(problem 67
  #(take %
         (remove
          (fn [n]
            (some (fn [x] (= (rem n x) 0))
                  (range 2 n)))
          (drop 2 (range))))
  (= (__ 2) [2 3])
  (= (__ 5) [2 3 5 7 11])
  (= (last (__ 100)) 541))


(problem 68
  [7 6 5 4 3]
  (= __
     (loop [x 5
            result []]
       (if (> x 0)
         (recur (dec x) (conj result (+ 2 x)))
         result))))


(problem 69
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


(problem 70
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


(problem 71
  last
  (= (__ (sort (rest (reverse [2 5 4 1 3 6]))))
     (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (__))
     5))


(problem 72
  (partial reduce +)
  (= (__ (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
     (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (__))
     11))


(problem 73
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


(problem 74
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


(problem 75
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


(problem 76
  (= [1 3 5 7 9 11]
     (letfn
         [(foo [x y] #(bar (conj x y) y))
          (bar [x y] (if (> (last x) 10)
                       x
                       #(foo x (+ 2 y))))]
       (trampoline foo [] 1))))


(problem 77
  (fn [words]
    (->>
     (map (fn [w] (set (filter #(= (sort w) (sort %)) words))) words)
     (remove #(= (count %) 1))
     set))
  (= (__ ["meat" "mat" "team" "mate" "eat"])
     #{#{"meat" "team" "mate"}})
  (= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
     #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}}))


(problem 78
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


(problem 79
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


(problem 80
  (fn [n]
    (let [subs (filter #(zero? (rem n %)) (range 1 n))
          sum (apply + subs)]
      (= sum n)))
  (= (__ 6) true)
  (= (__ 7) false)
  (= (__ 496) true)
  (= (__ 500) false)
  (= (__ 8128) true))


(problem 81
  (fn [a b]
    (set (filter #(and (a %) (b %)) (set (concat a b)))))
  (= (__ #{0 1 2 3} #{2 3 4 5}) #{2 3})
  (= (__ #{0 1 2} #{3 4 5}) #{})
  (= (__ #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d}))


(problem 82
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


(problem 83
  (fn [& bs]
    (boolean (and (some true? bs)
                  (not (every? true? bs)))))
  (= false (__ false false))
  (= true (__ true false))
  (= false (__ true))
  (= true (__ false true false))
  (= false (__ true true true))
  (= true (__ true true true false)))


;; ### Problem 84: <a href="http://www.4clojure.com/problem/84">Transitive Closure</a>
;;
;; Abstract examples: if \\([a, b]\\) and \\([b, c]\\) are given, then
;; \\([a, c]\\) should be added to the inputs in the result.  If
;; \\([a, b]\\), \\([b, c]\\) and \\([c, d]\\) are given, then \\([a,
;; c]\\), \\([b, d]\\) and \\([a, d]\\) should be added to the inputs.
;;
;; I took a fairly brute force approach to this.  `grow-1` is a single
;; step forward where, for any of the pairs in the input, I find all
;; other pairs in the input whose first element equals the second
;; element of the original pair; `grow-1` concats these new pairs with the
;; original collection of pairs in the input.
;;
;; `grow` iterates on `grow-1` until no change is found; i.e., the
;; resulting set is maximal.
(problem 84
  (fn [s]
    (letfn [(grow-1 [pairs]
              (let [pairs (vec pairs)
                    pairs' (for [[a b] pairs
                                 [bb cc] pairs :when (= b bb)]
                             [a cc])]
                (set (concat pairs pairs'))))
            (grow [pairs]
              (let [pairs' (grow-1 pairs)]
                (if (= pairs' pairs)
                  (set pairs)
                  (recur pairs'))))]
      (grow s)))
  (let [divides #{[8 4] [9 3] [4 2] [27 9]}]
    (= (__ divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]}))
  (let [more-legs
        #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}]
    (= (__ more-legs)
       #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
         ["spider" "cat"] ["spider" "man"] ["spider" "snake"]}))
  (let [progeny
        #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
    (= (__ progeny)
     #{["father" "son"] ["father" "grandson"]
       ["uncle" "cousin"] ["son" "grandson"]})))


;; ### Problem 85: <a href="http://www.4clojure.com/problem/85">Power Set</a>
;; The problem has a recursive solution, which becomes clearer when one realizes
;; that, for every subset in the power set, any given object is
;; either in that subset or not, and that both options (object
;; in, and object not in) are included for each subset.
;; In other words, for a given
;; \\(x\\) in a set \\(S\\), if \\(Q\\) is the power set of \\(S\\) when
;; \\(x\\) is removed, and \\(Q' = \\{\\{x\\} \cup q\\)
;; for all \\(q \in Q\\}\\), then \\(P\\), the power set of \\(S\\), is
;; \\(Q \cup Q'\\).
(problem 85
  (fn pwr [xs]
    (let [[x & more :as xs] (vec xs)]
      (if-not (seq xs)
        #{#{}}
        (->> (pwr more)
             (concat (map #(conj % x) (pwr more)))
             set))))

  (= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
  (= (__ #{}) #{#{}})
  (= (__ #{1 2 3})
     #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
  (= (count (__ (into #{} (range 10)))) 1024))


;; ### Problem 86: <a href="http://www.4clojure.com/problem/86">Happy Numbers</a>
;;
;; Here `f` implements the "particular formula" described in the
;; problem text; we iterate until the result is either 1 (happy), or a
;; repeated value is found again (sad).  To check the latter we
;; maintain a dictionary `seen` of previous values.
(problem 86
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


;; ### Problem 87 does not exist


;; ### Problem 88: <a href="http://www.4clojure.com/problem/88">Symmetric Difference</a>
;;
;; The problem is simple if you use the `clojure.set` library and the
;; `remove` function to eliminate the intersection elements from both
;; sets before combining them.
(problem 88
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
(problem 89
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
(problem 90
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


;; ### Problem 91: <a href="http://www.4clojure.com/problem/91">Graph Connectivity</a>
;;
;; We already determined connectedness in Problem 89.  The only
;; required variation is to coerce the input set to a vector so the
;; initial loop destructuring form works.
(problem 91
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
                (every? (partial = true) (map :explored (vals exp)))))]

      (->> pairs
           (into [])
           pairs-to-neighbor-list-map
           all-connected)))

  (= true (__ #{[:a :a]}))
  (= true (__ #{[:a :b]}))
  (= false (__ #{[1 2] [2 3] [3 1]
               [4 5] [5 6] [6 4]}))
  (= true (__ #{[1 2] [2 3] [3 1]
              [4 5] [5 6] [6 4] [3 4]}))
  (= false (__ #{[:a :b] [:b :c] [:c :d]
               [:x :y] [:d :a] [:b :e]}))
  (= true (__ #{[:a :b] [:b :c] [:c :d]
              [:x :y] [:d :a] [:b :e] [:x :a]})))


;; ### Problem 92: <a href="http://www.4clojure.com/problem/92">Read Roman Numerals</a>
;;
;; We find the allowed number patterns with their numerical values,
;; searching for the subtractive patterns (e.g. IV) first to keep them
;; distinct from the non-subptractive ones (e.g. VII).  We thread the
;; current string and an accumulator value through each of the
;; possible patterns, taking the final accumulator value at the end.
(problem 92
  (fn [s]
    (let [pn
          (fn [[s acc] pat incr]
            (loop [s s, acc acc]
              (if-not (re-find pat s)
                [s acc]
                (recur (clojure.string/replace-first s pat "") (+ acc incr)))))]
      (-> [s 0]
          (pn #"CM" 900)
          (pn #"CD" 400)
          (pn #"XC" 90)
          (pn #"XL" 40)
          (pn #"IX" 9)
          (pn #"IV" 4)
          (pn #"M" 1000)
          (pn #"D" 500)
          (pn #"C" 100)
          (pn #"L" 50)
          (pn #"X" 10)
          (pn #"V" 5)
          (pn #"I" 1)
          last)))

  (= 14 (__ "XIV"))
  (= 3999 (__ "MMMCMXCIX"))
  (= 827 (__ "DCCCXXVII"))
  (= 48 (__ "XLVIII")))


;; ### Problem 93: <a href="http://www.4clojure.com/problem/93">Partially Flatten a Sequence</a>
;;
;; I solved this by writing a simple, recursive `flatten` first, and
;; then adding the extra check `(coll? (first x))`.
(problem 93
  (fn flat [s]
    (if-not (coll? s)
      s
      (let [[x & xs] s]
        (if (and (coll? x) (coll? (first x)))
          (concat (flat x) (flat xs))
          (cons x (flat xs))))))

  (= (__ [["Do"] ["Nothing"]])
     [["Do"] ["Nothing"]])
  (= (__ [[[[:a :b]]] [[:c :d]] [:e :f]])
     [[:a :b] [:c :d] [:e :f]])
  (= (__ '((1 2)((3 4)((((5 6)))))))
     '((1 2)(3 4)(5 6))))

;; ### Problem 94: <a href="http://www.4clojure.com/problem/94">Game of Life</a>
;;
;; We accept a vector of strings as our "board representation."  The
;; problem is, essentially, For any given spot, how many neighbors do
;; I have?  This is made easier by the fact that we can easily address any
;; cell in the board representation with `get-in`.  For example, if `b` is the
;; first board, `(get-in b [2 2])` is `\#`, `(get-in b [0 0])` is
;; `\space`, and `(get-in b [-1 -5])` is `nil`.
;;
;; With the lookup method understood, the rest of the work is looping
;; over all the rows and columns, then for each position count
;; neighbor values by looking at the character at +/- one x or y
;; value, except for the position in question.  Then the rule for
;; whether the cell should live or die is trivial to apply.
(problem 94
  (fn [b]
    (for [j (range (count (first b)))]
      (let [row-chars (for [i (range (count b))]
                        (let [nn (count
                                  (for [ny (range (dec j) (+ 2 j))
                                        nx (range (dec i) (+ 2 i))
                                        :when (and (= \# (get-in b [ny nx]))
                                                   (or (not= ny j)
                                                       (not= nx i)))]
                                    [ny nx (= \# (get-in b [ny nx]))]))
                              reborn (if (= \# (get-in b [j i]))
                                       (#{2 3} nn)
                                       (#{3} nn))]
                          (if reborn \# \space)))]
        (apply str row-chars))))
  (= (__ ["      "
          " ##   "
          " ##   "
          "   ## "
          "   ## "
          "      "])
     ["      "
      " ##   "
      " #    "
      "    # "
      "   ## "
      "      "])
  (= (__ ["     "
          "     "
          " ### "
          "     "
          "     "])
     ["     "
      "  #  "
      "  #  "
      "  #  "
      "     "])
  (= (__ ["      "
          "      "
          "  ### "
          " ###  "
          "      "
          "      "])
     ["      "
      "   #  "
      " #  # "
      " #  # "
      "  #   "
      "      "]))


;; ### Problem 95: <a href="http://www.4clojure.com/problem/95">To Tree, or not to Tree</a>
;;
;; Solve recursively, with base cases to catch the wrong shape at any
;; given node.
(problem 95
  (fn treep [s]
    (or (nil? s)
        (and (coll? s)
             (= (count s) 3)
             (treep (nth s 1))
             (treep (nth s 2)))))

  (= (__ '(:a (:b nil nil) nil))
     true)
  (= (__ '(:a (:b nil nil)))
     false)
  (= (__ [1 nil [2 [3 nil nil] [4 nil nil]]])
     true)
  (= (__ [1 [2 nil nil] [3 nil nil] [4 nil nil]])
     false)
  (= (__ [1 [2 [3 [4 nil nil] nil] nil] nil])
     true)
  (= (__ [1 [2 [3 [4 false nil] nil] nil] nil])
     false)
  (= (__ '(:a nil ()))
     false))


;; ### Problem 96: <a href="http://www.4clojure.com/problem/96">Beauty is Symmetry</a>
;;
;; The solution is to make a transpose function which recursively
;; swaps the left and right parts of the tree.
(problem 96
  (fn [[v l r :as t]]
    (let [trans (fn tr [[v l r :as tt]]
                  (when tt
                    [v (tr r) (tr l)]))]
      (= l (trans r))))

  (= (__ '(:a (:b nil nil) (:b nil nil))) true)
  (= (__ '(:a (:b nil nil) nil)) false)
  (= (__ '(:a (:b nil nil) (:c nil nil))) false)
  (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
     true)
  (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
     false)
  (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] nil]] nil]])
     false))


;; ### Problem 97: <a href="http://www.4clojure.com/problem/97">Pascal's Triangle</a>
;;
;; I solved this problem by thinking about the following series of
;; data transformations on an arbitrary level of the problem:
;;
;;     [1     3     3     1]  ;; Given level
;;       [1 3] [3 3] [3 1]    ;; Partition by pairs
;;        [4     6     4]     ;; Map sum onto pairs
;;          [1 4 6 4 1]       ;; Prepend and append 1s
;;
;; The application of the transformations gets us to the next "level"
;; of the problem.  The code reflects this exactly, by iterating the
;; transformations on the base case, `[1]`.
(problem 97
 (fn [n]
   (nth (iterate (fn [s]
                   (let [p (partition 2 1 s)
                         added (map (partial apply +) p)]
                     (concat [1] added [1])))
                 [1])
        (dec n)))

 (= (__ 1) [1])
 (= (map __ (range 1 6))
   [     [1]
        [1 1]
       [1 2 1]
      [1 3 3 1]
     [1 4 6 4 1]])
 (= (__ 11)
    [1 10 45 120 210 252 210 120 45 10 1]))


;; ### Problem 98: <a href="http://www.4clojure.com/problem/98">Equivalence Classes</a>
(problem 98
  (fn equiv
    [f D]
    (set
     (for [a D]
       (set
        (for [b D :when (= (f a) (f b))]
          b)))))
  (= (__ #(* % %) #{-2 -1 0 1 2})
     #{#{0} #{1 -1} #{2 -2}})
  (= (__ #(rem % 3) #{0 1 2 3 4 5 })
     #{#{0 3} #{1 4} #{2 5}})
  (= (__ identity #{0 1 2 3 4})
     #{#{0} #{1} #{2} #{3} #{4}})
  (= (__ (constantly true) #{0 1 2 3 4})
     #{#{0 1 2 3 4}}))


;; ### Problem 99: <a href="http://www.4clojure.com/problem/99">Product Digits</a>
(problem 99
  (fn [a b]
    (->> a
         (* b)
         str
         (map (comp #(Integer. %) str))))
  (= (__ 1 1) [1])
  (= (__ 1 1) [1])
  (= (__ 99 9) [8 9 1])
  (= (__ 999 99) [9 8 9 0 1]))


;; ### Problem 100: <a href="http://www.4clojure.com/problem/100">Least Common Multiple</a>
(problem 100
  (fn dolcm [& r]
    (letfn [(lcm [a & rst]
              (/ (apply * a rst)
                 (reduce gcd1 a rst)))
            (gcd1 [a b]
              (let [aa (max a b)
                    bb (min a b)]
                (if (zero? b)
                  a
                  (recur b (mod a b)))))]
      (apply lcm r)))
  (== (__ 2 3) 6)
  (== (__ 5 3 7) 105)
  (== (__ 1/3 2/5) 2)
  (== (__ 3/4 1/6) 3/2)
  (== (__ 7 5/7 2 3/5) 210))


;; ### Problem 101: <a href="http://www.4clojure.com/problem/101">Levenshtein Distance</a>
;;
;; For this solution, we build on the simplest recursive solution
;; shown in the [relevant Wikipedia
;; article](http://en.wikipedia.org/wiki/Levenshtein_distance#Recursive).
;; The caveat given there is that the recursive solution "is very
;; inefficient because it recomputes the Levenshtein distance of the
;; same substrings many times."
;;
;; In this sense, the algorithm shares characteristics with the
;; Fibonacci sequence calculation, whose elegant but slow recursive
;; solution can be dramatically sped up using memoization.
;; Memoization is a bit trickier in cases such as this when `def`ing
;; new vars is not allowed (as is the case with all 4Clojure
;; problems).  We therefore note [Rafał Dowgird's StackOverflow
;; solution](http://stackoverflow.com/questions/3906831/how-do-i-generate-memoized-recursive-functions-in-clojure)
;; for memoizing without `def`.  Memoizing this way speeds up the test
;; suite from several minutes or more (I didn't bother waiting for it
;; to finish) to about 25 msec on my laptop.
(problem 101
  (let [ldist
        (fn [mem-ldist a b]
          (cond
            (empty? a) (count b)
            (empty? b) (count a)
            :else (min (inc (mem-ldist mem-ldist (butlast a) b))
                       (inc (mem-ldist mem-ldist a (butlast b)))
                       (+ (mem-ldist mem-ldist (butlast a) (butlast b))
                          (if (= (last a) (last b)) 0 1)))))
        mem-ldist (memoize ldist)]
    (partial mem-ldist mem-ldist))

  (= (__ "kitten" "sitting") 3)
  (= (__ "closure" "clojure") (__ "clojure" "closure") 1)
  (= (__ "xyx" "xyyyx") 2)
  (= (__ "" "123456") 6)
  (= (__ "Clojure" "Clojure") (__ "" "") (__ [] []) 0)
  (= (__ [1 2 3 4] [0 2 3 4 5]) 2)
  (= (__ '(:a :b :c :d) '(:a :d)) 2)
  (= (__ "ttttattttctg" "tcaaccctaccat") 10)
  (= (__ "gaattctaatctc" "caaacaaaaaattt") 9))


;; ### Problem 102: <a href="http://www.4clojure.com/problem/102">102intoCamelCase</a>
(problem 102
  #(let [[f & rst] (clojure.string/split % #"-")]
     (apply str f (map clojure.string/capitalize rst)))
  (= (__ "something") "something")
  (= (__ "multi-word-key") "multiWordKey")
  (= (__ "leaveMeAlone") "leaveMeAlone"))


;; ### Problem 103: <a href="http://www.4clojure.com/problem/103">Generating k-combinations</a>
;;
;; Note: these two solutions both scale rather badly.  Benchmark with, e.g.,:

(comment (doseq [i (range 12)]
           (print i "...")
           (time (println (f i (range i))))))


;; Old solution:
(comment (fn [k s]
           (set
            (filter #(= (count %) k)
                    (loop [n k, ret (map hash-set s)]
                      (if (zero? n)
                        ret
                        (recur (dec n)
                               (for [v ret, x s]
                                 (conj v x)))))))))


;; New solution.  Note that Mark Engelberg's [math.combinatorics
;; implementation](https://github.com/clojure/math.combinatorics/blob/master/src/main/clojure/clojure/math/combinatorics.clj)
;; relies heavily on Knuth, and is less straightforward, but has much
;; better time complexity than my solution.
(problem 103
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
  (= (__ 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a}
                                        #{[1 2 3] "abc"}
                                        #{[1 2 3] "efg"}
                                        #{:a "abc"} #{:a "efg"}
                                        #{"abc" "efg"}}))


;; ### Problem 104: <a href="http://www.4clojure.com/problem/104">Write Roman Numerals</a>
(problem 104
  (fn R [i]
    (let [romans (into (sorted-map-by >)
                       {1000 "M"
                        900  "CM"
                        500  "D"
                        400  "CD"
                        100  "C"
                        90   "XC"
                        50   "L"
                        40   "XL"
                        10   "X"
                        9    "IX"
                        5    "V"
                        4    "IV"
                        1    "I"})
          allchars (loop [remain i, charlist [], romans romans, ret []]
                     (let [[n char] (first romans)
                           x (quot remain n)
                           nx-chars (concat charlist (repeat x char))]
                       (if (seq (rest romans))
                         (recur (- remain (* x n))
                                nx-chars
                                (rest romans)
                                (concat ret nx-chars))
                         nx-chars)))]
      (apply str (concat allchars))))
  (= "I" (__ 1))
  (= "XXX" (__ 30))
  (= "IV" (__ 4))
  (= "CXL" (__ 140))
  (= "DCCCXXVII" (__ 827))
  (= "MMMCMXCIX" (__ 3999))
  (= "XLVIII" (__ 48)))


;; ### Problem 105: <a href="http://www.4clojure.com/problem/105">Identify keys and values</a>
(problem 105
  (fn unflatten-map [s]
    (loop [ret {}
           prev-kw []
           s s]
      (let [f (first s)
            kw? (keyword? f)
            num? (not kw?)
            this-kw (if kw? f prev-kw)
            prev-list (vec (ret this-kw))
            next-value (vec (concat (ret this-kw) (if num? [f] [])))
            next-map (assoc ret this-kw (if num? next-value []))]
        (if (seq s)
          (recur next-map this-kw (rest s))
          ret))))
  (= {} (__ []))
  (= {:a [1]} (__ [:a 1]))
  (= {:a [1], :b [2]} (__ [:a 1, :b 2]))
  (= {:a [1 2 3], :b [], :c [4]} (__ [:a 1 2 3 :b :c 4])))


;; ### Problem 106: <a href="http://www.4clojure.com/problem/106">Number Maze</a>
;;
;; This problem is similar to searching for the shortest path through
;; an undirected graph, a problem for which breadth-first search is
;; well-suited.  For every "layer" in the search (each layer
;; corresponds to a successive number of allowed operations) we
;; calculate the next layer by applying all the allowed operations to
;; the values in the current layer.  The layers can grow large
;; relatively quickly, but converting the sequence to a set on each
;; iteration reduces the size substantially for large layers.
(problem 106
  (fn [n0 n1]
    (letfn [(apply-ops [ns]
              (mapcat ops-on-num ns))
            (ops-on-num [n]
              (if (even? n)
                [(* 2 n) (/ n 2) (+ 2 n)]
                [(* 2 n) (+ 2 n)]))]

      (loop [c 1, ns [n0]]
        (if (some #{n1} ns)
          c
          (let [next-ns (apply-ops ns)]
            (recur (inc c)
                   (set (apply-ops ns))))))))

  (= 1 (__ 1 1))   ; 1
  (= 3 (__ 3 12))  ; 3 6 12
  (= 3 (__ 12 3))  ; 12 6 3
  (= 3 (__ 5 9))   ; 5 7 9
  (= 9 (__ 9 2))   ; 9 18 20 10 12 6 8 4 2
  (= 5 (__ 9 12))) ; 9 11 22 24 12


;; ### Problem 107: <a href="http://www.4clojure.com/problem/107">Simple closures</a>
(problem 107
  (fn [n]
    (fn [x]
      (apply * (repeat n x))))
  (= 256 ((__ 2) 16) ((__ 8) 2))
  (= [1 8 27 64] (map (__ 3) [1 2 3 4]))
  (= [1 2 4 8 16] (map #((__ %) 2) [0 1 2 3 4])))


;; ### Problem 108: <a href="http://www.4clojure.com/problem/108">Lazy Searching</a>
(problem 108
  (fn [& seqs]
    (let [firsts (map first seqs)
          biggest (apply max firsts)
          trim #(if (> biggest (first %)) (rest %) %)]
      (if (apply = firsts)
        (first firsts)
        (recur (map trim seqs)))))
  (= 3 (__ [3 4 5]))
  (= 4 (__ [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
  (= 7 (__ (range) (range 0 100 7/6) [2 3 5 7 11 13]))
  (= 64 (__ (map #(* % % %) (range))
            (filter #(zero? (bit-and % (dec %))) (range))
            (iterate inc 20))))


;; ### Problem 109 doesn't exist


;; ### Problem 110: <a href="http://www.4clojure.com/problem/110">Sequence of pronunciations</a>
;;
;; Classic case for `iterate`.  The trick here is to split the
;; sequence whenever the value changes (`partition-by identity`).
;; Every item in each subsequence is identical, and `first` gives the
;; value, while `count` gives the number of occurrences.  The last
;; tricky bit is you have to use `rest` since `iterate` gives the
;; original input value, which is not needed.
(problem 110
  (fn [s]
    (rest (iterate #(mapcat (juxt count first)
                            (partition-by identity %))
                   s)))
  (= [[1 1] [2 1] [1 2 1 1]] (take 3 (__ [1])))
  (= [3 1 2 4] (first (__ [1 1 1 4 4])))
  (= [1 1 1 3 2 1 3 2 1 1] (nth (__ [1]) 6))
  (= 338 (count (nth (__ [3 2]) 15))))


;; ### Problem 111: <a href="http://www.4clojure.com/problem/111">Crossword Puzzle</a>
;;
;; The approach taken here is a relatively straightforward functional
;; "pipeline":
;;
;; 1. collect horizontal rows (`rows`) and vertical ones (`vert-rows`)
;;    and treat them the same by concatenating them into `all-rows`.
;;    Vertical rows are made from horizontal ones by `interleave`ing
;;    the horizontal rows and then re-`partition`ing them (only needed
;;    when more than one horizontal row is present);
;; 1. Split each of these at the `\#` symbol using `partition-by`;
;; 1. Remove the left-over `\#`s;
;; 1. Remove all spaces;
;; 1. Only accept sequences with the same length as the given word;
;; 1. Accept any sequence which "matches" the word; match either using
;;    underscores, or when characters are equal (`match-char?` and
;;    `match?`);
;; 1. Convert truthiness to true/false.
(problem 111
  (fn [word rows]
    (let [vert-rows (if (empty? (rest rows))               ;; 1
                      (map vector (first rows))
                      (->> rows
                           (apply interleave)
                           (partition (count rows))))
          all-rows (concat rows vert-rows)                 ;; 1
          match-char? #(or (= %1 \_) (= %1 %2))            ;; 6
          match? #(every? true? (map match-char? %1 %2))]  ;; 6
      (->> all-rows
           (mapcat (partial partition-by #(= % \#)))       ;; 2
           (remove #{[\#]})                                ;; 3
           (map (partial remove #{\space}))                ;; 4
           (filter #(= (count %) (count word)))            ;; 5
           (some #(match? % word))                         ;; 6
           true?)))                                        ;; 7

  (= true  (__ "the" ["_ # _ _ e"]))
  (= false (__ "the" ["c _ _ _"
                      "d _ # e"
                      "r y _ _"]))
  (= true  (__ "joy" ["c _ _ _"
                      "d _ # e"
                      "r y _ _"]))
  (= false (__ "joy" ["c o n j"
                      "_ _ y _"
                      "r _ _ #"]))
  (= true  (__ "clojure" ["_ _ _ # j o y"
                          "_ _ o _ _ _ _"
                          "_ _ f _ # _ _"])))


;; ### Problem 112: <a href="http://www.4clojure.com/problem/112">Sequs Horribilis</a>
;; Though I suspect one could solve this more elegantly with Clojure's
;; built-in tree functions, I solved this by traversing the input and
;; building the output tree recursively depth-first, summing elements
;; and only adding them to the output tree so long as the sum was less
;; than or equal to the target value.
(problem 112
  (fn [max s]
    (let [f (fn f [n [el & more]]
              (cond
                (integer? el) (if (> (+ n el) max)
                                [(+ n el) ()]
                                (let [[n1 ret] (f (+ n el) more)]
                                  [n1 (cons el ret)]))
                (not el) [n ()]
                :else (let [[n1 left] (f n el)
                            [n2 right] (f n1 more)]
                        [n2 (if (> n2 max)
                              (list left)
                              (cons left right))])))]
      (second (f 0 s))))

  (=  (__ 10 [1 2 [3 [4 5] 6] 7])
      '(1 2 (3 (4))))
  (=  (__ 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
      '(1 2 (3 (4 (5 (6 (7)))))))
  (=  (__ 9 (range))
      '(0 1 2 3))
  (=  (__ 1 [[[[[1]]]]])
      '(((((1))))))
  (=  (__ 0 [1 2 [3 [4 5] 6] 7])
      '())
  (=  (__ 0 [0 0 [0 [0]]])
      '(0 0 (0 (0))))
  (=  (__ 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
      '(-10 (1 (2 3 (4)))))
  (=  (__ 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
      '(-10 (1 (2 3 (4)))))
  ;; Bonus, extra tests for edge cases:
  (=  (__ 36 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
      '(1 2 (3 (4 (5 (6 (7 8)))))))
  (=  (__ 37 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
      '(1 2 (3 (4 (5 (6 (7 8)))))))
  (=  (__ 44 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
      '(1 2 (3 (4 (5 (6 (7 8)))))))
  (=  (__ 45 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
      '(1 2 (3 (4 (5 (6 (7 8)) 9)))))
  (=  (__ 46 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
      '(1 2 (3 (4 (5 (6 (7 8)) 9))))))


;; ### Problem 113: <a href="http://www.4clojure.com/problem/113">Making Data Dance</a>
;;
;; The fact that `proxy` was disallowed suggested a look at
;; `reify`, since `proxy` and `reify` are often discussed together.  A
;; bit of research and REPL experimentation showed how to get `reify`
;; to implement the given interface.
(problem 113
  (fn [& args]
    (reify clojure.lang.ISeq
      (toString [this] (clojure.string/join ", " (sort args)))
      (seq [this] (and args (distinct args)))))

  (= "1, 2, 3" (str (__ 2 1 3)))
  (= '(2 1 3) (seq (__ 2 1 3)))
  (= '(2 1 3) (seq (__ 2 1 3 3 1 2)))
  (= '(1) (seq (apply __ (repeat 5 1))))
  (= "1, 1, 1, 1, 1" (str (apply __ (repeat 5 1))))
  (and (= nil (seq (__)))
       (=  "" (str (__)))))


;; Hats off to user `dacquiri` for the
;; [reminder](http://pastebin.com/cS9sP764) that `distinct` removes
;; duplicates (the `and args` is needed in case args is not supplied
;; at all).  My original solution to the `seq` part was quite a bit uglier.
(comment  ;; Original solution for removing duplicates:
  (seq [this] (loop [[x & xs :as X] args
                     ret []]
                (if-not (seq X)
                  (seq ret)
                  (recur xs (if ((set ret) x) ret (conj ret x)))))))


;; ### Problem 114: <a href="http://www.4clojure.com/problem/114">Global Take-While</a>
;; Since no solution that composes the core sequence functions was
;; obvious to me, the fact that the result is stipulated to be lazy
;; (though there are no unit tests for that), would suggest using
;; `lazy-seq`.  Call a helper function recursively to keep track of
;; the number of times the test predicate `pred` is true. The edge
;; cases are tricky to get right without writing a few simpler tests
;; first, so I include those.
(problem 114
  (fn [maxnum pred coll]
    (let [nth-take (fn nth-take [hits maxnum [x & more :as coll]]
                     (lazy-seq
                      (when (seq coll)
                        (if-not (pred x)
                          (cons x (nth-take hits maxnum more))
                          (if (< hits (dec maxnum))
                            (cons x (nth-take (inc hits) maxnum more))
                            ;; We're done; don't add this element
                            ())))))]
      (nth-take 0 maxnum coll)))
  ;; A couple of simpler test cases:
  (= (__ 1 identity [false true]) [false])
  (= (__ 1 identity [false false true]) [false false])
  (= (__ 2 identity [true]) [true])
  ;; Supplied test cases:
  (= [2 3 5 7 11 13]
     (__ 4 #(= 2 (mod % 3))
         [2 3 5 7 11 13 17 19 23]))
  (= ["this" "is" "a" "sentence"]
     (__ 3 #(some #{\i} %)
         ["this" "is" "a" "sentence" "i" "wrote"]))
  (= ["this" "is"]
     (__ 1 #{"a"}
         ["this" "is" "a" "sentence" "i" "wrote"])))


;; ### Problem 115: <a href="http://www.4clojure.com/problem/115">The Balance of N</a>
;; The cheap and dirty trick here is to use the string representation
;; of the number to get the digits; after that, it's a matter of
;; chopping the sequence in half and adding up the digits in each half.
(problem 115
  (fn [x]
    (let [digits (map #(Integer. (str %)) (str x))
          pivot (quot (count digits) 2)]
      (= (apply + (take pivot digits))
         (apply + (take pivot (reverse digits))))))
  (= true (__ 11))
  (= true (__ 121))
  (= false (__ 123))
  (= true (__ 0))
  (= false (__ 88099))
  (= true (__ 89098))
  (= true (__ 89089))
  (= (take 20 (filter __ (range)))
     [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])
  (= (take 20 (filter __ (range)))
     [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101]))


;; ### Problem 116: <a href="http://www.4clojure.com/problem/116">Prime Sandwich</a>
;; Not a super efficient solution, but simple enough; determine
;; primality of \\(n\\) by checking all divisors from 2 on up
;; (\\(O(n)\\)), and then, because we need to find previous and next
;; primes, we get a list of all primes and partition them in groups of
;; three so that, when the middle of the group is equal to \\(n\\),
;; the first and third values are the previous and next primes,
;; respectively; overall, \\(O(n^2)\\).
(problem 116
  (fn [n]
    (let [isprime (fn [x]
                    (and (not (#{1 4} x))
                         (->> (range 2 (quot x 2))
                              (some #(zero? (rem x %)))
                              not)))
          [lo self hi] (->> (range)
                            rest
                            (filter isprime)
                            (partition 3 1)
                            (take-while (comp (partial >= n) second))
                            last)]
      (and (isprime n)
           (= n self)
           (= self (/ (+ lo hi) 2)))))
  (= false (__ 4))
  (= true (__ 563))
  (= 1103 (nth (filter __ (range)) 15)))


;; ### Problem 117: <a href="http://www.4clojure.com/problem/117">For Science!</a>
;;
;; I treat this as another graph search problem, where connections
;; between "nodes" are determined by horizontal or vertical adjacency.
;; At each stage in the search, we loop through neighbors that:
;;
;; 1. are "valid" positions (within bounds of the board, i.e. their
;;    contents are non-nil;
;; 1. have not been explored yet; and
;; 1. are not walls (`#`).
;;
;; At each point we check to see if we've found the cheese or run out
;; of unexplored spaces to check. After the graph traversal terminates,
;; we look at all the visited nodes to see if cheese has been found.
;; Though the algorithm terminates as soon as cheese is found
;; (`(set-cheese bp p)`), it is somewhat inefficient in that it loops
;; through the visited locations again at the end.  It does, however,
;; only visit "connected" parts of the maze.
(problem 117
  (fn [board]
    (let [bm (into {} (for [r (range (count board))
                            c (range (count (get board r)))]
                        (let [val (get-in board [r c])]
                          [[r c] {:val val
                                  :explored (= val \#)}])))
          mouse (some (fn [[k v]] (when (= (:val v) \M) k)) bm)]
      (letfn [(explored [bm pos] (get-in bm [pos :explored]))
              (set-explored [bm pos] (assoc-in bm [pos :explored] true))
              (set-cheese [bm pos] (assoc-in bm [pos :cheese] true))
              (is-valid [bm pos] (not (nil? (get-in bm [pos :val]))))
              (is-wall [bm pos] (= (get-in bm [pos :val]) \#))
              (get-neighbors [[r c]]
                [[(dec r) c], [r (dec c)], [(inc r) c], [r (inc c)]])
              (dfs [bm pos]
                (let [bm (set-explored bm pos)
                      neighbors (->> pos
                                     get-neighbors
                                     (filter (partial is-valid bm))
                                     (remove (partial explored bm))
                                     (remove (partial is-wall bm)))]
                  (loop [bm bm, [p & ps] neighbors]
                    (cond
                      (not p) bm
                      (= (get-in bm [p :val]) \C) (set-cheese bm p)
                      (or (explored bm p)
                          (= (get-in bm [p :val]) \#)) (recur bm ps)
                      :else (recur (dfs bm p) ps)))))]
        (true? (some (fn [[_ {cheese :cheese}]] cheese)
                     (dfs bm mouse))))))

  (= true  (__ ["M   C"]))
  (= false (__ ["M # C"]))
  (= true  (__ ["#######"
                "#     #"
                "#  #  #"
                "#M # C#"
                "#######"]))
  (= false (__ ["########"
                "#M  #  #"
                "#   #  #"
                "# # #  #"
                "#   #  #"
                "#  #   #"
                "#  # # #"
                "#  #   #"
                "#  #  C#"
                "########"]))
  (= false (__ ["M     "
                "      "
                "      "
                "      "
                "    ##"
                "    #C"]))
  (= true  (__ ["C######"
                " #     "
                " #   # "
                " #   #M"
                "     # "]))
  (= true  (__ ["C# # # #"
                "        "
                "# # # # "
                "        "
                " # # # #"
                "        "
                "# # # #M"])))



;; ### Problem 118: <a href="http://www.4clojure.com/problem/118">Re-implement Map</a>
;; Classic, straightforward recursion problem whose lazy version is
;; provided by `lazy-seq`.
(problem 118
  (fn mymap [f s]
    (lazy-seq
     (when (seq s)
       (cons (f (first s)) (mymap f (rest s))))))
  (= [3 4 5 6 7]
     (__ inc [2 3 4 5 6]))
  (= [3 4 5 6 7]
     (__ inc [2 3 4 5 6]))
  (= (repeat 10 nil)
     (__ (fn [_] nil) (range 10)))
  (= [1000000 1000001]
     (->> (__ inc (range))
          (drop (dec 1000000))
          (take 2)))
  (= [1000000 1000001]
     (->> (__ inc (range))
          (drop (dec 1000000))
          (take 2))))


;; ### Problem 119: <a href="http://www.4clojure.com/problem/119">Win at Tic Tac Toe</a>
;;
;; The solution is quite similar to that for [Problem
;; 73](http://www.4clojure.com/problem/73).  We preserve the "diag,"
;; "ant-diag," "across" and "down" logic for determining whether
;; three-in-a-row has been obtained.  The difference is that we now
;; apply this logic over every \\(x, y\\) (or, rather, `[y x]`)
;; combination, placing the desired piece (`:x` or `:o`) in the spot
;; if it's currently empty, and returning the pair only if it makes
;; three-in-a-row.
(problem 119
  (fn [ox current-board]
    (set
     (for [y (range 3)
           x (range 3)
           :let [piece (get-in current-board [y x])
                 new-piece (if (= piece :e) ox piece)
                 new-board (assoc-in current-board [y x] new-piece)]
           :when (or
                  ;; diag:
                  (every? #{ox}
                          (for [i (range 3)]
                            (get-in new-board [i i])))
                  ;; anti-diag:
                  (every? #{ox}
                          (for [i (range 3)]
                            (get-in new-board [i (- 2 i)])))
                  ;; across:
                  (some (fn [i]
                          (every? #{ox} (for [j (range 3)]
                                          (get-in new-board [i j]))))
                        (range 3))
                  ;; down:
                  (some (fn [i]
                          (every? #{ox} (for [j (range 3)]
                                          (get-in new-board [j i]))))
                        (range 3)))]
       [y x])))

  (= (__ :x [[:o :e :e]
             [:o :x :o]
             [:x :x :e]])
     #{[2 2] [0 1] [0 2]})
  (= (__ :x [[:x :o :o]
             [:x :x :e]
             [:e :o :e]])
     #{[2 2] [1 2] [2 0]})
  (= (__ :x [[:x :e :x]
             [:o :x :o]
             [:e :o :e]])
     #{[2 2] [0 1] [2 0]})
  (= (__ :x [[:x :x :o]
             [:e :e :e]
             [:e :e :e]])
     #{})
  (= (__ :o [[:x :x :o]
             [:o :e :o]
             [:x :e :e]])
     #{[2 2] [1 1]}))


;; ### Problem 120: <a href="http://www.4clojure.com/problem/120">Sum of Square Digits</a>
;;
;; The solution consists of building up and composing functions which:
;; convert a character to an integer (`char-to-int`); square an
;; integer (`square`); pull out the squares of digits of a number
;; (`sq-digs-fn`); and apply the comparison of the sum of squares to
;; the original number (`compare-fn`).
(problem 120
  (fn [s]
    (let [char-to-int #(-> % str Integer/parseInt)
          square #(* % %)
          sq-digs-fn (fn [n] (map #(->> % char-to-int square)
                                  (str n)))
          compare-fn #(< % (apply + (sq-digs-fn %)))]
      (count (filter compare-fn s))))

  (= 8 (__ (range 10)))
  (= 19 (__ (range 30)))
  (= 50 (__ (range 100)))
  (= 50 (__ (range 1000))))


;; ### Problem 121: <a href="http://www.4clojure.com/problem/121">Universal Computation Engine</a>
;; Straighforward tree traversal, where literal symbols are replaced
;; with the functions or values they represent.
(problem 121
  (fn [expr]
     (fn [syms]
       (let [m (merge syms {'+ +, '- -, '/ /, '* *})
             f (fn f [x]
                 (cond
                   (symbol? x) (m x)
                   (list? x) (apply (m (first x)) (map f (rest x)))
                   :else x))]
         (f expr))))

  (= 2 ((__ '(/ a b))
        '{b 8 a 16}))
  (= 8 ((__ '(+ a b 2))
        '{a 2 b 4}))
  (= [6 0 -4]
     (map (__ '(* (+ 2 a)
                  (- 10 b)))
          '[{a 1 b 8}
            {b 5 a -2}
            {a 2 b 11}]))
  (= 1 ((__ '(/ (+ x 2)
                (* 3 (+ y 1))))
        '{x 4 y 1})))



;; ### Problem 122: <a href="http://www.4clojure.com/problem/122">Read a binary number</a>
;; When the digits are reversed, a recursive solution is
;; straightforward: each call shifts the previous result left and adds
;; the new term.  Assumes the number length is less than the max call
;; stack, which is usually the case.  Otherwise for large numbers, use
;; `loop`..`recur` and the promoting `+` operator, `+'`.
(problem 122
  (comp (fn f [[el & more]]
          (if el
            (+ ({\1 1 \0 0} el) (bit-shift-left (f more) 1))
            0))
        reverse)

  (= 0     (__ "0"))
  (= 7     (__ "111"))
  (= 8     (__ "1000"))
  (= 9     (__ "1001"))
  (= 255   (__ "11111111"))
  (= 1365  (__ "10101010101"))
  (= 65535 (__ "1111111111111111")))


;; ### Problem 123 doesn't exist


;; ### Problem 124: <a href="http://www.4clojure.com/problem/124">Analyze Reversi</a>
;;
;; To solve this, we check every non-empty starting position (1)
;; which causes one or more "flips" (2), as determined by the `flips`
;; function, called for each of eight possible directions (3a,b).
;;
;; Given a board, starting position, player color and direction,
;; `flips` works by iterating the direction function against the
;; starting position and collecting the color at each resulting
;; position (4), stopping when it runs off the board (5).
;; `partition-by` collects sequences of like colors (including
;; empties) (6); `flips` returns a truthy val iff the last color
;; sequence is the played color, and the closest color sequence is the
;; opposite color (7).
(problem 124
  (fn [board player]
    (let [n (count board) ;; assume square board
          ;; 8 directions:
          no (fn [[r c]] [(dec r) c])
          so (fn [[r c]] [(inc r) c])
          ea (fn [[r c]] [r (inc c)])
          we (fn [[r c]] [r (dec c)])
          ne (comp no ea)
          nw (comp no we)
          se (comp so ea)
          sw (comp so we)
          directions [no so ea we ne nw se sw]                           ;; #3a
          flips (fn [B [r c] color dirn]
                  (let [places-on-path (map (juxt (partial get-in B) identity)
                                            (rest (iterate dirn [r c]))) ;; #4
                        pieces-on-path (take-while (comp (complement nil?)
                                                         first)
                                                   places-on-path)]      ;; #5
                    (->> pieces-on-path
                         (partition-by first)                            ;; #6
                         (take-while #(not= (ffirst %) 'e))
                         reverse
                         ((fn [[[[clr-a]] ret]]
                            (if (and (= clr-a color)                     ;; #7
                                     (= (ffirst ret) ({'w 'b, 'b 'w} color)))
                              (map second ret)))))))]
      (into {} (for [row (range n)
                     col (range n)
                     :let [flps (set
                                 (mapcat (partial flips board [row col] player)
                                         directions))]                   ;; #3b
                     :when (and (= (get-in board [row col]) 'e)          ;; #1
                                (seq flps))]                             ;; #2
                 [[row col] flps]))))

  (= {[1 3] #{[1 2]}, [0 2] #{[1 2]}, [3 1] #{[2 1]}, [2 0] #{[2 1]}}
     (__ '[[e e e e]
           [e w b e]
           [e b w e]
           [e e e e]] 'w))
  (= {[3 2] #{[2 2]}, [3 0] #{[2 1]}, [1 0] #{[1 1]}}
     (__ '[[e e e e]
           [e w b e]
           [w w w e]
           [e e e e]] 'b))
  (= {[0 3] #{[1 2]}, [1 3] #{[1 2]}, [3 3] #{[2 2]}, [2 3] #{[2 2]}}
     (__ '[[e e e e]
           [e w b e]
           [w w b e]
           [e e b e]] 'w))
  (= {[0 3] #{[2 1] [1 2]}, [1 3] #{[1 2]}, [2 3] #{[2 1] [2 2]}}
     (__ '[[e e w e]
           [b b w e]
           [b w w e]
           [b w w w]] 'b)))


;; ### Problem 125: <a href="http://www.4clojure.com/problem/125">Gus' Quinundrum</a>
;;
;; Even if you didn't follow the hint link to Wikipedia (I did), the
;; title itself is a hint that a Quine is called for.  Not much more
;; to say about this solution other than that getting all the escaped
;; characters to match up was the tricky bit.
;;
;; I saw a talk at, I think, Clojure/West a few years back, where Dan
;; Friedman and William Byrd generated an infinite lazy sequence of
;; progressively more complex quines -- very impressive!
(problem 125
  (fn [] (let [e \\ q \" s "(fn [] (let [e %c%c q %c%c s %c%s%c] (format s e e e q q s q)))"] (format s e e e q q s q)))
  (= (str '__) (__)))


;; ### Problem 126: <a href="http://www.4clojure.com/problem/126">Through the Looking Class</a>
;;
;; I found the answer to this one just by iterating `class` at the
;; REPL, calling it successively on its own output.  The final `x` in
;; the `and` form excludes `nil`, whose class is also itself.
(problem 126
  java.lang.Class
  (let [x __]
    (and (= (class x) x) x)))


;; ### Problem 127: <a href="http://www.4clojure.com/problem/127">Love Triangles</a>
;;
;; I had to take a couple runs at this one.
;;
;; The easiest part is converting the numeric values to bit patterns
;; (`as-bit-strings`).  Once those are in hand (a series of character
;; string rows), we use `get-in` as is done in several other problems
;; to get the character value of a particular row and column.
;;
;; The strategy is then:
;;
;; 1. Scan across each point, finding the largest triangle which can
;;    be made starting at that point.
;; 2. There are eight possible triangles which can "grow" in colinear
;;    sets of points from a given point.  Triangles are "grown" until
;;    one or more of the colinear points has a value other than `\1`.
;; 3. The largest triangle is chosen, and its size used as the triangle
;;    size for that point.
;;
;; Unlike many of my solutions to other problems, I find this solution
;; much easier to read and understand than it was to come up with in
;; the first place!
(problem 127
  (fn f [input-vec]
    (let [to-bin-str (fn [n] (Integer/toBinaryString n))

          max-bits-in-coll
          (fn [coll]
            (->> coll (map (comp count to-bin-str)) (apply max)))

          board-size (fn [strings]
                       [(count strings), (-> strings first count)])

          as-bit-strings
          (fn [coll]
            (let [mb (max-bits-in-coll coll)]
              (->> coll
                   (mapv (fn [n]
                           (let [as-bin (to-bin-str n)
                                 num-zeros (- mb (count as-bin))]
                             (apply str (concat (repeat num-zeros \0)
                                                as-bin))))))))
          bd (as-bit-strings input-vec)

          [nr nc] (board-size bd)

          top-left-triangle-edge (fn [[r c] delta]
                                   (for [cursor (range 0 (inc delta))]
                                     [(- (+ cursor r) delta)
                                      (+ c cursor)]))
          bottom-left-triangle-edge (fn [[r c] delta]
                                      (for [cursor (range 0 (inc delta))]
                                        [(- (+ delta r) cursor)
                                         (+ c cursor)]))
          top-right-triangle-edge (fn [[r c] delta]
                                    (for [cursor (range 0 (inc delta))]
                                      [(- (+ cursor r) delta)
                                       (- c cursor)]))
          bottom-right-triangle-edge (fn [[r c] delta]
                                       (for [cursor (range 0 (inc delta))]
                                         [(- (+ delta r) cursor)
                                          (- c cursor)]))
          east-double-triangle-edge (fn [[r c] delta]
                                      (for [cursor (range (- delta)
                                                          (inc delta))]
                                        [(+ r cursor)
                                         (+ c delta)]))
          west-double-triangle-edge (fn [[r c] delta]
                                      (for [cursor (range (- delta)
                                                          (inc delta))]
                                        [(+ r cursor)
                                         (- c delta)]))
          north-double-triangle-edge (fn [[r c] delta]
                                       (for [cursor (range (- delta)
                                                           (inc delta))]
                                         [(- r delta)
                                          (+ c cursor)]))
          south-double-triangle-edge (fn [[r c] delta]
                                       (for [cursor (range (- delta)
                                                           (inc delta))]
                                         [(+ r delta)
                                          (+ c cursor)]))

          good-edge (fn [edge-points]
                      (every? #(= \1 (get-in bd %)) edge-points))

          best-score-for-triangles-at-point
          (fn [[r c]]
            (reduce max
                    (for [f [top-left-triangle-edge
                             bottom-left-triangle-edge
                             top-right-triangle-edge
                             bottom-right-triangle-edge
                             east-double-triangle-edge
                             west-double-triangle-edge
                             north-double-triangle-edge
                             south-double-triangle-edge]]
                      (->> (range)
                           (map (partial f [r c]))
                           (take-while good-edge)
                           (map count)
                           (reduce +)))))]
      (let [ans
            (reduce max
                    (for [r (range nr)
                          c (range nc)]
                      (best-score-for-triangles-at-point [r c])))]
        (when (>= ans 3) ans))))

  (= 10 (__ [15 15 15 15 15]))
                                        ; 1111      1111
                                        ; 1111      *111
                                        ; 1111  ->  **11
                                        ; 1111      ***1
                                        ; 1111      ****
  (= 15 (__ [1 3 7 15 31]))
                                        ; 00001      0000*
                                        ; 00011      000**
                                        ; 00111  ->  00***
                                        ; 01111      0****
                                        ; 11111      *****
  (= 3 (__ [3 3]))
                                        ; 11      *1
                                        ; 11  ->  **
  (= 4 (__ [7 3]))
                                        ; 111      ***
                                        ; 011  ->  0*1
  (= 6 (__ [17 22 6 14 22]))
                                        ; 10001      10001
                                        ; 10110      101*0
                                        ; 00110  ->  00**0
                                        ; 01110      0***0
                                        ; 10110      10110
  (= 9 (__ [18 7 14 14 6 3]))
                                        ; 10010      10010
                                        ; 00111      001*0
                                        ; 01110      01**0
                                        ; 01110  ->  0***0
                                        ; 00110      00**0
                                        ; 00011      000*1
  (= nil (__ [21 10 21 10]))
                                        ; 10101      10101
                                        ; 01010      01010
                                        ; 10101  ->  10101
                                        ; 01010      01010
  (= nil (__ [0 31 0 31 0])))
                                        ; 00000      00000
                                        ; 11111      11111
                                        ; 00000  ->  00000
                                        ; 11111      11111
                                        ; 00000      00000


;; ### Problem 128: <a href="http://www.4clojure.com/problem/128">Recognize Playing Cards</a>
(problem 128
  (fn [[s v]]
    {:suit ({\S :spade, \C :club, \D :diamond, \H :heart} s)
     :rank (get {\T 8, \J 9, \Q 10, \K 11, \A 12}
                v
                (- (int v) (int \0) 2))})

  (= {:suit :diamond :rank 10} (__ "DQ"))
  (= {:suit :heart :rank 3} (__ "H5"))
  (= {:suit :club :rank 12} (__ "CA"))
  (= (range 13) (map (comp :rank __ str)
                     '[S2 S3 S4 S5 S6 S7
                       S8 S9 ST SJ SQ SK SA])))


;; ### Problem 129: doesn't exist


;; ### Problem 130: <a href="http://www.4clojure.com/problem/130">Tree Reparenting</a>
;;
;; The solution works by successively "lifting" an element upwards in
;; the tree.  "Lifting" (`lift-kid`) means promoting the element to be
;; lifted to the root node, and adding the original root node (minus
;; the child) to the new root's children.  Finally the reduction
;; applies this "lifting" successively to all nodes on the path to the
;; desired one.  `path-fn` finds that path.
;;
;; I found it easiest to reason about this in small steps, making the
;; various helper functions, e.g. to find the appropriate child node
;; (`find-kid`), remove a child node (`remove-kid`), etc. as I went,
;; and staring at lots of drawings of tree graphs.
(problem 130
  (fn [lift-el T]
    (let [kids (fn [[_ & ks]] ks)
          tree (fn [eln kids] (list* eln kids))
          add (fn [el T] (concat T [el]))
          name-fn (fn [[eln & _]] eln)
          find-kid (fn [[_ & ks] el-name]
                     (first (filter #(= el-name (name-fn %)) ks)))
          remove-kid (fn [[eln & ks] el-name]
                       (tree eln
                             (remove #(= el-name (name-fn %)) ks)))
          lift-kid (fn [T el-name]
                     (add (remove-kid T el-name)
                          (find-kid T el-name)))
          path-fn (fn p [so-far goal [eln & kids]]
                    (if (= eln goal)
                      (conj so-far eln)
                      (mapcat (partial p (conj so-far eln) goal) kids)))
          path (rest (path-fn [] lift-el T))]
      (reduce lift-kid T path)))

  (= '(n)
     (__ 'n '(n)))
  (= '(a (t (e)))
     (__ 'a '(t (e) (a))))
  (= '(e (t (a)))
     (__ 'e '(a (t (e)))))
  (= '(a (b (c)))
     (__ 'a '(c (b (a)))))
  (= '(d
       (b
        (c)
        (e)
        (a
         (f
          (g)
          (h)))))
     (__ 'd '(a
              (b
               (c)
               (d)
               (e))
              (f
               (g)
               (h)))))
  (= '(c
       (d)
       (e)
       (b
        (f
         (g)
         (h))
        (a
         (i
          (j
           (k)
           (l))
          (m
           (n)
           (o))))))
     (__ 'c '(a
              (b
               (c
                (d)
                (e))
               (f
                (g)
                (h)))
              (i
               (j
                (k)
                (l))
               (m
                (n)
                (o)))))))


;; ### Problem 131: <a href="http://www.4clojure.com/problem/131">Sum Some Set Subsets</a>
;; Reusing the solution to Problem 85 ("Power Set"), we first collect
;; all the sums of each (non-empty) subset - one set of sums per
;; original set.  The intersection of all these sets of sums is
;; non-empty if and only if all the sets have one sum in common.
(problem 131
  (fn [& sets]
    (let [pwr (fn pwr [xs]
                (let [[x & more :as xs] (vec xs)]
                  (if-not (seq xs)
                    #{#{}}
                    (->> (pwr more)
                         (concat (map #(conj % x) (pwr more)))
                         set))))
          sets-of-sums (for [powerset (map pwr sets)]
                         (->> powerset
                              (remove empty?)
                              (map (partial apply +))
                              set))]
      (->> sets-of-sums
           (apply clojure.set/intersection)
           empty?
           not)))

  (= true  (__ #{-1 1 99}
               #{-2 2 888}
               #{-3 3 7777})) ; ex. all sets have a subset which sums to zero
  (= false (__ #{1}
               #{2}
               #{3}
               #{4}))
  (= true  (__ #{1}))
  (= false (__ #{1 -3 51 9}
               #{0}
               #{9 2 81 33}))
  (= true  (__ #{1 3 5}
               #{9 11 4}
               #{-3 12 3}
               #{-3 4 -2 10}))
  (= false (__ #{-1 -2 -3 -4 -5 -6}
               #{1 2 3 4 5 6 7 8 9}))
  (= true  (__ #{1 3 5 7}
               #{2 4 6 8}))
  (= true  (__ #{-1 3 -5 7 -9 11 -13 15}
               #{1 -3 5 -7 9 -11 13 -15}
               #{1 -1 2 -2 4 -4 8 -8}))
  (= true  (__ #{-10 9 -8 7 -6 5 -4 3 -2 1}
               #{10 -9 8 -7 6 -5 4 -3 2 -1}))
  (= true  (__ #{-10 9 -8 7 -6 5 -4 3 -2 1}
               #{10 -9 8 -7 6 -5 4 -3 2 -1})))


;; ### Problem 132: <a href="http://www.4clojure.com/problem/132">Insert between two items</a>
;; As is so often the case, the problem is amenable either to a
;; low-level solution with `loop` / `recur`, or a higher-level, more
;; functional solution. We choose the latter: take all pairs `a`-`b`
;; and convert them into vectors of either `[b]` or `[token b]`,
;; depending on `(pred a b)`.  Concatenate these together, and attach
;; the first `a` to the front, and this gives the desired answer with
;; no unwanted duplication of items.
(problem 132
  (fn [pred token s]
    (when (seq s)
      (cons (first s)
            (apply concat
                   (for [[a b] (partition 2 1 s)]
                     (if (pred a b)
                       [token b]
                       [b]))))))

  (= '(1 :less 6 :less 7 4 3) (__ < :less [1 6 7 4 3]))
  (= '(2) (__ > :more [2]))
  (= [0 1 :x 2 :x 3 :x 4]  (__ #(and (pos? %) (< % %2)) :x (range 5)))
  (empty? (__ > :more ()))
  (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
     (take 12 (->> [0 1]
                   (iterate (fn [[a b]] [b (+ a b)]))
                   (map first)          ; fibonacci numbers
                   (__ (fn [a b]        ; both even or both odd
                         (= (mod a 2) (mod b 2)))
                       :same)))))


;; ### Problem 137: <a href="http://www.4clojure.com/problem/137">Digits and Bases</a>
;;
;; This is a simple matter of dividing modulo the base and shifting
;; out low-order digits.  Alternatively, could do as a lazy seq.
(problem 137
  (fn [n base]
    (loop [n n, ret []]
      (if (zero? n)
        (if (empty? ret) [0] ret)
        (recur (quot n base) (cons (mod n base) ret)))))
  (= [1 2 3 4 5 0 1] (__ 1234501 10))
  (= [0] (__ 0 11))
  (= [1 0 0 1] (__ 9 2))
  (= [1 0] (let [n (rand-int 100000)](__ n n))) 
  (= [16 18 5 24 15 1] (__ Integer/MAX_VALUE 42)))


;; ### Problem 138: <a href="http://www.4clojure.com/problem/138">Squares Squared</a>
;;
;;
;; I'm probably less proud of this one than most of the others.  In
;; any case, had the 4Clojure format supported `defn` I'd have broken
;; this up into several functions and documented them individually for
;; the sake of clarity.  Instead, comments are salted throughout the
;; code, which is longer than I'd like.
(problem 138
  (fn [n0 n1]
    (let [turn-right {[ 1  1] [ 1 -1], [ 1 -1] [-1 -1],
                      [-1 -1] [-1  1], [-1  1] [ 1  1]}
          ;; The function which returns the coordinates of an
          ;; ever-increasing spiral of `n` values:
          spiral (fn [n]
                   (let [pts (loop [pos [0 0], dir [-1 1], n n, ret [],
                                    board #{pos}]
                               (if-not (pos? n)
                                 ret
                                 (let [next-dir (turn-right dir)
                                       next-pos (map + pos next-dir)
                                       should-turn (board next-pos)]
                                   ;; Already have been there?  If so,
                                   ;; use existing rather than new
                                   ;; direction.  If not, turn right.
                                   (recur (if should-turn
                                            (map + pos dir)
                                            next-pos)
                                          (if should-turn dir next-dir)
                                          (dec n)
                                          (conj ret pos)
                                          (conj board pos)))))
                         minr (->> pts (map first) (apply min))
                         minc (->> pts (map second) (apply min))]
                     ;; Apply offset to zero-based positions to "true"
                     ;; rows/columns
                     (map (fn [[r c]] [(- r minr) (- c minc)]) pts)))
          ;; Calculate the actual values in the 'digit spiral',
          ;; filling in the necessary number of `*` characters::
          sqr #(*' % %)
          s (take-while (partial >= n1) (iterate sqr n0))
          squares (map sqr (range))
          digs (mapcat str s)
          square-size (->> squares
                           (drop-while (partial > (count digs)))
                           first)
          stars (repeat (- square-size (count digs)) \*)
          digs (concat digs stars)
          ;; Figure out how big the board needs to be, based on the
          ;; range of positions returned by the `spiral` function:
          spir (spiral (count digs))
          board-size (->> spir
                          (map first)
                          (apply (juxt max min))
                          (apply -)
                          inc)
          ;; Unadorned board consisting only of spaces:
          raw-board (->> \space
                         (repeat board-size)
                         (into [])
                         (repeat board-size)
                         (into []))]
      ;; The final board calculation is a reduction, using `assoc-in`
      ;; to fill the values in `digs` and coordinates returned by
      ;; `spiral`:
      (->> (reduce (fn [B [ch [r c]]]
                     (assoc-in B [r c] ch))
                   raw-board
                   (partition 2 (interleave digs spir)))
           (map (partial apply str)))))

  (= (__ 2 2) ["2"])
  (= (__ 2 4) [" 2 "
               "* 4"
               " * "])
  (= (__ 3 81) [" 3 "
                "1 9"
                " 8 "])
  (= (__ 4 20) [" 4 "
                "* 1"
                " 6 "])
  (= (__ 2 256) ["  6  "
                 " 5 * "
                 "2 2 *"
                 " 6 4 "
                 "  1  "])
  (= (__ 10 10000) ["   0   "
                    "  1 0  "
                    " 0 1 0 "
                    "* 0 0 0"
                    " * 1 * "
                    "  * *  "
                    "   *   "]))



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
(problem 141
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
                   {:suit :diamond :rank 10} {:suit :heart :rank 4}]))
  (= {:suit :diamond :rank 2}
     ((__ :club) [{:suit :diamond :rank 2}
                  {:suit :spade :rank 10}])))


;; ### Problem 144: <a href="https://www.4clojure.com/problem/144">Oscilrate<a/>
;;
;; The `cycle` function (1), applied to the list of given functions, makes
;; this straightforward.
(problem 144
  (fn f [x & s]
    (letfn [(g [x s]
              (lazy-seq
               (cons x
                     (g ((first s) x)
                        (next s)))))]
      (g x (cycle s))))                    ;; #1

  (= (take 3 (__ 3.14 int double)) [3.14 3 3.0])
  (= (take 5 (__ 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7])
  (= (take 12 (__ 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3]))


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
(problem 150
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


;; ### Problem 158: <a href="http://www.4clojure.com/problem/158">Decurry</a>

;; Here's my original solution.  It works and probably performs OK.
(fn g [f]
  (fn l [& xs]
    (loop [f f
           [x & xs] xs]
      (if-not (seq? xs)
        (f x)
        (recur (f x) xs)))))

;; I also tried to do this without `recur`.  I thought `iterate` would
;; make things more elegant and would lead me to a solution in [point
;; free style](http://en.wikipedia.org/wiki/Tacit_programming).  This
;; implementation repeatedly transform a pair of values, the first of
;; which is the (progressively uncurried) function and the second is
;; the list of values remaining.  When this second list is empty, the
;; function in the first slot has been replaced by the final value.
;; Though almost point-free, it isn't quite... nor is it any clearer
;; than my original, IMO.
(problem 158
 (fn [f]
   (fn [& args]
     (->> [f args]
          (iterate (juxt #((comp (first %) first second) %)
                         (comp rest second)))
          (drop-while (comp seq second))
          ffirst)))

 (= 10 ((__ (fn [a]
              (fn [b]
                (fn [c]
                  (fn [d]
                    (+ a b c d))))))
        1 2 3 4))
 (= 24 ((__ (fn [a]
              (fn [b]
                (fn [c]
                  (fn [d]
                    (* a b c d))))))
        1 2 3 4))
 (= 25 ((__ (fn [a]
              (fn [b]
                (* a b))))
        5 5)))


;; ### Problem 164: <a href="http://www.4clojure.com/problem/164">Language of a DFA</a>
;;
;; My first attempt at this was (I realized, too late) a recursive
;; depth-first search.  This doesn't work (as the warning in the
;; problem description suggests) because some of the examples never
;; "bottom out" -- they keep generating strings infinitely.  This
;; obviously suggests that something more lazy is needed.  It took a
;; bit of fiddling to figure out the basic ingredients:
;;
;; 1. Create a `lazy-seq`-based function which represents each stage
;;    of a breadth-first search, collecting all the current states and
;;    emitting (`cons`ing) a record with all accumulated letters
;;    whenever a terminal state is reached.
;; 2. For the letters, accumulate symbols rather than strings; convert
;;    to strings at the end (strings and laziness can be a poor match,
;;    as any lazy object gets converted to something like
;;    "clojure.lang.LazySeq@c5d38b66" rather than what you want.
;;
;; The inputs are overdetermined; `:states` and `:alphabet` are
;; redundant and not used.
(problem 164
  (fn [m]
    (letfn [(stateseq [chains]
              (lazy-seq
               (let [transitions (-> m :transitions)
                     next-chains (for [{:keys [state letters]} chains
                                       [letter next-state] (transitions state)]
                                   {:terminal (-> m :accepts next-state)
                                    :state next-state
                                    :letters (conj letters letter)})
                     terminals (filter :terminal next-chains)]
                 (when (seq next-chains)
                   (concat terminals (stateseq next-chains))))))]
      (map (comp (partial apply str) :letters)
           (stateseq [{:state (:start m), :letters []}]))))

  (= #{"a" "ab" "abc"}
     (set (__ '{:states #{q0 q1 q2 q3}
                :alphabet #{a b c}
                :start q0
                :accepts #{q1 q2 q3}
                :transitions {q0 {a q1}
                              q1 {b q2}
                              q2 {c q3}}})))
  (= #{"hi" "hey" "hello"}
     (set (__ '{:states #{q0 q1 q2 q3 q4 q5 q6 q7}
                :alphabet #{e h i l o y}
                :start q0
                :accepts #{q2 q4 q7}
                :transitions {q0 {h q1}
                              q1 {i q2, e q3}
                              q3 {l q5, y q4}
                              q5 {l q6}
                              q6 {o q7}}})))
  (= (set (let [ss "vwxyz"] (for [i ss, j ss, k ss, l ss] (str i j k l))))
     (set (__ '{:states #{q0 q1 q2 q3 q4}
                :alphabet #{v w x y z}
                :start q0
                :accepts #{q4}
                :transitions {q0 {v q1, w q1, x q1, y q1, z q1}
                              q1 {v q2, w q2, x q2, y q2, z q2}
                              q2 {v q3, w q3, x q3, y q3, z q3}
                              q3 {v q4, w q4, x q4, y q4, z q4}}})))
  (let [res (take 2000 (__ '{:states #{q0 q1}
                             :alphabet #{0 1}
                             :start q0
                             :accepts #{q0}
                             :transitions {q0 {0 q0, 1 q1}
                                           q1 {0 q1, 1 q0}}}))]
    (and (every? (partial re-matches #"0*(?:10*10*)*") res)
         (= res (distinct res))))
  (let [res (take 2000 (__ '{:states #{q0 q1}
                             :alphabet #{n m}
                             :start q0
                             :accepts #{q1}
                             :transitions {q0 {n q0, m q1}}}))]
    (and (every? (partial re-matches #"n*m") res)
         (= res (distinct res))))
  (let [res (take 2000 (__ '{:states #{q0 q1 q2 q3 q4 q5 q6 q7 q8 q9}
                             :alphabet #{i l o m p t}
                             :start q0
                             :accepts #{q5 q8}
                             :transitions {q0 {l q1}
                                           q1 {i q2, o q6}
                                           q2 {m q3}
                                           q3 {i q4}
                                           q4 {t q5}
                                           q6 {o q7}
                                           q7 {p q8}
                                           q8 {l q9}
                                           q9 {o q6}}}))]
    (and (every? (partial re-matches #"limit|(?:loop)+") res)
         (= res (distinct res)))))


;; ### Problem 165 does not exist


;; ### Problem 166: <a href="http://www.4clojure.com/problem/166">Comparisons</a>
(problem 166
 (fn [f a b]
   (cond
     (f a b) :lt
     (f b a) :gt
     (and (not (f a b)) (not (f b a))) :eq))
  (= :gt (__ < 5 1))
  (= :eq (__ (fn [x y] (< (count x) (count y))) "pear" "plum"))
  (= :lt (__ (fn [x y] (< (mod x 5) (mod y 5))) 21 3))
  (= :gt (__ > 0 2)))


;; ### Problem 167 does not exist


;; ### Problem 168: <a href="http://www.4clojure.com/problem/168">Infinite Matrix</a>
;;
;; This problem is all about `lazy-seq` and would probably be hard to
;; do without it.  I originally considered a one-dimensional version
;; of this problem as `maprange1`, a combination of `map` and `range`,
;; each of which can be trivially implemented with `lazy-seq`.  Then I
;; realized that the 2d version is very nearly just another
;; implementation of the 1d case: `maprange2` slightly modifies
;; `maprange1` by adding a second argument to the mapping function
;; `f`.
(problem 168
 (fn mapmap
   ([f] (mapmap f 0 0))
   ([f s1 s2]
      (letfn [(maprange1 [f n1]
                (lazy-seq
                 (cons (f n1) (maprange1 f (inc n1)))))
              (maprange2 [f n1 n2]
                (lazy-seq
                 (cons (f n1 n2) (maprange2 f n1 (inc n2)))))]
        (maprange1 (fn [n1] (maprange2 f n1 s2)) s1)))
   ([f s1 s2 t1 t2]
      (take t1 (map #(take t2 %) (mapmap f s1 s2)))))

 (= (take 5 (map #(take 6 %) (__ str)))
    [["00" "01" "02" "03" "04" "05"]
     ["10" "11" "12" "13" "14" "15"]
     ["20" "21" "22" "23" "24" "25"]
     ["30" "31" "32" "33" "34" "35"]
     ["40" "41" "42" "43" "44" "45"]])
 (= (take 6 (map #(take 5 %) (__ str 3 2)))
    [["32" "33" "34" "35" "36"]
     ["42" "43" "44" "45" "46"]
     ["52" "53" "54" "55" "56"]
     ["62" "63" "64" "65" "66"]
     ["72" "73" "74" "75" "76"]
     ["82" "83" "84" "85" "86"]])
 (= (__ * 3 5 5 7)
    [[15 18 21 24 27 30 33]
     [20 24 28 32 36 40 44]
     [25 30 35 40 45 50 55]
     [30 36 42 48 54 60 66]
     [35 42 49 56 63 70 77]])
 (= (__ #(/ % (inc %2)) 1 0 6 4)
    [[1/1 1/2 1/3 1/4]
     [2/1 2/2 2/3 1/2]
     [3/1 3/2 3/3 3/4]
     [4/1 4/2 4/3 4/4]
     [5/1 5/2 5/3 5/4]
     [6/1 6/2 6/3 6/4]])
 (= (class (__ (juxt bit-or bit-xor)))
    (class (__ (juxt quot mod) 13 21))
    (class (lazy-seq)))
 (= (class (nth (__ (constantly 10946)) 34))
    (class (nth (__ (constantly 0) 5 8) 55))
    (class (lazy-seq)))
 (= (let [m 377 n 610 w 987
          check (fn [f s] (every? true? (map-indexed f s)))
          row (take w (nth (__ vector) m))
          column (take w (map first (__ vector m n)))
          diagonal (map-indexed #(nth %2 %) (__ vector m n w w))]
      (and (check #(= %2 [m %]) row)
           (check #(= %2 [(+ m %) n]) column)
           (check #(= %2 [(+ m %) (+ n %)]) diagonal)))
    true))


;; ### Problems 169-170 do not exist

;; ### Problem 171: <a href="http://www.4clojure.com/problem/171">Intervals</a>
(problem 171
  (fn [l]
    (if-not (seq l)
      []
      (let [sorted (distinct (sort l))]
        (loop [s sorted, ret [], a (first s), b a]
          (if (seq s)
            (if (> (first s) (inc b))
              (recur (rest s)
                     (conj ret [a b])
                     (first s)
                     (first s))
              (recur (rest s)
                     ret
                     a
                     (first s)))
            (conj ret [a b]))))))
  (= (__ [1 2 3]) [[1 3]])
  (= (__ [1 2 3]) [[1 3]])
  (= (__ [10 9 8 1 2 3]) [[1 3] [8 10]])
  (= (__ [1 1 1 1 1 1 1]) [[1 1]])
  (= (__ []) [])
  (= (__ [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
     [[1 4] [6 6] [9 11] [13 17] [19 19]]))


;; ### Problem 172 does not exist


;; ### Problem 173: <a href="http://www.4clojure.com/problem/173">Intro to Destructuring 2</a>
;; Also not amenable to the "problem" macro; I have just added the
;; solution, commented out, and the test cases:
(problem 173
  ;; f x   <--- solution
  (= 3
     (let [[__] [+ (range 3)]] (apply __))
     (let [[[__] b] [[+ 1] 2]] (__ b))
     (let [[__] [inc 2]] (__))))


;; ### Problems 174-176 don't exist


;; ### Problem 177: <a href="http://www.4clojure.com/problem/177">Balancing Brackets</a>
;;
;; After a couple of tries on this I realized that stripping out
;; non-parenthetic characters and then successively eliminating empty,
;; balanced pairs of parens/braces/brackets would do the trick.
(problem 177
  (fn [s]
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


;; ### Problem 178: <a href="http://www.4clojure.com/problem/178">Best Hand</a>
;;
;; In this problem, it's most straightforward to use `cond` to order
;; the card priorities.  The common pattern in recognizing hands is to
;; look at the frequencies of either the first or the second element
;; in each "card" (two-character suit-and-value combo).  `freqs` does
;; this.
;;
;; The only tricky bit is to check for straights; in this case, since
;; aces can be either next to 2s or next to kings, two orders are
;; tested for (`order1` and `order2`).  Other strategies might be used
;; (such as aligning a sequence of cards along the output of `cycle`
;; of a single order sequence), but this one seemed most
;; straightforward.
(defn best-hand [h]
  (letfn [(straight? [s]
            (let [order1 "A23456789TJQK"
                  order2 "23456789TJQKA"
                  vals (map second s)
                  sorter (fn [order]
                           (->> order
                                (map-indexed (fn [a i] [i a]))
                                (into {})))
                  checker (fn [sorter]
                            (->> vals
                                 (sort-by sorter)
                                 (reverse)
                                 (map sorter)
                                 (partition 2 1)
                                 (map (partial apply -))
                                 set
                                 count
                                 (= 1)))]
              (or (checker (sorter order1))
                  (checker (sorter order2)))))

          (freqs [pos s]
            (->> s
                 (map pos)
                 frequencies
                 (map second)))
          (flush? [s]
            (->> s (freqs first) (some #{5})))
          (straight-flush? [s]
            (and (flush? s) (straight? s)))
          (four-of-a-kind? [s]
            (->> s (freqs second) (some #{4})))
          (full-house? [s]
            (->> s (freqs second) set (= #{2 3})))
          (three-of-a-kind? [s]
            (->> s (freqs second) (some #{3})))
          (two-pair? [s]
            (->> s
                 (freqs second)
                 frequencies
                 (#(get % 2))
                 (= 2)))
          (pair? [s]
            (->> s (freqs second) (some #{2})))]
    (cond (straight-flush? h)  :straight-flush
          (four-of-a-kind? h)  :four-of-a-kind
          (full-house? h)      :full-house
          (flush? h)           :flush
          (straight? h)        :straight
          (three-of-a-kind? h) :three-of-a-kind
          (two-pair? h)        :two-pair
          (pair? h)            :pair
          :else                :high-card)))


(problem 178
  best-hand
  (= :high-card (__ ["HA" "D2" "H3" "C9" "DJ"]))
  (= :pair (__ ["HA" "HQ" "SJ" "DA" "HT"]))
  (= :two-pair (__ ["HA" "DA" "HQ" "SQ" "HT"]))
  (= :three-of-a-kind (__ ["HA" "DA" "CA" "HJ" "HT"]))
  (= :straight (__ ["HA" "DK" "HQ" "HJ" "HT"]))
  (= :straight (__ ["HA" "H2" "S3" "D4" "C5"]))
  (= :flush (__ ["HA" "HK" "H2" "H4" "HT"]))
  (= :full-house (__ ["HA" "DA" "CA" "HJ" "DJ"]))
  (= :four-of-a-kind (__ ["HA" "DA" "CA" "SA" "DJ"]))
  (= :straight-flush (__ ["HA" "HK" "HQ" "HJ" "HT"])))


;; ### Problems 179-194 don't exist


;; ### Problem 195: <a href="http://www.4clojure.com/problem/195">Parentheses... Again</a>
;;
;; I first tried to solve this problem by generating all combinations
;; of \\(2n\\) parentheses and selecting only those that balanced.
;; This passed the tests, but, not surprisingly, exceeded the time limit.
;;
;; The required insight was to realize (again: c.f.
;; [Problem #177](http://www.4clojure.com/problem/177)) that one definition of a
;; balanced set of parentheses is one in which can be built up
;; _by inserting balanced pairs of parens_ at any point.  All that's required
;; then to build up strings of matching parentheses, of length \\(n+2\\),
;; from a set of length \\(n\\), is to insert `()` at every point in each
;; string from that set, then removing duplicates using the `set` function.
;;
;; In other words, if on Iteration 1 I have `()`, in Iteration 2 I
;; will have `()()` (inserting in position 0), `(())` (inserting in
;; position 1), and again `()()` (inserting in position 2).  The given
;; solution just encodes that, building the collections up over a sequence of
;; iterations.
(problem 195
 (fn [n]
   (letfn [(splice-parens-in [s n]
             (str (subs s 0 n)
                  "()"
                  (subs s n (count s))))
           (splice-parens-at-all-positions [s]
             (set (map (partial splice-parens-in s) (range (inc (count s))))))]
     (nth (iterate (comp set (partial mapcat splice-parens-at-all-positions))
                   #{""})
          n)))
 (= [#{""} #{"()"} #{"()()" "(())"}] (map (fn [n] (__ n)) [0 1 2]))
 (= #{"((()))" "()()()" "()(())" "(())()" "(()())"} (__ 3))
 (= 16796 (count (__ 10)))
 (= (nth (sort (filter #(.contains ^String % "(()()()())")
                       (__ 9))) 6) "(((()()()())(())))")
 (= (nth (sort (__ 12)) 5000) "(((((()()()()()))))(()))"))

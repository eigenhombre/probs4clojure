(ns probs4clojure.core
  (:use expectations))


;; The Big Divide
;;
;; Difficulty:Medium
;; Topics:math
;;
;;
;; Write a function which calculates the sum of all natural numbers
;; under n (first argument) which are evenly divisible by at least one
;; of a and b (second and third argument). Numbers a and b are
;; guaranteed to be coprimes.
;;
;; Note: Some test cases have a very large n, so the most obvious
;; solution will exceed the time limit.


(defn gg
  [T a b]
  (letfn
      [(sumrange [m] (/ (* m (inc m)) 2))
       (sumproducts [T a]
         (* a (sumrange (bigint (/ (dec (bigint T)) a)))))]
    (- (+ (sumproducts T a) (sumproducts T b))
       (sumproducts T (* a b)))))

(expect 0 (gg 3 17 11))

(expect 23 (gg 10 3 5))

(expect 233168 (gg 1000 3 5))

(expect "2333333316666668"
        (str (gg 100000000 3 5)))

(expect "110389610389889610389610"
        (str (gg (* 10000 10000 10000) 7 11)))


(expect "1277732511922987429116"
        (str (gg (* 10000 10000 10000) 757 809)))

(expect "4530161696788274281"
        (str (gg (* 10000 10000 1000) 1597 3571)))



;; Insert between two items
;;
;; Difficulty:Medium
;; Topics:seqs core-functions
;;
;;
;; Write a function that takes a two-argument predicate, a value, and
;; a collection; and returns a new collection where the value is
;; inserted between every two items that satisfy the predicate.
;;
;; (= '(1 :less 6 :less 7 4 3) (__ < :less [1 6 7 4 3]))
;;
;; (= '(2) (__ > :more [2]))
;;
;; (= [0 1 :x 2 :x 3 :x 4]  (__ #(and (pos? %) (< % %2)) :x (range 5)))
;;
;; (empty? (__ > :more ()))
;;
;; (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
;;    (take 12 (->> [0 1]
;;                  (iterate (fn [[a b]] [b (+ a b)]))
;;                  (map first) ; fibonacci numbers
;;                  (__ (fn [a b] ; both even or both odd
;;                        (= (mod a 2) (mod b 2)))
;;                                           :same))))

(defn f [pred kw s]
  (if (seq s)
    (let [p (partition 2 1 s)]
      (concat [(first s)]
              (apply concat (for [[a b] p]
                              (if (pred a b)
                                [kw b]
                                [b])))))
    ()))

(expect '(1 :less 6 :less 7 4 3) (f < :less [1 6 7 4 3]))
(expect '(2) (f > :more [2]))
(expect [0 1 :x 2 :x 3 :x 4]  (f #(and (pos? %) (< % %2)) :x (range 5)))
(expect (empty? (f > :more ())) true)
(expect [0 1 :same 1 2 3 :same 5 8 13 :same 21]
        (take 12 (->> [0 1]
                      (iterate (fn [[a b]] [b (+ a b)]))
                      (map first) ; fibonacci numbers
                      (f (fn [a b] ; both even or both odd
                           (= (mod a 2) (mod b 2)))
                         :same))))




;  Subset and Superset
;   
;  Difficulty:	Elementary
;  Topics:	set-theory
;  
;  
;  Set A is a subset of set B, or equivalently B is a superset of A, if 
;  A is "contained" inside B. A and B may coincide.
;  	
;  (clojure.set/superset? __ #{2})
;  	
;  (clojure.set/subset? #{1} __)
;  	
;  (clojure.set/superset? __ #{1 2})
;  	
;  (clojure.set/subset? #{1 2} __)

#{1 2}

;  Given any number of sequences, each sorted from smallest to
;  largest, find the smallest number which appears in each
;  sequence. The sequences may be infinite, so be careful to search
;  lazily.
;
;
;
;  (= 3 (__ [3 4 5]))
;
;  (= 4 (__ [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
;
;  (= 7 (__ (range) (range 0 100 7/6) [2 3 5 7 11 13]))
;
;  (= 64 (__ (map #(* % % %) (range)) ;; perfect cubes
;            (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
;            (iterate inc 20))) ;; at least as large as 20

;(defn d [seqs]
;  (let [pairs (zipmap (map first seqs) seqs)
;        firsts (map first seqs)
;        least (apply min firsts)
;        most (apply max firsts)]
;    (if (apply = firsts)
;      (first firsts)
;      (recur (apply (partial drop2most most))))))

;(defn xc [& seqs]
;  (d seqs))

(defn c [& seqs]
  (let [firsts (map first seqs)
        biggest (apply max firsts)
        trim #(if (> biggest (first %)) (rest %) %)]
    (if (apply = firsts)
      (first firsts)
      (recur (map trim seqs)))))

(expect 4 (c [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
(expect 7 (c (range) (range 0 100 7/6) [2 3 5 7 11 13]))
(expect 3 (c [3 4 5]))
(expect 64 (c (map #(* % % %) (range)) ;; perfect cubes
            (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
            (iterate inc 20))) ;; at least as large as 20

;  Digits and bases
;
;  Difficulty:	Medium
;  Topics:	math
;
;
;  Write a function which returns a sequence of digits of a non-negative number
;  (first argument) in numerical system with an arbitrary base (second argument).
;  Digits should be represented with their integer values, e.g. 15 would be
;  [1 5] in base 10, [1 1 1 1] in base 2 and [15] in base 16.
;
;  (= [1 2 3 4 5 0 1] (__ 1234501 10))
;
;  (= [0] (__ 0 11))
;
;  (= [1 0 0 1] (__ 9 2))
;
;  (= [1 0] (let [n (rand-int 100000)](__ n n)))
;
;  (= [16 18 5 24 15 1] (__ Integer/MAX_VALUE 42))

(def f
  (fn [n base]
    (loop [n n
           ret []]
      (let [dig (rem n base)
            nxt (int (Math/floor (/ n base)))]
        (if (zero? nxt)
          (cons dig ret)
          (recur nxt (cons dig ret))))))
  )

(expect [1 2 3 4 5 0 1] (f 1234501 10))
(expect [16 18 5 24 15 1] (f Integer/MAX_VALUE 42))




;  Indexing Sequences
;
;  Difficulty:	Easy
;  Topics:	seqs
;
;
;  Transform a sequence into a sequence of pairs containing the original
;  elements along with their index.
;
;  (= (__ [:a :b :c]) [[:a 0] [:b 1] [:c 2]])
;
;  (= (__ [0 1 3]) '((0 0) (1 1) (3 2)))
;
;  (= (__ [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])

;;; HINT: http://stackoverflow.com/questions/4830900/how-do-i-find-the-index-of-an-item-in-a-vector



(def x (fn [l]
         (for [[x y] (map-indexed vector l)]
           [y x])))

(println (for [[x y] (map-indexed vector [:a :b :c])]
           [y x]))
(println (x [:a :b :c]))

(expect (x [:a :b :c]) [[:a 0] [:b 1] [:c 2]])
(expect (x [0 1 3]) '((0 0) (1 1) (3 2)))


;Map Defaults
;
;Difficulty:	Elementary
;Topics:	seqs
;
;
;When retrieving values from a map, you can specify default values in case
;the key is not found:
;
;(= 2 (:foo {:bar 0, :baz 1} 2))
;
;However, what if you want the map itself to contain the default values?
;Write a function which takes a default value and a sequence of keys and
;constructs a map.
;	
;(= (__ 0 [:a :b :c]) {:a 0 :b 0 :c 0})
;	
;(= (__ "x" [1 2 3]) {1 "x" 2 "x" 3 "x"})
;	
;(= (__ [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})


(def f (fn f [default keys]
        (let [vals (repeat default)]
          (zipmap keys vals))))

(expect (f 0 [:a :b :c]) {:a 0 :b 0 :c 0})

(expect (f "x" [1 2 3]) {1 "x" 2 "x" 3 "x"})


; Logical falsity and truth
;
;Difficulty:	Elementary
;Topics:	logic
;
;
;In Clojure, only nil and false representing the values of logical falsity in conditional tests - anything else is logical truth.
;	
;(= __ (if-not false 1 0))
;	
;(= __ (if-not nil 1 0))
;	
;(= __ (if true 1 0))
;	
;(= __ (if [] 1 0))
;	
;(= __ (if [0] 1 0))
;	
;(= __ (if 0 1 0))
;	
;(= __ (if 1 1 0))

1

;; Identify keys and values
;;
;; Difficulty:Medium
;; Topics:maps seqs
;;
;;
;; Given an input sequence of keywords and numbers, create a map such
;; that each key in the map is a keyword, and the value is a sequence of
;; all the numbers (if any) between it and the next keyword in the
;; sequence.

;; (= {} (__ []))

;; (= {:a [1]} (__ [:a 1]))

;; (= {:a [1], :b [2]} (__ [:a 1, :b 2]))

;; (= {:a [1 2 3], :b [], :c [4]} (__ [:a 1 2 3 :b :c 4]))


(defn unflatten-map [s]
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



;; The Balance of N
;;
;; Difficulty:Medium
;; Topics:math
;;
;; A balanced number is one whose component digits have the same sum
;; on the left and right halves of the number. Write a function which
;; accepts an integer n, and returns true iff n is balanced.

;; (= true (__ 11))

;; (= true (__ 121))

;; (= false (__ 123))

;; (= true (__ 0))

;; (= false (__ 88099))

;; (= true (__ 89098))

;; (= true (__ 89089))

;; (= (take 20 (filter __ (range)))
;;    [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])


(defn balanced [n]
  (let [digits (map #(Integer/parseInt (str %)) (str n))
        len (count digits)
        pivot (int (/ len 2))
        [l r] (split-at pivot digits)
        r (if (even? len) r (rest r))]
    (= (apply + l) (apply + r))))

(expect true (balanced 11))

(expect (take 20 (filter balanced (range)))
        [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])


;; Difficulty:Easy
;; Topics:set-theory
;; Given a set of sets, create a function which returns true if no two
;; of those sets have any elements in common1 and false
;; otherwise. Some of the test cases are a bit tricky, so pay a little
;; more attention to them.

;; 1 Such sets are usually called pairwise disjoint or mutually
;; disjoint.

;; (= (__ #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
;;    true)

;; (= (__ #{#{:a :b :c :d :e}
;;          #{:a :b :c :d}
;;          #{:a :b :c}
;;          #{:a :b}
;;          #{:a}})
;;    false)

;; (= (__ #{#{[1 2 3] [4 5]}
;;          #{[1 2] [3 4 5]}
;;          #{[1] [2] 3 4 5}
;;          #{1 2 [3 4] [5]}})
;;    true)

;; (= (__ #{#{'a 'b}
;;          #{'c 'd 'e}
;;          #{'f 'g 'h 'i}
;;          #{''a ''c ''f}})
;;    true)

;; (= (__ #{#{'(:x :y :z) '(:x :y) '(:z) '()}
;;          #{#{:x :y :z} #{:x :y} #{:z} #{}}
;;          #{'[:x :y :z] [:x :y] [:z] [] {}}})
;;    false)

;; (= (__ #{#{(= "true") false}
;;          #{:yes :no}
;;          #{(class 1) 0}
;;          #{(symbol "true") 'false}
;;          #{(keyword "yes") ::no}
;;          #{(class '1) (int \0)}})
;;    false)

;; (= (__ #{#{distinct?}
;;          #{#(-> %) #(-> %)}
;;          #{#(-> %) #(-> %) #(-> %)}
;;          #{#(-> %) #(-> %) #(-> %)}})
;;    true)

;; (= (__ #{#{(#(-> *)) + (quote mapcat) #_ nil}
;;          #{'+ '* mapcat (comment mapcat)}
;;          #{(do) set contains? nil?}
;;          #{, , , #_, , empty?}})
;;       false)

(def samp1 #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})

(def samp2 #{#{:a :b :c :d :e}
             #{:a :b :c :d}
             #{:a :b :c}
             #{:a :b}
             #{:a}})

(def samp3 #{#{[1 2 3] [4 5]}
             #{[1 2] [3 4 5]}
             #{[1] [2] 3 4 5}
             #{1 2 [3 4] [5]}})

(def samp4 #{#{(#(-> *)) + (quote mapcat) #_ nil}
             #{'+ '* mapcat (comment mapcat)}
             #{(do) set contains? nil?}
             #{, , , #_, , empty?}})

(defn in?
  [seq elm]
  (some #(= elm %) seq))

(def f
  (fn [ss]
    (empty?
     (for [ss0 ss
           ss1 ss :when (not (= ss0 ss1))
           el ss0 :when (some #(= el %) ss1)]
       el))))

(expect (f samp1) true)
(expect (f samp2) false)
(expect (f samp3) true)
(expect (f samp4) false)




;; Power set

"
Difficulty:Medium
Topics:set-theory


Write a function which generates the power set of a given set. The
power set of a set x is the set of all subsets of x, including the
empty set and x itself.

(= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})

(= (__ #{}) #{#{}})

(= (__ #{1 2 3})
   #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})

(= (count (__ (into #{} (range 10)))) 1024)

"

(defn pwrset [S]
  (let [pwr2 (fn [n] (apply * (repeat n 2)))
        has-bit-set (fn [num digit]
                      (> (bit-and num (pwr2 digit)) 0))
        nitems (count S)
        itemlist (apply list S)
        num_combinations (pwr2 nitems)]
    (set
     (for [c (range num_combinations)]
       (set (for [i (range nitems) :when (has-bit-set c i)]
              (nth itemlist i)))))))

(println (combi #{:a :b :c}))


(defn pwr2 [n] (apply * (repeat n 2)))

(defn has-bit-set [num digit]
  (> (bit-and num (pwr2 digit)) 0))

(expect (pwr2 2) 4)
(expect (pwr2 10) 1024)
(expect (has-bit-set 0 0) false)
(expect (has-bit-set 1 0) true)
(expect (map (partial has-bit-set 0xFF) (range 8)) (repeat 8 true))

(defn works [S]  ;; But requires S of length 3!!!
  (set
   (for [tf1 [true false], e1 S :when tf1,
         tf2 [true false], e2 S :when tf2,
         tf3 [true false], e3 S :when tf3]
     (set [e1 e2 e3]))))


       
;; Equivalence relations

"
(= (__ #(* % %) #{-2 -1 0 1 2})
   #{#{0} #{1 -1} #{2 -2}})

(= (__ #(rem % 3) #{0 1 2 3 4 5 })
   #{#{0 3} #{1 4} #{2 5}})

(= (__ identity #{0 1 2 3 4})
   #{#{0} #{1} #{2} #{3} #{4}})

(= (__ (constantly true) #{0 1 2 3 4})
   #{#{0 1 2 3 4}})
"

(defn equiv
  [f D]
  (set
   (for [a D]
     (set
      (for [b D :when (= (f a) (f b))]
        b)))))

(expect (equiv #(* % %) #{-2 -1 0 1 2})
        #{#{0} #{1 -1} #{2 -2}})

(expect (equiv #(rem % 3) #{0 1 2 3 4 5 })
        #{#{0 3} #{1 4} #{2 5}})

(expect (equiv identity #{0 1 2 3 4})
        #{#{0} #{1} #{2} #{3} #{4}})

(expect (equiv (constantly true) #{0 1 2 3 4})
        #{#{0 1 2 3 4}})


;; Reimplement trampoline

(def tramp
  (fn tramp
    [f & rest]
    (if (fn? f)
      (recur (apply f rest) nil)
      f)))

(expect [1 3 5 7 9 11]
        (letfn
            [(foo [x y] #(bar (conj x y) y))
             (bar [x y] (if (> (last x) 10)
                          x
                          #(foo x (+ 2 y))))]
          (trampoline foo [] 1)))

(expect [1 3 5 7 9 11]
        (letfn
            [(foo [x y] #(bar (conj x y) y))
             (bar [x y] (if (> (last x) 10)
                          x
                          #(foo x (+ 2 y))))]
          (tramp foo [] 1)))

(expect 1 1)


;; Euler's totient

(def totient-f
  (fn totient-f [x]
    (let [gcd (fn [a b]
                (let [aa (max a b)
                      bb (min a b)]
                  (if (zero? b)
                    a
                    (recur b (mod a b))))),
          coprime (fn [a b]
                    (= (gcd a b) 1))]
      (if (= x 1)
        1
        (let [r (range 1 x)
              coprimes (filter #(coprime % x) r)]
          (count coprimes))))))

(expect (totient-f 1) 1)
(expect (totient-f 10) 4)
(expect (totient-f 40) 16)
(expect (totient-f 99) 60)


;; Happy numbers

(def is-happy
  (fn is-happy
    ([n] (is-happy n []))
    ([n seen]
       (let [digits (fn [x]
                      (map #(Integer/parseInt (str %))
                           (seq (str x)))),
             sumsquares (fn [n]
                          (let [digs (digits n)]
                            (reduce + (map #(* % %) digs)))),
             is-in (fn [y l] (some #(= % y) l)),
             ss (sumsquares n)]
         (cond
          (= 1 ss) true
          (is-in ss seen) false
          :else (recur ss (conj seen ss)))))))

(expect (is-happy 3) false)
(expect (is-happy 2) false)
(expect (is-happy 7) true)
(expect (is-happy 986543210) true)


;; intoCamelCase

((fn [s]
   (let [words (re-seq #"[^-]+" s)]
       (apply str (cons (first words)
                        (for [w (rest words)]
                          (str (.toUpperCase (str (first w))) (apply str (rest w))))))))
     "multi-word-key")


;; Merge with a function

((fn [fun & maps]
      (let [pairs (apply concat
                         (for [mm maps]
                           (for [[k, v] mm] [k v])))]
        (loop [p pairs, ret {}]
          (if (seq p)
            (let [[k, v] (first p),
                  lookup (ret k)
                  insert (if lookup (fun lookup v) v)]
              (recur (rest p)
                     (conj ret (assoc {} k insert))))
            ret))))
     - {1 10, 2 20} {1 3, 2 10, 3 15})


;; Anagrams

(
  (fn [words]
    (let [is-anagram #(= (frequencies (seq %1))
                         (frequencies (seq %2))),
          S (set (for [w2 words] (set (for [w1 words :when (is-anagram w2 w1)] w1))))]
      (set (for [s S :when (> (count s) 1)] s)))) ["veer" "lake" "item" "kale" "mite" "ever"])

;; Sequence reductions

((fn redu
   ([f i s]
     (if (seq s)
       (lazy-seq
         (cons i
               (redu f (f i (first s)) (rest s))))
       [i]))
   ([f s]
     (redu f (first s) (rest s))))
  conj [1] [2 3 4])



;; Black Box Testing

(map (fn [s]
   (let [r (conj s [4 5] [5 4] [5 5]),
         a (- (count r) (count s)),
         b (first r),
         c (last r)]
     (cond (= a 2) :map,
           (and (= a 3) (= b [5 4])) :set,
           (and (= a 3) (= c [5 5])) :vector,
           (and (= a 3) (= b [5 5])) :list,
           :true :something-else))) [{:a 1, :b 2} #{:a 1 :b 2} [:a 1 :b 2] '(:a 1 :b 2) [1 2 3 4 5 6]])



; Perfect numbers

((fn perfect [x] (= (apply + (for [f (range 1 x) :when (= 0 (rem x f))] f)) x)) 8128)



;  Filter Perfect Squares
;
;  Difficulty:	Medium
;  Topics:	
;
;
;  Given a string of comma separated integers, write a function which
;  returns a new comma separated string that only contains the numbers
;  which are perfect squares.
;
;  (= (__ "4,5,6,7,8,9") "4,9")
;
;  (= (__ "15,16,25,36,37") "16,25,36")

(fn [S]
  (apply str (interpose ","
                        (for [[x, y] (map (fn [s]
                                            (let [i (Integer/parseInt s),
                                                  rx (Math/sqrt i),
                                                  ri (int rx)]
                                              [i (= (* ri ri) i)])) (re-seq #"\d+" S)) :when y] x))))




; Answer from Repl (obvious upon inspection):
[1 3 5 7 9 11]

;  Intro to Trampoline
;
;  Difficulty:	Medium
;  Topics:	recursion
;
;
;  The trampoline function takes a function f and a variable number of
;  parameters. Trampoline calls f with any parameters that were
;  supplied. If f returns a function, trampoline calls that function
;  with no arguments. This is repeated, until the return value is not
;  a function, and then trampoline returns that non-function
;  value. This is useful for implementing mutually recursive
;  algorithms in a way that won't consume the stack.
;
;  (= __
;     (letfn
;       [(foo [x y] #(bar (conj x y) y))
;        (bar [x y] (if (> (last x) 10)
;                     x
;                     #(foo x (+ 2 y))))]
;       (trampoline foo [] 1)))


;  Prime Numbers
;
;  Difficulty:	Medium
;  Topics:	primes
;
;
;  Write a function which returns the first x number of prime numbers.
;
;  (= (__ 2) [2 3])
;
;  (= (__ 5) [2 3 5 7 11])
;
;  (= (last (__ 100)) 541)



;; This works, but for some reason 4clojure times out on the last test:

((fn [m]
     (take m
           (letfn [(prime? [n]
                           (empty?
                             (take 1
                                   (for [x (range 2 n)
                                         :when (zero? (rem n x))]
                                     x))))]
                  (for [v (range 600)
                        :when (and (> v 1) (prime? v))] v))))
     100)

;; Trying now -- loops infinitely
 ((fn [m]
     (take m
           (letfn [(prime? [n]
                           (empty?
                             (take 1
                                   (for [x (range 2 n)
                                         :when (zero? (rem n x))]
                                     x))))]
                  (lazy-seq
                    (loop [v 2, ret []]
                      (recur (inc v) (if (prime? v) (concat [v] ret) ret)))))))
     100)

((fn [m]
     (take m
           (letfn [(prime? [prev n]
                           (empty?
                             (take 1
                                   (for [x (range prev n)
                                         :when (zero? (rem n x))]
                                     x))))]
                  (map prime? (range)))))
     100)

;; Another try -- WORKS!!

((fn [m]
   (take m
         (for [n (range)
               :when (and
                       (> n 1)
                       (loop [x 2]
                         (cond
                           (= n 2) true
                           (zero? (rem n x)) false
                           (= x (dec n)) true
                           :else (recur (inc x)))))] n)))
  100)



(def noprintln (fn [&rest]))

;  Word Sorting
;
;  Difficulty:	Medium
;  Topics:	sorting
;
;
;  Write a function that splits a sentence up into a sorted list of
;  words. Capitalization should not affect sort order and punctuation
;  should be ignored.
;
;  (= (__  "Have a nice day.")
;     ["a" "day" "Have" "nice"])
;
;  (= (__  "Clojure is a fun language!")
;     ["a" "Clojure" "fun" "is" "language"])
;
;  (= (__  "Fools fall for foolish follies.")
;     ["fall" "follies" "foolish" "Fools" "for"])

((fn [str]
   (sort (fn [a b] (.compareToIgnoreCase a b))
         (re-seq #"[a-zA-Z]+" str)))
  "Clojure is a fun language!")

;  Partition a Sequence
;
;  Difficulty:	Medium
;  Topics:	seqs core-functions
;
;
;  Write a function which returns a sequence of lists of x items each. Lists of less than x items should not be returned.
;
;  (= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
;
;  (= (__ 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
;
;  (= (__ 3 (range 8)) '((0 1 2) (3 4 5)))

(fn [n s]
      (let [nslices (quot (count s) n)]
        (for [slice (range nslices)]
          (subvec (vec s) (* slice n) (+ (* slice n) n)))))


;  Juxtaposition
;
;  Difficulty:	Medium
;  Topics:	higher-order-functions core-functions
;
;
;  Take a set of functions and return a new function that takes a variable
;  number of arguments and returns a sequence containing the result of
;  applying each function left-to-right to the argument list.
;
;  (= [21 6 1] ((__ + max min) 2 3 5 1 6 4))
;
;  (= ["HELLO" 5] ((__ #(.toUpperCase %) count) "hello"))
;
;  (= [2 6 4] ((__ :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))

; Solution:
(fn [& fargs]
       (fn [& args]
         (loop [f fargs,
                ret []]
           (if (seq f)
             (recur (rest f) (conj ret (apply (first f) args)))
             ret))))


;  Function Composition
;
;  Difficulty:	Medium
;  Topics:	higher-order-functions core-functions
;
;
;  Write a function which allows you to create function
;  compositions. The parameter list should take a variable number of
;  functions, and create a function applies them from right-to-left.
;
;  (= [3 2 1] ((__ rest reverse) [1 2 3 4]))
;
;  (= 5 ((__ (partial + 3) second) [1 2 3 4]))
;
;  (= true ((__ zero? #(mod % 8) +) 3 5 7 9))
;
;  (= "HELLO" ((__ #(.toUpperCase %) #(apply str %) take) 5 "hello world"))

(noprintln
  (((fn [& funcargs]
    (fn [& args]
     (loop [f (rest (reverse funcargs)),
            result (apply (last funcargs) args)]
       (if (seq f)
         (recur (rest f) ((first f) result))
         result))))
   (partial + 3) second) [1 2 3 4]))


;; Find distinct items

(println
  ((fn [sarg]
     (loop [s sarg,
            ret []
            seen {}]
       (if (seq s)
         (recur (rest s)
                (if (seen (first s))
                  ret
                  (conj ret (first s)))
                (assoc seen (first s) true))
         ret)))
  [:a :a :b :b :c :c]))


;  (= (__ [1 2 1 3 1 2 4]) [1 2 3 4])
;
;  (= (__ [:a :a :b :b :c :c]) [:a :b :c])
;
;  (= (__ '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
;
;  (= (__ (range 50)) (range 50))

;; Count occurrences

(noprintln
  ((fn [sarg]
     (loop [s sarg,
            ret {}]
       (if (seq s)
         (do (println s ret)
           (recur (rest s)
                  (assoc ret
                         (first s)
                         (inc (get ret (first s) 0)))))
         ret)))
    [1 1 2 3 2 1 1]))


;  (= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})

;  (= (__ [:b :a :b :a :b]) {:a 2, :b 3})

;  (= (__ '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})


;; Split by type

(noprintln
  ((fn [l]
     (vals (loop [in (for [x l] [x (type x)]),
            dict {}]
       (if (seq in)
         (recur (rest in) (assoc dict (second (first in))
                                 (concat (dict (second (first in)))
                                         [(first (first in))])))
         dict))))
    [1 :a 2 :b 3 :c]))


;; FAILED split by type
(noprintln (set
  ((fn [l]
    (let [m (apply hash-map (apply concat (for [x l] [x (type x)]))),
            v (set (vals m))]
        (for [typ v]
          (vec (for [k (keys m) :when (= (m k) typ)] k)))))
    [1 :a 2 :b 3 :c])))

(noprintln ((fn [l m]
            (for [k (range m)]
              (for [[i j] (map-indexed vector l) :when (zero? (rem (- i k) m))] j)))
           (range 10) 5))


;; Rotate sequence
(noprintln
  ((fn [n l]
     (loop [nn n,
            ret l]
       (cond
         (= nn 0) ret
         (> nn 0) (recur (- nn 1)
                        (concat (rest ret) [(first ret)]))
         :else (recur (+ nn 1)
                      (concat [(last ret)] (reverse (rest (reverse ret))))))))
         -2 [1 2 3 4 5]))


;; Pascal's Trapezoid

(noprintln
  (take 4
        ((fn [outer]
           (let [nxt (fn [s]
                       (concat [(first s)]
                               (loop [ss s, ret []]
                                 (let [[a & b] ss]
                                   (if (seq ss)
                                     (recur (rest ss) (conj ret (if (nil? b)
                                                                  a
                                                                  (+ a (first b)))))
                                     ret)))))]
             (iterate nxt outer))) [3 1 2])))

(noprintln
  ((fn [s]
     (concat [(first s)]
             (loop [ss s, ret []]
               (let [[a & b] ss]
                 (if (seq ss)
                   (recur (rest ss) (conj ret (if (nil? b)
                                                a
                                                (+ a (first b)))))
                   ret)))))
    [3 1 2]))


;; Dot product

(noprintln ((fn [a b]
            (loop [aa a,
                   bb b,
                   ret 0]
              (if (seq aa)
                (recur (rest aa) (rest bb) (+ ret (* (first aa) (first bb))))
                ret)))
           [0 1 0] [1 1 1]))

(noprintln ((fn [s]  ;;
            (let [[val l r] s
                  swp (fn swap [t]
                        (let [[v l r] t]
                          (if
                            (nil? v) t
                            [v (swap r) (swap l)])))]
              (= l (swp r))))
           '(:a (:b nil nil) (:b nil nil))))

(noprintln
  ((fn [SS]
     (let [suit ({\D :diamond,
                  \H :heart,
                  \C :club,
                  \S :spade} (first SS))
           rank ({\2 0
                  \3 1
                  \4 2
                  \5 3
                  \6 4
                  \7 5
                  \8 6
                  \9 7
                  \T 8
                  \J 9
                  \Q 10
                  \K 11
                  \A 12}
                  (second SS))] {:suit suit, :rank rank}))
    "DQ"))

(noprintln
  ((fn f
     ([a op b] (op a b))
     ([a op b & rst]
       (apply f (op a b) rst)))
    38 + 48 - 2 / 2))

(noprintln
  ((fn mymap2 [f s]
     (lazy-seq
       (if (seq s)
         (cons (f (first s)) (mymap2 f (rest s))))))
    inc (range 10)))

(noprintln
  ((fn mymap [f s]
    (lazy-seq
      (loop [cur s
             ret []]
        (if (seq cur)
          (recur (rest cur) (conj ret (f (first cur))))
          ret))))
  inc (range 10)))


(noprintln ((fn dolcm [& r]
            (letfn [(lcm [a & rst]
                         (/ (reduce * a rst)
                            (reduce gcd1 a rst)))
                    (gcd1 [a b]
                          (let [aa (max a b)
                                bb (min a b)]
                            (if (zero? b)
                              a
                              (recur b (mod a b)))))]
                   (apply lcm r)))
           5 3 7))

(noprintln
  ((fn [s]
     (loop [cur s
            cnt 0]
       (if (not (seq cur))
         cnt
         (let [f (first cur)
               digits (map (comp #(Integer/parseInt %) str) (seq (str f)))
               dsq (map #(* % %) digits)
               sumsq (apply + dsq)]
           (recur (rest cur) (if (< f sumsq) (+ cnt 1) cnt))))))
    (range 100)))

(noprintln
  (let [x java.lang.Class]
    (and (= (class x) x) x)))

(noprintln
  ((fn treep [s]
     (or (nil? s)
         (and (coll? s)
              (= (count s) 3)
              (treep (nth s 1))
              (treep (nth s 2)))))
    [1 nil [2 [3 nil nil] [4 nil nil]]]))

(noprintln
  ((fn [s]
     (loop [in (map (comp #(Integer/parseInt %) str) (seq s)),
            pwr 0,
            ret 0]
       (if (seq in)
         (recur (rest in) (+ pwr 1) (+ (first in) (bit-shift-left ret 1)))
         ret)))
    "1111"))

(noprintln
  ((fn [n]
     (last
       (take n
             (iterate
               (fn [l]
                 (loop [cur l,
                        ret [1]]
                   (if (seq cur)
                     (let [a (or (first cur) 0)
                           b (or (second cur) 0)
                           next (+ a b)]
                       (recur (rest cur) (conj ret next)))
                     ret)))
      [1]))))
    11))

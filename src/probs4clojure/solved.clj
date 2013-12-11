(ns probs4clojure.core
  (:use expectations))


;; #92: Read Roman numerals
;; Difficulty:	Hard
;; Topics:	strings math


;; Roman numerals are easy to recognize, but not everyone knows all
;; the rules necessary to work with them. Write a function to parse a
;; Roman-numeral string and return the number it represents. 

;; You can assume that the input will be well-formed, in upper-case,
;; and follow the subtractive principle. You don't need to handle any
;; numbers greater than MMMCMXCIX (3999), the largest number
;; representable with ordinary letters.
	
;; (= 14 (__ "XIV"))
	
;; (= 827 (__ "DCCCXXVII"))
	
;; (= 3999 (__ "MMMCMXCIX"))
	
;; (= 48 (__ "XLVIII"))
 

(defn read-roman [goal]
  (let [mnums (sorted-map-by >
                             1000 "M"
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
                             1    "I")
        s-starts-with (fn [slong sshort]
                        (= sshort (take (count sshort) slong)))]
    (loop [in (seq goal)
           chain (keys mnums)
           ret 0]
      (if-not (seq in)
        ret
        (let [cnum (first chain)
              cseq (seq (mnums cnum))
              has-it (s-starts-with in cseq)
              next-in (if has-it (drop (count cseq) in) in)
              rc (rest chain)
              next-chain (if (empty? rc) 
                           (keys mnums) 
                           rc)]
          (if-not has-it
            (recur next-in next-chain ret)
            (recur next-in next-chain (+ ret cnum))))))))

 

;; Problem 73: Analyze a Tic-Tac-Toe Board
 
;; Difficulty:	Hard
;; Topics:	game


;; A tic-tac-toe board is represented by a two dimensional vector. X
;; is represented by :x, O is represented by :o, and empty is
;; represented by :e. A player wins by placing three Xs or three Os in
;; a horizontal, vertical, or diagonal row. Write a function which
;; analyzes a tic-tac-toe board and returns :x if X has won, :o if O
;; has won, and nil if neither player has won.
	

;; (= nil (__ [[:e :e :e]
;;             [:e :e :e]
;;             [:e :e :e]]))
	

;; (= :x (__ [[:x :e :o]
;;            [:x :e :e]
;;            [:x :e :o]]))
	

;; (= :o (__ [[:e :x :e]
;;            [:o :o :o]
;;            [:x :e :x]]))
	

;; (= nil (__ [[:x :e :o]
;;             [:x :x :e]
;;             [:o :x :o]]))
	

;; (= :x (__ [[:x :e :e]
;;            [:o :x :e]
;;            [:o :e :x]]))
	

;; (= :o (__ [[:x :e :o]
;;            [:x :o :e]
;;            [:o :e :x]]))
	

;; (= nil (__ [[:x :o :x]
;;             [:x :o :x]
;;             [:o :x :o]]))


((fn winner [board]
   (letfn [(fcombos [board]
             (concat
              ;; by rows
              (for [j (range 3)]
                (for [i (range 3)]
                  ((board i) j)))
              
              ;; by columns
              (for [i (range 3)]
                (for [j (range 3)]
                  ((board i) j)))
              
              ;; crosswise
              (list (for [i (range (count board))]
                      (nth (board i) i)))
              
              (list (for [i (range (count board))]
                      (nth (board i) (- 2 i))))))]
     (cond
      (reduce #(or %1 %2) (map #(apply = (list* :x %)) (fcombos board))) :x
      (reduce #(or %1 %2) (map #(apply = (list* :o %)) (fcombos board))) :o)))

  [[:x :o :x]
   [:o :o :o]
   [:o :x :x]])

(winner [[:x :o :x]
         [:x :o :x]
         [:o :x :x]])

(winner [[:o :e :e]
         [:o :x :e]
         [:o :e :x]])




;; Problem 53
;; Difficulty:	Hard
;; Topics:	seqs


;; Given a vector of integers, find the longest consecutive sub-sequence of increasing numbers. If two sub-sequences have the same length, use the one that occurs first. An increasing sub-sequence must have a length of 2 or greater to qualify.
	

;; (= (__ [1 0 1 2 3 0 4 5]) [0 1 2 3])
	

;; (= (__ [5 6 1 3 2 7]) [5 6])
	

;; (= (__ [2 3 3 4 5]) [3 4 5])
	

;; (= (__ [7 6 5 4]) [])


(def f
  (fn [in]
    (let [pairs (partition-all 2 1 in)
          find-incs (for [[a b] pairs] [a b (= (inc a) b)])
          find-mono (partition-by #(nth % 2) find-incs)
          mono-only (filter #(nth (first %) 2) find-mono)
          lengthmap (group-by count mono-only)
          longest (if-not (seq lengthmap) 0 (apply max (keys lengthmap)))
          best (first (lengthmap longest))]
      (if-not best [] (cons (ffirst best) (map second best))))))

(f [2 3 3 4 5])
(f [7 6 5 4])


;; Problem 171
;; Intervals
;;  
;; Difficulty:	Medium
;; Topics:	
;; 
;; 
;; Write a function that takes a sequence of integers and returns a sequence of "intervals". Each interval is a a vector of two integers, start and end, such that all integers between start and end (inclusive) are contained in the input sequence.
;; 	
;; (= (__ [1 2 3]) [[1 3]])
;; 	
;; (= (__ [10 9 8 1 2 3]) [[1 3] [8 10]])
;; 	
;; (= (__ [1 1 1 1 1 1 1]) [[1 1]])
;; 	
;; (= (__ []) [])
;; 	
;; (= (__ [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
;;        [[1 4] [6 6] [9 11] [13 17] [19 19]])

((fn [l]
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
  [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])


;; 173
;; Intro to Destructuring 2
;;
;; Difficulty:	Easy
;; Topics:	Destructuring
;;
;;
;; Sequential destructuring allows you to bind symbols to parts of
;; sequential things (vectors, lists, seqs, etc.): (let [bindings* ]
;; exprs*) Complete the bindings so all let-parts evaluate to 3.
;;
;; (= 3
;;   (let [[__] [+ (range 3)]] (apply __))
;;   (let [[[__] b] [[+ 1] 2]] (__ b))
;;   (let [[__] [inc 2]] (__)))



(= 3
  (let [[f x] [+ (range 3)]] (apply f x))
  (let [[[f x] b] [[+ 1] 2]] (f x b))
  (let [[f x] [inc 2]] (f x)))


"
#150 Palindromic Numbers -- https://4clojure.com/problem/150

Difficulty:Medium
Topics:seqs math


A palindromic number is a number that is the same when written
forwards or backwards (e.g., 3, 99, 14341).

Write a function which takes an integer n, as its only argument, and
returns an increasing lazy sequence of all palindromic numbers that
are not less than n.

The most simple solution will exceed the time limit!

;; UP sequence: 12 -> 1221
;; DN sequence: 12 -> 121
;; (inc 9)      is     10(2)  (pinc 9)      is     11: DN  magic=1
;; (inc 10)     is     11(2)  (pinc 10)     is     11: UP  magic=1
;; (inc 99)     is    100(3)  (pinc 99)     is    101: DN  magic=10
;; (inc 999)    is   1000(4)  (pinc 999)    is   1001: UP  magic=10
;; (inc 9999)   is  10000(5)  (pinc 9999)   is  10001: DN  magic=100
;; (inc 10001)  is  10002(5)  (pinc 10001)  is  10101: DN  magic=101
;; (inc 99999)  is 100000(6)  (pinc 99999)  is 100001: UP  magic=100
;; (inc 100001) is 100002(6)  (pinc 100001) is 101101: UP  magic=101

"

(defn pseq [n]
  (letfn [(mirror-len [n]
            (let [nlen (count (str n))]
              (quot (inc nlen) 2)))

          (palindromic? [n]
            (let [ml (mirror-len n)
                  magic (seq2num (take ml (num2seq n)))]
              (= n (tomirror magic (up? n)))))

          (up? [n] (even? (count (str n))))

          (num2seq [n] (into [] (str n)))

          (seq2num [l] (read-string (apply str l)))

          (tomirror [m up]
            (seq2num (concat (num2seq m)
                             (if up
                               (reverse (num2seq m))
                               (rest (reverse (num2seq m)))))))
          (next-palindromic-num [n]
            (let [nx (inc n)
                  ml (mirror-len nx)
                  magic (seq2num (take ml (num2seq nx)))
                  mirror (tomirror magic (up? nx))
                  mirror+ (tomirror (inc magic) (up? nx))]
              (if (<= mirror n) mirror+ mirror)))]
    (if (palindromic? n)
      (iterate next-palindromic-num n)
      (rest (iterate next-palindromic-num n)))))

(expect (take 26 (pseq 0))
   [0 1 2 3 4 5 6 7 8 9
    11 22 33 44 55 66 77 88 99
    101 111 121 131 141 151 161])

(expect (take 16 (pseq 162))
   [171 181 191 202
    212 222 232 242
    252 262 272 282
    292 303 313 323])

(expect (take 6 (pseq 1234550000))
   [1234554321 1234664321 1234774321
    1234884321 1234994321 1235005321])

(expect (first (pseq (* 111111111 111111111)))
   (* 111111111 111111111))

(expect (set (take 199 (pseq 0)))
   (set (map #(first (pseq %)) (range 0 10000))))

(expect true
   (apply < (take 6666 (pseq 9999999))))

(expect (nth (pseq 0) 10101)
   9102019)

"
#158 Decurry

Difficulty:Medium
Topics:partial-functions


Write a function that accepts a curried function of unknown arity
n. Return an equivalent function of n arguments.  You may wish to
read this: http://en.wikipedia.org/wiki/Currying
"

(defn g [f]
  (fn l [& xs]
    (loop [f f
           [x & xs] xs]
      (if-not (seq? xs)
        (f x)
        (recur (f x) xs)))))

(expect 10 ((g (fn [a]
             (fn [b]
               (fn [c]
                 (fn [d]
                   (+ a b c d))))))
       1 2 3 4))

(expect 24 ((g (fn [a]
             (fn [b]
               (fn [c]
                 (fn [d]
                   (* a b c d))))))
       1 2 3 4))

(expect 25 ((g (fn [a]
             (fn [b]
               (* a b))))
       5 5))



"
#141: Tricky card games

Difficulty:Medium
Topics:game cards

In trick-taking card games such as bridge, spades, or hearts, cards
are played in groups known as 'tricks' - each player plays a single
card, in order; the first player is said to 'lead' to the trick. After
all players have played, one card is said to have 'won' the trick. How
the winner is determined will vary by game, but generally the winner
is the highest card played in the suit that was led. Sometimes (again
varying by game), a particular suit will be designated 'trump',
meaning that its cards are more powerful than any others: if there is
a trump suit, and any trumps are played, then the highest trump wins
regardless of what was led.

Your goal is to devise a function that can determine which of a number
of cards has won a trick. You should accept a trump suit, and return a
function winner. Winner will be called on a sequence of cards, and
should return the one which wins the trick. Cards will be represented
in the format returned by Problem 128, Recognize Playing Cards: a
hash-map of :suit and a numeric :rank. Cards with a larger rank are
stronger.
"

(defn crd [trump]
  (fn [cards]
    (let [first-card (first cards)
          trump-in-cards (some #{trump} (map #(:suit %) cards))
          lead-suit (if trump-in-cards trump (:suit first-card))
          cards-in-lead-suit (filter #(= (:suit %) lead-suit) cards)
          lead-suit-values (map #(:rank %) cards-in-lead-suit)
          max-val (apply max lead-suit-values)]
      (first (filter #(and (= (:suit %) lead-suit)
                           (= (:rank %) max-val)) cards)))))

(expect ((crd :spade) [{:suit :spade :rank 2}
                       {:suit :diamond :rank 10}]) {:suit :spade :rank 2})

(expect ((crd nil) [{:suit :heart :rank 2}
                    {:suit :diamond :rank 10}]) {:suit :heart :rank 2})

(expect ((crd :club) [{:suit :heart :rank 2}
                      {:suit :club :rank 10}]) {:suit :club :rank 10})

(expect ((crd :club) [{:suit :heart :rank 2}
                      {:suit :diamond :rank 10}]) {:suit :heart :rank 2})


(expect true
        (let [notrump (crd nil)]
          (and (= {:suit :club :rank 9}  (notrump [{:suit :club :rank 4}
                                                   {:suit :club :rank 9}]))
               (= {:suit :spade :rank 2} (notrump [{:suit :spade :rank 2}
                                                   {:suit :club :rank 10}])))))


(expect {:suit :club :rank 10} ((crd :club) [{:suit :spade :rank 2}
                                             {:suit :club :rank 10}]))

(expect {:suit :heart :rank 8}
        ((crd :heart) [{:suit :heart :rank 6} {:suit :heart :rank 8}
                       {:suit :diamond :rank 10} {:suit :heart :rank 4}]))



;; #131: Sum Some Set Subsets
;;
;; Difficulty:Medium
;; Topics:math
;;
;;
;; Given a variable number of sets of integers, create a function
;; which returns true iff all of the sets have a non-empty subset with
;; an equivalent summation.

(defn gg [& ss]
  "
  Certainly there is a more elegant way to do this, but the approach is:
  - form a set of non-empty subsets of each sequence; this is done by
    creating a set of true or false variables indicating whether each element
    is in the subset;
  - sum each of these subsets;
  - look for a nonempty intersection of sums across the sequences.
  "
  (letfn [(tmult [s]
            (if (seq s)
              (for [el s, tf [true false]] (conj el tf))
              [[true] [false]]))
          (subsets [s]
            (let [v (vec s)
                  n (count s)
                  tfs (nth (iterate tmult []) n)
                  sets (filter (complement empty?)
                               (for [bits tfs]
                                 (filter (complement nil?)
                                         (for [i (range n)]
                                           (if (nth bits i)
                                             (nth v i))))))]
              sets))
          (sums [s]
            (map #(apply + %) (subsets s)))
          (intersection [a b]
            (set (for [el b :when (contains? a el)] el)))
          (intersections [ss]
            (reduce intersection ss))]
    (let [allsums (map (comp set sums) ss)
          common (intersections allsums)]
      (not (empty? common)))))


(expect true  (gg #{-1 1 99}
             #{-2 2 888}
             #{-3 3 7777})) ; ex. all sets have a subset which sums to zero

(expect false (gg #{1}
                  #{2}
                  #{3}
                  #{4}))

(expect true  (gg #{1}))


(expect false (gg #{1 -3 51 9}
             #{0}
             #{9 2 81 33}))

(expect true  (gg #{1 3 5}
             #{9 11 4}
             #{-3 12 3}
             #{-3 4 -2 10}))

(expect false (gg #{-1 -2 -3 -4 -5 -6}
             #{1 2 3 4 5 6 7 8 9}))

(expect true  (gg #{1 3 5 7}
             #{2 4 6 8}))

(expect true  (gg #{-1 3 -5 7 -9 11 -13 15}
             #{1 -3 5 -7 9 -11 13 -15}
             #{1 -1 2 -2 4 -4 8 -8}))

(expect true  (gg #{-10 9 -8 7 -6 5 -4 3 -2 1}
                          #{10 -9 8 -7 6 -5 4 -3 2 -1}))



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



;; Sequs Horribilis
;;
;; Difficulty:Medium
;; Topics:seqs
;;
;; Create a function which takes an integer and a nested collection of
;; integers as arguments. Analyze the elements of the input collection
;; and return a sequence which maintains the nested structure, and
;; which includes all elements starting from the head whose sum is
;; less than or equal to the input integer.
;;
;; (=  (__ 10 [1 2 [3 [4 5] 6] 7])
;;     '(1 2 (3 (4))))
;;
;; (=  (__ 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
;;     '(1 2 (3 (4 (5 (6 (7)))))))
;;
;; (=  (__ 9 (range))
;;     '(0 1 2 3))
;;
;; (=  (__ 1 [[[[[1]]]]])
;;     '(((((1))))))
;;
;; (=  (__ 0 [1 2 [3 [4 5] 6] 7])
;;     '())
;;
;; (=  (__ 0 [0 0 [0 [0]]])
;;     '(0 0 (0 (0))))
;;
;; (=  (__ 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
;;        '(-10 (1 (2 3 (4)))))

(defn ff [T s]
  (letfn [(inner [T s S]
            (loop [s s, S S, ret []]
              (let [[x & xs] s
                    nS (if (or (coll? x) (nil? x)) S (+ x S))
                    [fs fS] (if (coll? x) (inner T x nS) [x nS])]
                (if (and (seq s) (<= fS T))
                  (recur xs fS (conj ret fs))
                  [ret S]))))]
    ((inner T s 0) 0)))

(expect  (ff 10 [1 2 [3 [4 5] 6] 7])
         '(1 2 (3 (4))))

(expect  (ff 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
         '(1 2 (3 (4 (5 (6 (7)))))))

(expect  (ff 9 (range))
         '(0 1 2 3))

(expect (ff 1 [[[[[1]]]]])
        '(((((1))))))

(expect  (ff 0 [1 2 [3 [4 5] 6] 7])
         '())

(expect  (ff 0 [0 0 [0 [0]]])
         '(0 0 (0 (0))))

(expect  (ff 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
         '(-10 (1 (2 3 (4)))))


;; Generating k-combinations
;;
;; Difficulty:Medium
;; Topics:seqs combinatorics
;;
;;
;; Given a sequence S consisting of n elements generate all
;; k-combinations of S, i. e. generate all possible sets consisting of k
;; distinct elements taken from S. The number of k-combinations for a
;; sequence is equal to the binomial coefficient.
;;
;; (= (__ 1 #{4 5 6}) #{#{4} #{5} #{6}})
;;
;; (= (__ 10 #{4 5 6}) #{})
;;
;; (= (__ 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}})
;;
;; (= (__ 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
;;                          #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}})
;;
;; (= (__ 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}})
;;
;; (= (__ 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
;;                                       #{:a "abc"} #{:a "efg"} #{"abc" "efg"}})


(defn choose [k s]  ;; Kind of a brute force solution, must be a better way...
  (set
   (filter #(= (count %) k)
           (loop [n k, ret (map (fn [x] #{x}) s)]
             (if (zero? n)
               ret
               (recur (dec n)
                      (for [v ret, x s]
                        (conj v x))))))))


(expect (choose 1 #{4 5 6}) #{#{4} #{5} #{6}})

(expect (choose 10 #{4 5 6}) #{})

(expect (choose 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}})

(expect (choose 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                         #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}})

(expect (choose 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}})

(expect (choose 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                      #{:a "abc"} #{:a "efg"} #{"abc" "efg"}})





;; Universal Computation Engine
;;
;; Difficulty:Medium
;; Topics:functions
;;
;; Given a mathematical formula in prefix notation, return a function
;; that calculates the value of the formula. The formula can contain
;; nested calculations using the four basic mathematical operators,
;; numeric constants, and symbols representing variables. The returned
;; function has to accept a single parameter containing the map of
;; variable names to their values.
;;
;; (= 2 ((__ '(/ a b))
;;       '{b 8 a 16}))
;;
;; (= 8 ((__ '(+ a b 2))
;;       '{a 2 b 4}))
;;
;; (= [6 0 -4]
;;    (map (__ '(* (+ 2 a)
;;                 (- 10 b)))
;;         '[{a 1 b 8}
;;           {b 5 a -2}
;;           {a 2 b 11}]))
;;
;; (= 1 ((__ '(/ (+ x 2)
;;               (* 3 (+ y 1))))
;;             '{x 4 y 1}))

(defn f [expr]
  (cond
   (number? expr) (fn [m] expr)
   (symbol? expr) (fn [m] (m expr))
   :else (fn [m]
           (let [[op & args] expr
                 mapseq (map #((f %) m) args)
                 opmap {'/ /, '* *, '+ +, '- -}]  ;; FIXME -- maybe a cleaner way?
             (apply (opmap op) (map #((f %) m) args))))))


(expect 3 ((f 'a) '{a 3}))
(expect 2 ((f 2) '{}))

(expect 2 ((f '(/ a b))
           '{b 8 a 16}))

(expect 8 ((f '(+ a b 2))
           '{a 2 b 4}))

(expect 1 ((f '(/ (+ x 2)
                  (* 3 (+ y 1))))
           '{x 4 y 1}))

(expect [6 0 -4]
        (map (f '(* (+ 2 a)
                    (- 10 b)))
             '[{a 1 b 8}
               {b 5 a -2}
               {a 2 b 11}]))



;; Prime Sandwich
;;
;; Difficulty:Medium
;; Topics:math
;;
;; A balanced prime is a prime number which is also the mean of the
;; primes directly before and after it in the sequence of valid
;; primes. Create a function which takes an integer n, and returns
;; true iff it is a balanced prime.
;;
;; (= false (__ 4))
;;
;; (= true (__ 563))
;;
;; (= 1103 (nth (filter __ (range)) 15))

(defn is-balanced-prime [x]
  (letfn [(is-prime [x]
            (not (some #(= (rem x %) 0) (range 2 x))))
          (next-prime [x]
            (first (filter is-prime (drop (inc x) (range)))))
          (prev-prime [x]
            (first (filter is-prime (range (dec x) 0 -1))))]
    (and (> x 2)
         (is-prime x)
         (let [prev-prime (prev-prime x)
               next-prime (next-prime x)]
           (= x (/ (+ prev-prime next-prime) 2))))))

(expect false (is-balanced-prime 4))

(expect true (is-balanced-prime 563))

(expect (nth (filter is-balanced-prime (range)) 15)
        1103)



;; Oscilrate
;;
;; Difficulty:Medium
;; Topics:sequences
;;
;; Write an oscillating iterate: a function that takes an initial
;; value and a variable number of functions. It should return a lazy
;; sequence of the functions applied to the value in order, restarting
;; from the first function after it hits the end.
;;
;; (= (take 3 (__ 3.14 int double)) [3.14 3 3.0])
;;
;; (= (take 5 (__ 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7])
;;
;; (= (take 12 (__ 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])


(defn f [x & s]
  (letfn [(g [x s]
            (lazy-seq
               (cons x
                     (g ((first s) x)
                        (next s)))))]
    (g x (cycle s))))

(expect (take 3 (f 3.14 int double)) [3.14 3 3.0])
(expect (take 5 (f 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7])
(expect (take 12 (f 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])


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



;; Global take-while

;; Difficulty:Medium
;; Topics:seqs higher-order-functions


;; take-while is great for filtering sequences, but it limited: you can
;; only examine a single item of the sequence at a time. What if you need
;; to keep track of some state as you go over the sequence?

;; Write a function which accepts an integer n, a predicate p, and a
;; sequence. It should return a lazy sequence of items in the list up to,
;; but not including, the nth item that satisfies the predicate.



;; (= [2 3 5 7 11 13]
;;    (__ 4 #(= 2 (mod % 3))
;;        [2 3 5 7 11 13 17 19 23]))

;; (= ["this" "is" "a" "sentence"]
;;    (__ 3 #(some #{\i} %)
;;        ["this" "is" "a" "sentence" "i" "wrote"]))

;; (= ["this" "is"]
;;    (__ 1 #{"a"}
;;                 ["this" "is" "a" "sentence" "i" "wrote"]))


(defn f
  [n p s]
  (lazy-seq
   (loop [s s, ret [], np 0]
     (if (and (< np n) (not (and (= np (dec n)) (p (first s)))))
       (recur (rest s)
              (conj ret (first s))
              (if (p (first s)) (inc np) np))
       ret))))

(println (f 4 #(= 2 (mod % 3)) [2 3 5 7 11 13 17 19 23]))

(println
   (f 3 #(some #{\i} %)
       ["this" "is" "a" "sentence" "i" "wrote"]))

(println
   (f 1 #{"a"}
                ["this" "is" "a" "sentence" "i" "wrote"]))


;; initially:
;; n 4  s [2 3 5 7 11 13 17 19 23]

;; initially:
;; n 4  s [2 3 5 7 11 13 17 19 23]


;; loop begin:

;; s [2 3 5 7 11 13 17 19 23] ret []        np 0   (< np n) true

;; recur
;;     [3 5 7 11 13 17 19 23]     [2]       np 1
;;                                                 (< np n) true
;;       [5 7 11 13 17 19 23]     [2 3]     np 1
;;                                                 (< np n) true
;;         [7 11 13 17 19 23]     [2 3 5]   np 2
;;                                                 (< np n) true
;;           [11 13 17 19 23]     [2 3 5 7] np 2
;;                                                 (< np n) true
;;              [13 17 19 23]     [2 3 5 7 11]
;;                                          np 3
;;                                                 (< np n) true
;;                 [17 19 23]     [2 3 5 7 11 13]
;;                                          np 3
;;                                                 (< np n) true
;;                    [19 23]     [2 3 5 7 11 13 17]
;;                                          np 4
;;                                                 (< np n) false
;;                          ----->[2 3 5 7 11 13 17]







;; Comparisons

;; Difficulty:Elementary
;; Topics:


;; For any orderable data type it's possible to derive all of the basic
;; comparison operations (<, ≤, =, ≠, ≥, and >) from a single operation
;; (any operator but = or ≠ will work). Write a function that takes three
;; arguments, a less than operator for the data and two items to
;; compare. The function should return a keyword describing the
;; relationship between the two items. The keywords for the relationship
;; between x and y are as follows:

;; x = y → :eq
;; x > y → :gt
;; x < y → :lt


;; (= :gt (__ < 5 1))

;; (= :eq (__ (fn [x y] (< (count x) (count y))) "pear" "plum"))

;; (= :lt (__ (fn [x y] (< (mod x 5) (mod y 5))) 21 3))

;; (= :gt (__ > 0 2))

(defn f
  [op x y]
  (let [a (op x y)
        b (op y x)]
    (cond (= [false false] [a b]) :eq
          (= [true false] [a b]) :lt
          (= [false true] [a b]) :gt)))


(expect :gt (f < 5 1))
(expect :eq (f (fn [x y] (< (count x) (count y))) "pear" "plum"))

(expect :lt (f (fn [x y] (< (mod x 5) (mod y 5))) 21 3))

(expect :gt (f > 0 2))


; Write Roman Numerals
;
;  Difficulty:	Medium
;  Topics:	strings math
;
;
;  This is the inverse of Problem 92, but much easier. Given an integer
;  smaller than 4000, return the corresponding roman numeral in uppercase,
;  adhering to the subtractive principle.
;
;  (= "I" (__ 1))
;
;  (= "XXX" (__ 30))
;
;  (= "IV" (__ 4))
;
;  (= "CXL" (__ 140))
;
;  (= "DCCCXXVII" (__ 827))
;
;  (= "MMMCMXCIX" (__ 3999))
;
;  (= "XLVIII" (__ 48))

(defn R [i]
  (let [romans (into (sorted-map-by #(> %1 %2))
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


(expect "I" (R 1))
(expect "II" (R 2))
(expect "III" (R 3))
(expect "IV" (R 4))
(expect "V" (R 5))
(expect "VI" (R 6))
(expect "VII" (R 7))
(expect "VIII" (R 8))
(expect "IX" (R 9))
(expect "X" (R 10))
(expect "XX" (R 20))
(expect "XLVIII" (R 48))
(expect "L" (R 50))
(expect "C" (R 100))
(expect "CL" (R 150))
(expect "CXL" (R 140))
(expect "CC" (R 200))
(expect "M" (R 1000))
(expect "MD" (R 1500))
(expect "MM" (R 2000))
(expect "MMM" (R 3000))
(expect "CXL" (R 140))
(expect "DCCCXXVII" (R 827))
(expect "MMMCMXCIX" (R 3999))



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


;; Sequence of pronunciations
;;
;; Difficulty:Medium
;; Topics:seqs
;;
;;
;; Write a function that returns a lazy sequence of "pronunciations"
;; of a sequence of numbers. A pronunciation of each element in the
;; sequence consists of the number of repeating identical numbers and
;; the number itself. For example, [1 1] is pronounced as [2 1] ("two
;; ones"), which in turn is pronounced as [1 2 1 1] ("one two, one
;; one").
;;
;; Your function should accept an initial sequence of numbers, and
;; return an infinite lazy sequence of pronunciations, each element
;; being a pronunciation of the previous element.
;;
;;
;;
;; (= [[1 1] [2 1] [1 2 1 1]] (take 3 (__ [1])))
;;
;; (= [3 1 2 4] (first (__ [1 1 1 4 4])))
;;
;; (= [1 1 1 3 2 1 3 2 1 1] (nth (__ [1]) 6))
;;
;; (= 338 (count (nth (__ [3 2]) 15)))

(defn pronunseq [s]
  (letfn [(next-term [s]
            (loop [l s
                   prev nil
                   ret []]
              (if (seq l)
                (let [[newval & rst] l,
                      cntpos (- (count ret) 2)
                      oldcnt (get ret cntpos)
                      same? (= prev newval)]
                  (recur rst
                         newval
                         (if same?
                           (assoc ret cntpos (inc oldcnt))
                           (vec (concat ret [1 newval])))))
                ret)))]
    (rest (iterate next-term s))))

(expect (take 3 (pronunseq [1])) [[1 1] [2 1] [1 2 1 1]])
(expect (count (nth (pronunseq [3 2]) 15)) 338)


;; Partially Flatten a Sequence
;;
;; Difficulty:Medium
;; Topics:seqs
;;
;;
;; Write a function which flattens any nested combination of
;; sequential things (lists, vectors, etc.), but maintains the lowest
;; level sequential items. The result should be a sequence of
;; sequences with only one level of nesting.
;;
;; (= (__ [["Do"] ["Nothing"]])
;;    [["Do"] ["Nothing"]])
;;
;; (= (__ [[[[:a :b]]] [[:c :d]] [:e :f]])
;;    [[:a :b] [:c :d] [:e :f]])
;;
;; (= (__ '((1 2) ((3 4) ((((5 6)))))))
;;       '((1 2) (3 4) (5 6)))

(defn q [x]
  (letfn [(flat-seq? [x]
            (empty? (filter (fn [y] (coll? y)) x)))]
    (loop [x x, ret []]
      (cond
       (not (seq x)) ret
       (flat-seq? x) x
       (flat-seq? (first x)) (recur (rest x)
                                    (concat ret [(first x)]))
       :else (recur (rest x)
                    (concat ret (q (first x))))))))

(expect (q []) [])
(expect (q [1]) [1])
(expect (q [1 2]) [1 2])
(expect (q [[1 2]]) [[1 2]])
(expect (q [[1 2] [3 4]]) [[1 2] [3 4]])
(expect (q [[1 2] [3 4] [5 6]]) [[1 2] [3 4] [5 6]])
(expect (q [[[1 2]]]) [[1 2]])
(expect (q [[1 2] [[3 4] [[[[5 6]]]]]]) [[1 2] [3 4] [5 6]])
(expect (q [[[[:a :b]]] [[:c :d]] [:e :f]]) [[:a :b] [:c :d] [:e :f]])
(expect (q '((1 2) ((3 4) ((((5 6))))))) '((1 2) (3 4) (5 6)))
(expect (q [["Do"] ["Nothing"]]) [["Do"] ["Nothing"]])


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


;; Compress a sequence
;; Difficulty:	Easy
;; Topics:	seqs


;; Write a function which removes consecutive duplicates from a sequence.
;; test not run	

;; (= (apply str (__ "Leeeeeerrroyyy")) "Leroy")
;; test not run	

;; (= (__ [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
;; test not run	

;; (= (__ [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))

(fn [s] (->> s (partition-by identity)
               (map first)))

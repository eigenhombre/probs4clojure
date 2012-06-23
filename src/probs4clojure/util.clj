(use 'clojure.tools.trace)
(use 'clojure.pprint)

(defmacro dbg [x]
  `(let [x# ~x] (println (str '~x "=" x#)) x#))

(def noprintln (fn [&rest]))

(defn pmax
  ([x] (pmax x 15))
  ([x T]
     (let [sx (str x)
           six (take (- T 4) sx)
           ell (concat six ["..."])]
       (if (> (count sx) T)
         (apply str ell)
         (str x)))))

(defn p
  ([s]
     (p 10 s))
  ([width s]
     (let [c (count s)
           fmt (apply str (repeat c (apply str "%" width "s")))]
       (apply (partial format fmt) (map pmax s)))))

(defn pcolls [& s] (println (p 20 s)))

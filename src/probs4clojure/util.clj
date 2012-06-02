(use 'clojure.tools.trace)
(use 'clojure.pprint)

(defmacro dbg [x]
  `(let [x# ~x] (println (str '~x "=" x#)) x#))

(def noprintln (fn [&rest]))

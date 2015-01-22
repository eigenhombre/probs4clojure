(defproject probs4clojure "0.1.0-SNAPSHOT"
  :description "Problems from 4clojure.com, solved and annotated."
  :url "https://github.com/eigenhombre/probs4clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :aliases {"autotest" ["midje" ":autotest"]
            "doc" ["marg" "test"]}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [expectations "1.3.3"]]
  :profiles {:dev {:dependencies [[midje "1.6.3"]]
                   :plugins [[lein-midje "3.1.3"]]}})

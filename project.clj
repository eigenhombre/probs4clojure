(defproject probs4clojure "0.1.0-SNAPSHOT"
  :description "Problems from 4clojure.com, solved and annotated."
  :url "https://github.com/eigenhombre/probs4clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :aliases {"autotest" ["test-refresh"]
            "doc" ["marg"
                   "test/probs4clojure/core_test.clj"
                   "test/probs4clojure/test_util.clj"]}
  :plugins [[speclj "3.3.0"]]
  :test-paths ["test"]
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :profiles {:dev {:dependencies [[speclj "3.3.0"]]
                   :plugins [[michaelblume/lein-marginalia "0.9.0"]
                             [com.jakemccrary/lein-test-refresh "0.21.1"]]}
             :uberjar {:aot :all}})


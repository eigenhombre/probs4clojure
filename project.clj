(defproject probs4clojure "0.1.0-SNAPSHOT"
  :description "Problems from 4clojure.com, solved and annotated."
  :url "https://github.com/eigenhombre/probs4clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :aliases {"autotest" ["spec" "--format=progress" "-r" "v"]}
  :plugins [[speclj "3.3.0"]]
  :test-paths ["test"]
  :dependencies [[org.clojure/clojure "1.7.0"]]
  :profiles {:dev {:dependencies [[speclj "3.3.0"]]}
             :uberjar {:aot :all}})


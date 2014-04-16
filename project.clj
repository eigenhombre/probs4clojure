(defproject probs4clojure "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :aliases {"autotest" ["midje" ":autotest"]}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [expectations "1.3.3"]
                 [org.clojure/tools.trace "0.7.3"]]
  :profiles {:dev {:dependencies [[midje "1.5.1"]]}})

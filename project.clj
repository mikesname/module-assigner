(defproject module-assigner "0.1.0-SNAPSHOT"
  :description "A toy project for assigning students to course modules"
  :url "https://github.com/mikesname/module-assigner"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [compojure "1.3.1"]
                 [ring/ring-defaults "0.1.2"]
                 [ring/ring-jetty-adapter "1.2.2"]
                 [environ "0.5.0"]
                 [enlive "1.1.5"]
                 [org.clojure/data.csv "0.1.2"]]
  :plugins [
            [lein-ring "0.8.13"]
            [environ/environ.lein "0.2.1"]]
  :ring {:handler module-assigner.handler/app}
  :profiles {
             :dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring-mock "0.1.5"]]}}
  :uberjar-name "module-assigner-standalone.jar"
  :hooks [environ.leiningen.hooks])

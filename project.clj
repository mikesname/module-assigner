(defproject module-assigner "0.1.0-SNAPSHOT"
  :description "A toy project for assigning students to course modules"
  :url "https://github.com/mikesname/module-assigner"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/data.csv "0.1.2"]
                 [compojure "1.3.4"]
                 [ring/ring-defaults "0.1.2"]
                 [ring/ring-jetty-adapter "1.2.2"]
                 [environ "0.5.0"]
                 [org.apache.httpcomponents/httpmime "4.5"] 
                 [enlive "1.1.5"]
                 [com.akolov.enlive-reload "0.2.1" :refer [wrap-enlive-reload]]]
  :plugins [
            [lein-ring "0.8.13"]
            [environ/environ.lein "0.2.1"]]
  :ring {:handler module-assigner.handler/app}
  :profiles {
             :dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring-mock "0.1.5"]]}}
  :uberjar-name "module-assigner-standalone.jar"
  :hooks [environ.leiningen.hooks]
  :resource-paths ["resources" "test/resources"])

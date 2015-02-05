;; For Heroku deployment
;; https://module-assigner.herokuapp.com/

(ns module-assigner.web
  (:require [module-assigner.handler :refer :all]
            [compojure.handler :refer [site]]
            [ring.adapter.jetty :as jetty]
            [environ.core :refer [env]]))


(defn -main [& [port]]
  (let [port (Integer. (or port (env :port) 5000))]
    (jetty/run-jetty (site #'app) {:port port :join? false})))

;; For interactive development:
;; (.stop server)
;; (def server (-main))

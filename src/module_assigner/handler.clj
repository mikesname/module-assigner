(ns module-assigner.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [clojure.java.io :as io]))

(defn step1
  [request]
  "The first page, where we show the course info upload field")

(defn step2
  [request]
  "The second page, where we show the student preference upload field")

(defn step3
  [request]
  "The third page, where we render the results")

(defroutes app-routes
  (GET "/" [] step1)
  (POST "/step2" [] step2)
  (POST "/step3" [] step3)
  ; this is an example POST file upload
  (POST "/upload"
           {{{tempfile :tempfile filename :filename} :file} :params :as params}
           (io/copy tempfile (io/file "resources" "public" filename))
           "Success")
  (route/not-found "Not Found"))

(def app
  (wrap-defaults app-routes (assoc-in site-defaults [:security :anti-forgery] false)))


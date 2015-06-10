(ns module-assigner.handler
  (:use [net.cgrand.enlive-html
         :only [deftemplate defsnippet content clone-for
                first-of-type nth-of-type first-child do-> set-attr sniptest at emit*]])
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware [multipart-params :as mp]]
            [clojure.java.io :as io]))

(use '[module-assigner.assigner :refer :all]
     '[module-assigner.csvdata :refer :all])

(deftemplate index-t "module-assigner/index.html" [])

(defsnippet module-snippet "module-assigner/step2.html" 
  [:tbody [:tr first-of-type]]
  [modules]
  [:tr] (clone-for [module modules]
                   [:tr [:td (nth-of-type 1)]] (content (str (:id module)))
                   [:tr [:td (nth-of-type 2)]] (content (:name module))
                   [:tr [:td (nth-of-type 3)]] (content (:name (:course module)))))

(deftemplate step2-t "module-assigner/step2.html" [modules]
  [:tbody] (content (module-snippet modules)))


(defn render [t]
  (apply str t))

(defn index
  [request]
  "The first page, where we show the course info upload field")

(defn upload-modules
  [file filename]
  (with-open [rdr (clojure.java.io/reader file)]
    (let [modules (read-modules rdr)]
      (render (step2-t modules)))))

(defn step2
  [request]
  "The second page, where we show the student preference upload field")

(defn step3
  [request]
  "The third page, where we render the results")

(defroutes app-routes
  (GET "/" [] (render (index-t)))
  (POST "/step2" {:keys [params]}
        (let [{:keys [tempfile filename]} (get params :file)]
          (upload-modules tempfile filename)))

  (POST "/step3" [] step3)
  ; this is an example POST file upload
  (POST "/upload"
           {{{tempfile :tempfile filename :filename} :file} :params :as params}
           (io/copy tempfile (io/file "resources" "public" filename))
           "Success")
  (route/not-found "Not Found"))

(def app
  (wrap-defaults app-routes (assoc-in site-defaults [:security :anti-forgery] false)))


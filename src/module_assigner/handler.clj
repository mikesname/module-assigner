(ns module-assigner.handler
  (:use [net.cgrand.enlive-html
         :only [deftemplate defsnippet content clone-for
                first-of-type nth-of-type first-child do-> set-attr add-class sniptest at emit*]])
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.util.response :as r]
            [com.akolov.enlive-reload :refer :all]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware [multipart-params :as mp]]
            [clojure.java.io :as io]))

(use '[module-assigner.assigner :refer :all]
     '[module-assigner.csvdata :refer :all])

(defn module-cap-with-default [param]
 (if (clojure.string/blank? param) 45 (read-string param)))

(deftemplate page-t "module-assigner/template.html" [title body]
  [:title] (content title)
  [:.main] (content body))

(defsnippet step1-t "module-assigner/step1.html" [:div.article] [] identity)

(defsnippet module-snippet "module-assigner/step2.html" 
  [:#modules :tbody [:tr first-of-type]]
  [modules]
  [:tr] (clone-for [module modules]
                   [:tr [:td (nth-of-type 1)]] (content (str (:id module)))
                   [:tr [:td (nth-of-type 2)]] (content (:name module))
                   [:tr [:td (nth-of-type 3)]] (content (str (:term module)))
                   [:tr [:td (nth-of-type 4)]] (content (:name (:course module)))))

(defsnippet preferences-snippet "module-assigner/step3.html" 
  [:#preferences :tbody [:tr first-of-type]]
  [assignments cap]
  [:tr] (clone-for [[module students] (by-modules assignments)]
                   [:tr [:td (nth-of-type 1)]] (content (:name module))
                   [:tr [:td (nth-of-type 2)]] (content 
                                                 (if (> (count students) cap)
                                                   (str (- (count students) cap)) 
                                                   ""))
                   [:tr [:td (nth-of-type 3)]] (content (clojure.string/join ", " (map :name students)))))

(defsnippet solved-assignments-snippet "module-assigner/step3.html" 
  [:#assignments :tbody [:tr first-of-type]]
  [assignments]
  [:tr] (clone-for [[module students] (by-modules assignments)]
                   [:tr [:td (nth-of-type 1)]] (content (:name module))
                   [:tr [:td (nth-of-type 2)]] (content (str (count students)))
                   [:tr [:td (nth-of-type 3)]] (content (clojure.string/join ", " (map :name students)))))

(defsnippet moves-snippet "module-assigner/step3.html" 
  [:#moves :tbody [:tr first-of-type]]
  [moves]
  [:tr] (clone-for [move moves]
                   [:tr [:td (nth-of-type 1)]] (content (:name (:student move)))
                   [:tr [:td (nth-of-type 2)]] (content (:name (:from move)))
                   [:tr [:td (nth-of-type 3)]] (content (:name (:to move)))))

(defsnippet step2-t "module-assigner/step2.html" [:form] [modules modcsv cap]
  [:input#module-cap] (set-attr :value cap)
  [:input#module-data] (set-attr :value modcsv)
  [:#modules :tbody] (content (module-snippet modules)))

(defsnippet step2-error "module-assigner/step1.html" [:form] [error cap]
  [:input#module-cap] (set-attr :value cap)
  [:.modules-file] (add-class "error")
  [:.modules-file :.form-errors] (content error))

(defsnippet step3-t "module-assigner/step3.html" [:form] [modules modcsv prefs prefcsv solved cap]
  [:input#module-cap] (set-attr :value cap)
  [:input#module-data] (set-attr :value modcsv)
  [:input#preference-data] (set-attr :value prefcsv)
  [:#modules :tbody] (content (module-snippet modules))
  [:#preferences :tbody] (content 
                           (preferences-snippet 
                             (assign-initial prefs) (:cap solved)))
  [:#assignments :tbody] (content 
                           (solved-assignments-snippet 
                             (:assignments solved)))
  [:#moves :tbody] (content (moves-snippet (:moves solved))))

(defsnippet step3-error "module-assigner/step2.html" [:form] [modules modcsv error cap]
  [:input#module-cap] (set-attr :value cap)
  [:input#module-data] (set-attr :value modcsv)
  [:#modules :tbody] (content (module-snippet modules))
  [:.form-errors] (content error))


(defn render [t]
  (apply str t))

(defn render-page [title t]
  (page-t title t))

(defn render-err [title t]
  {:body (page-t title t) :status 400})

(defn upload-modules
  [file filename cap]
  (if (not (clojure.string/blank? filename))
    (try
      (let [csvstr (slurp file) modules (read-modules csvstr)]
        (render-page "Step 2" (step2-t modules csvstr cap)))
      (catch clojure.lang.ExceptionInfo e
        (render-err "Data Error" (step2-error (.getMessage e)))))
    (render-err "Error" (step2-error "no file given" cap))))

(defn upload-preferences
  [file filename modcsv cap]
  (let [modules (read-modules modcsv)]
    (if (not (clojure.string/blank? filename))
      (try    
        (let [prefcsv (slurp file)
              prefs (read-preferences modules prefcsv)
              solved (calculate-terms modules prefs cap (range 1 3))]
          (print-report solved)
          (render-page "Result" (step3-t modules modcsv prefs prefcsv solved cap)))
        (catch clojure.lang.ExceptionInfo e
          (render-err "Error" (step3-error modules modcsv (.getMessage e) cap))))
      (render-err "Error" (step3-error modules modcsv "no file given" cap)))))

(defn send-result-csv [modcsv prefcsv cap]
  (let [modules (read-modules modcsv)
        prefs (read-preferences modules prefcsv)
        solved (solve (calculate-terms modules prefs cap (range 1 3)))]
    (print-report solved)
    (->                  
      (r/response (write-results solved))
      (r/header "Content-type" "text/csv; charset=utf-8")
      (r/header "Content-disposition" "attachment; filename=module-assignments.csv"))))

(defroutes app-routes
  (GET "/" [] (render-page "Module Assigner" (step1-t)))

  (POST "/step2" {:keys [params]}
        (let [{:keys [tempfile filename]} (get params :file)
              cap (module-cap-with-default (get params :modcap))]
          (upload-modules tempfile filename cap)))
  
  (POST "/step3" {:keys [params]}
        (let [{:keys [tempfile filename]} (get params :file)
              modcsv (get params :modcsv)
              cap (module-cap-with-default (get params :modcap))]
          (upload-preferences tempfile filename modcsv cap)))

  (POST "/download" {:keys [params]}
        (let [
              prefcsv (get params :prefcsv)
              modcsv (get params :modcsv)
              cap (module-cap-with-default (get params :modcap))]
          (send-result-csv modcsv prefcsv cap)))

  (route/not-found "Not Found"))

(def app
  (wrap-enlive-reload
    (wrap-defaults app-routes (assoc-in site-defaults [:security :anti-forgery] false))))


(ns module-assigner.handler-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [clojure.java.io :as io]
            [module-assigner.handler :refer :all]))

(import 'org.apache.http.entity.mime.MultipartEntityBuilder
        'org.apache.http.entity.ContentType
        'java.io.ByteArrayOutputStream)

(def module-cap "10")

(defn- build-multipart [params files]
  (let [builder (MultipartEntityBuilder/create)]
    (doseq [[key value] params]
      (.addTextBody builder (name key) value))
    (doseq [[key file] files]
      (.addBinaryBody
        builder
        (name key)
        (.getBytes (:data file) "UTF-8")
        ContentType/TEXT_PLAIN
        (:filename file)))
    (.setBoundary builder "12345678")
    (let [writer (new ByteArrayOutputStream)]
      (.writeTo (.build builder) writer)
      (.toString writer))))

(deftest test-app
  (testing "index route"
    (let [response (app (mock/request :get "/"))]
      (is (= (:status response) 200))
      (is (.contains (apply str (:body response)) "Step 1"))))

  (testing "assignment"
    (let [module-data (slurp (-> "modules.csv" io/resource io/file))
          preference-data (slurp (-> "preferences.csv" io/resource io/file))
          response (app (-> (mock/request :post "/assign" (build-multipart {:modcap module-cap} {                                                                                                             :modfile {
                                                                               :filename "modules.csv"                                                                                                           :data module-data
                                                                             }
                                                                             :preffile {
                                                                               :filename "preferences.csv"
                                                                               :data preference-data         
                                                                             }}))
                            (mock/content-type "multipart/form-data; boundary=12345678")))]
      (is (= (:status response) 200))
      (is (.contains (apply str (:body response)) "Student preferences"))
      (is (.contains (apply str (:body response)) "Calculated assignments"))))

  (testing "process with no file"
    (let [response (app (-> (mock/request :post "/assign")))]
      (is (= (:status response) 400))
      (is (.contains (apply str (:body response)) "Step 1"))))

  (testing "not-found route"
    (let [response (app (mock/request :get "/invalid"))]
      (is (= (:status response) 404)))))


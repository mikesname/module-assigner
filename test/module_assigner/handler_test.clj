(ns module-assigner.handler-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [clojure.java.io :as io]
            [module-assigner.handler :refer :all]))

(import 'org.apache.http.entity.mime.MultipartEntityBuilder
        'org.apache.http.entity.ContentType
        'java.io.ByteArrayOutputStream)

(def module-cap "10")

(defn- build-multipart [params file]
  (let [builder (MultipartEntityBuilder/create)]
    (doseq [[key value] params]
      (.addTextBody builder (name key) value))
    (.addBinaryBody
      builder
      (:name file)
      (.getBytes (:data file) "UTF-8")
      ContentType/TEXT_PLAIN
      (:filename file))
    (.setBoundary builder "12345678")
    (let [writer (new ByteArrayOutputStream)]
      (.writeTo (.build builder) writer)
      (.toString writer))))

(deftest test-app
  (testing "index route"
    (let [response (app (mock/request :get "/"))]
      (is (= (:status response) 200))
      (is (.contains (apply str (:body response)) "Step 1"))))

  (testing "step 1"
    (let [module-data (slurp (-> "modules.csv" io/resource io/file))
          response (app (-> (mock/request :post "/step2" (build-multipart {:modcap module-cap} {
                                                                             :name "file"
                                                                             :filename "modules.csv"
                                                                             :data module-data}))
                            (mock/content-type "multipart/form-data; boundary=12345678")))]
      (is (= (:status response) 200))
      (is (.contains (apply str (:body response)) "Step 2"))))

  (testing "step 1 with no file"
    (let [response (app (-> (mock/request :post "/step2")))]
      (is (= (:status response) 400))
      (is (.contains (apply str (:body response)) "Step 1"))))

  (testing "step 2"
    (let [
          module-data (slurp (-> "modules.csv" io/resource io/file))
          preferences-data (slurp (-> "preferences.csv" io/resource io/file))
          mp (build-multipart {:modcsv module-data :modcap module-cap} {
                             :name "file"
                             :filename "preferences.csv"
                             :data preferences-data})]
      (let [
          response (app (-> (mock/request :post "/step3" mp)
                            (mock/content-type "multipart/form-data; boundary=12345678")))]
        (is (= (:status response) 200))
        (is (.contains (apply str (:body response)) "Calculated assignments")))))

  (testing "not-found route"
    (let [response (app (mock/request :get "/invalid"))]
      (is (= (:status response) 404)))))

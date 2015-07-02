(ns module-assigner.handler-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [clojure.java.io :as io]
            [module-assigner.handler :refer :all]))

(defn- module-mp-data []
  (let [data (slurp (-> "modules.csv" io/resource io/file))]
    (apply str (interpose "\r\n" [
        "--12345678"
        "Content-Disposition: form-data; name=\"test\"\r\n"
        "test"                                  
        "--12345678"
        "Content-Disposition: form-data; name=\"file\"; filename=\"modules.csv\""
        "Content-Type: text/plain\r\n"
        data
        "--12345678--"
        ""]))))

(deftest test-app
  (testing "index route"
    (let [response (app (mock/request :get "/"))]
      (is (= (:status response) 200))
      (is (.contains (apply str (:body response)) "Step 1"))))

  (testing "step 1"
    (let [response (app (-> (mock/request :post "/step2" (module-mp-data))
                            (mock/content-type "multipart/form-data; boundary=12345678")))]
      (is (= (:status response) 200))
      (is (.contains (apply str (:body response)) "Step 2"))))

  (testing "step 2"
    (let [response (app (-> (mock/request :post "/step3")
                            (mock/body {:modcsv (str "1,DH,1,M1,XML\n"
                                                      "2,DH,1,M2,Python\n"
                                                      "3,DAM,1,M3,Metadata\n"
                                                      "4,DAM,1,M4,DigiPres\n")})))]
      (is (= (:status response) 200))
      (is (.contains (apply str (:body response)) "Step 1"))))

  (testing "not-found route"
    (let [response (app (mock/request :get "/invalid"))]
      (is (= (:status response) 404)))))

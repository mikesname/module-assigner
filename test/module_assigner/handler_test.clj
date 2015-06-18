(ns module-assigner.handler-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [module-assigner.handler :refer :all]))

(deftest test-app
  (testing "main route"
    (let [response (app (mock/request :get "/"))]
      (is (= (:status response) 200))
      (is (.contains (:body response) "Module Assigner: Step 1"))))

  (testing "not-found route"
    (let [response (app (mock/request :get "/invalid"))]
      (is (= (:status response) 404)))))

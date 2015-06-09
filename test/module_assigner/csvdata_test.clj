(ns module-assigner.csvdata-test
  (:require [clojure.test :refer :all] [clojure.java.io :as io])
  (:use module-assigner.csvdata))

(use '[module-assigner.assigner :refer :all])

(def test-module-data-1
  (str
    "1,DH,1,M1,XML" \newline
    "2,DH,1,M2,Python" \newline
    "3,DAM,1,M3,Metadata" \newline
    "4,DAM,1,M4,DigiPres" \newline))

(def test-preference-data-1
  (str
    "1,Bob,DH,1,2,3,4" \newline
    "2,Jane,DH,4,3,2,1" \newline))

(deftest test-read-modules
  (testing "Reading Module CSV"
    (let [data [
                (->Module 1 "XML" (->Course "DH"))
                (->Module 2 "Python" (->Course "DH"))
                (->Module 3 "Metadata" (->Course "DAM"))
                (->Module 4 "DigiPres" (->Course "DAM"))]]
      (is (= data (read-modules test-module-data-1))))))

(deftest test-read-preferences
  (testing "Reading Preference CSV"
    (let [mod1 (->Module 1 "XML" (->Course "DH"))
          mod2 (->Module 2 "Python" (->Course "DH"))
          mod3 (->Module 3 "Metadata" (->Course "DAM"))
          mod4 (->Module 4 "DigiPres" (->Course "DAM"))
          allmods [mod1 mod2 mod3 mod4]
          test-prefs [
                      (->Preference
                                    (->Student 1 "Bob" (->Course "DH"))
                                    [mod1 mod2 mod3 mod4])
                      (->Preference
                                    (->Student 2 "Jane" (->Course "DH"))
                                    [mod4 mod3 mod2 mod1])]]
      (is (= test-prefs (read-preferences allmods test-preference-data-1))))))




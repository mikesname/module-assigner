(ns module-assigner.csvdata-test
  (:require [clojure.test :refer :all] [clojure.java.io :as io])
  (:use module-assigner.csvdata))

(use '[module-assigner.assigner :refer :all]
     '[module-assigner.assigner-test :refer :all])

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
                (->Module 1 "XML" (->Course "DH") 1)
                (->Module 2 "Python" (->Course "DH") 1)
                (->Module 3 "Metadata" (->Course "DAM") 1)
                (->Module 4 "DigiPres" (->Course "DAM") 1)]]
      (is (= data (read-modules test-module-data-1))))))

(deftest test-read-modules-with-error-at-line
  (testing "Reading Module CSV with error at line 3"
    (let [badcsv (str
            "1,DH,1,M1,XML" \newline
            "2,DH,1,M2,Python" \newline
            "3,DAM,M3,Metadata" \newline
            "4,DAM,1,M4,DigiPres" \newline)]
      (is (thrown? clojure.lang.ExceptionInfo (read-modules badcsv))))))

(deftest test-read-modules-with-error-at-line
  (testing "Reading Module CSV with bad id"
    (let [badcsv (str
            "bad,DH,1,M1,XML" \newline)
          info (try (read-modules badcsv)
                    (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= {:line 0 :column 0 :description "module id"} info)))))

(deftest test-read-preferences
  (testing "Reading Preference CSV"
    (let [mod1 (->Module 1 "XML" (->Course "DH") 1)
          mod2 (->Module 2 "Python" (->Course "DH") 1)
          mod3 (->Module 3 "Metadata" (->Course "DAM") 1)
          mod4 (->Module 4 "DigiPres" (->Course "DAM") 1)
          allmods [mod1 mod2 mod3 mod4]
          test-prefs [
                      (->Preference
                                    (->Student 1 "Bob" (->Course "DH"))
                                    [mod1 mod2 mod3 mod4])
                      (->Preference
                                    (->Student 2 "Jane" (->Course "DH"))
                                    [mod4 mod3 mod2 mod1])]]
      (is (= test-prefs (read-preferences allmods test-preference-data-1))))))

(deftest test-read-preferences-with-error-at-line
  (testing "Reading Preference CSV with bad id at line 2 col 4"
    ;; NB: Line/column data are 0-indexed
    (let [badcsv (str
            "1,Bob,DH,1,2,3,4" \newline
            "2,Jane,DH,?,2,3,4" \newline)
          mods (read-modules test-module-data-1)
          info (try (read-preferences mods badcsv)
                    (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= {:line 1 :column 3 :description "choice 1"} info)))))

(deftest test-read-preferences-with-bad-module-reference
  (testing "Reading Preference CSV with bad module ref at line 1 col 7"
    ;; NB: Line/column data are 0-indexed
    (let [badcsv (str
            "1,Bob,DH,1,2,3,5" \newline ;; uh oh, 5 is not a module
            "2,Jane,DH,1,2,3,4" \newline)
          mods (read-modules test-module-data-1)
          info (try (read-preferences mods badcsv)
                    (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= {:line 0 :column 6 :description "choice 4"} info)))))

(deftest test-write-results
  (testing "Writing CSV data results"
    (let [board (init-board-with-modules test-modules test-preferences module-cap)
          out (str (clojure.string/join \newline
                   [
                    "1,Bob,1,2"
                    "2,Jane,1,2"
                    "3,Fred,1,2"
                    "4,Sue,1,2"
                    "5,Mark,1,2"
                    "6,Linda,1,2"
                    "7,David,1,2"
                    "8,Laura,1,2"
                    ]) "\n")]
     (is (= out (write-results board))))))


(ns module-assigner.csvdata-test
  (:require [clojure.test :refer :all] [clojure.java.io :as io])
  (:use module-assigner.csvdata))

(use '[module-assigner.assigner :refer :all]
     '[module-assigner.assigner-test :refer :all])

(def test-module-data-1
  (str
    "m1,DH,1,M1,XML" \newline
    "m2,DH,1,M2,Python" \newline
    "m3,DAM,1,M3,Metadata" \newline
    "m4,DAM,1,M4,DigiPres" \newline))

(def test-preference-data-1
  (str
    "1,Bob,DH,m1,m2,m3,m4" \newline
    "2,Jane,DH,m4,m3,m2,m1" \newline))

(deftest test-read-modules
  (testing "Reading Module CSV"
    (let [data [
                (->Module "m1" "XML" (->Course "DH") 1)
                (->Module "m2" "Python" (->Course "DH") 1)
                (->Module "m3" "Metadata" (->Course "DAM") 1)
                (->Module "m4" "DigiPres" (->Course "DAM") 1)]]
      (is (= data (read-modules test-module-data-1))))))

(deftest test-read-modules-with-error-at-line
  (testing "Reading Module CSV with error at line 3"
    (let [badcsv (str
            "m1,DH,1,M1,XML" \newline
            "m2,DH,1,M2,Python" \newline
            "m3,DAM,M3,Metadata" \newline
            "m4,DAM,1,M4,DigiPres" \newline)]
      (is (thrown? clojure.lang.ExceptionInfo (read-modules badcsv))))))

(deftest test-read-modules-with-error-at-line
  (testing "Reading Module CSV with bad id"
    (let [badcsv (str
            ",DH,1,M1,XML" \newline)
          info (try (read-modules badcsv)
                    (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= {:line 0 :column 0 :description "module id"} info)))))

(deftest test-read-preferences
  (testing "Reading Preference CSV"
    (let [mod1 (->Module "m1" "XML" (->Course "DH") 1)
          mod2 (->Module "m2" "Python" (->Course "DH") 1)
          mod3 (->Module "m3" "Metadata" (->Course "DAM") 1)
          mod4 (->Module "m4" "DigiPres" (->Course "DAM") 1)
          allmods [mod1 mod2 mod3 mod4]
          test-prefs [
                      (->Preference
                                    (->Student "1" "Bob" (->Course "DH"))
                                    [mod1 mod2 mod3 mod4])
                      (->Preference
                                    (->Student "2" "Jane" (->Course "DH"))
                                    [mod4 mod3 mod2 mod1])]]
      (is (= test-prefs (read-preferences allmods test-preference-data-1))))))

(deftest test-read-preferences-with-error-at-line
  (testing "Reading Preference CSV with bad id at line 2 col 4"
    ;; NB: Line/column data are 0-indexed
    (let [badcsv (str
            "1,Bob,DH,m1,m2,m3,m4" \newline
            "2,Jane,DH,-,m2,m3,m4" \newline)
          mods (read-modules test-module-data-1)
          info (try (read-preferences mods badcsv)
                    (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= {:line 1 :ref "-"} info)))))

(deftest test-read-preferences-with-bad-module-reference
  (testing "Reading Preference CSV with bad module ref at line 1 col 7"
    ;; NB: Line/column data are 0-indexed
    (let [badcsv (str
            "1,Bob,DH,m1,m2,m3,m5" \newline ;; uh oh, 5 is not a module
            "2,Jane,DH,m1,m2,m3,m4" \newline)
          mods (read-modules test-module-data-1)
          info (try (read-preferences mods badcsv)
                 (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= {:line 0 :ref "m5"} info)))))

(deftest test-read-preferences-alt
  (testing "Reading alt preferences"
    (let [
          csv1 (str
                "id,name,course,m1,m2,m3,m4" \newline
                "1,Bob,DH,1,2,," \newline
                "2,Jane,DH,3,,1,2" \newline)
          csv2 (str
                "1,Bob,DH,m1,m2" \newline
                "2,Jane,DH,m3,m4,m1" \newline)]
      (let [result1 (read-preferences-alt test-modules csv1)
            result2 (read-preferences test-modules csv2)]
        (is (= result1 result2))))))

(deftest test-write-results
  (testing "Writing CSV data results"
    (let [board (init-board-with-modules test-modules test-preferences module-cap)
          out (str (clojure.string/join \newline
                   [
                    "1,Bob,m1,m2"
                    "2,Jane,m1,m2"
                    "3,Fred,m1,m2"
                    "4,Sue,m1,m2"
                    "5,Mark,m1,m2"
                    "6,Linda,m1,m2"
                    "7,David,m1,m2"
                    "8,Laura,m1,m2"
                    ]) "\n")]
     (is (= out (write-results board))))))


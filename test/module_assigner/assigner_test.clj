(ns module-assigner.assigner-test
  (:require [clojure.test :refer :all])
  (:use module-assigner.assigner))

;; Courses
(def course-dh (->Course "DH"))
(def course-dam (->Course "DAM"))

;; Students
(def student-1 (->Student 1 "Bob" course-dh))
(def student-2 (->Student 2 "Jane" course-dh))
(def student-3 (->Student 3 "Fred" course-dh))
(def student-4 (->Student 4 "Sue" course-dh))
(def student-5 (->Student 5 "Mark" course-dam))
(def student-6 (->Student 6 "Linda" course-dam))
(def student-7 (->Student 7 "David" course-dam))
(def student-8 (->Student 8 "Laura" course-dam))

(def test-students [
  student-1
  student-2
  student-3
  student-4
  student-5
  student-6
  student-7
  student-8])

(def module-xml (->Module 1 "XML"  course-dh))
(def module-python (->Module 2 "Python"  course-dh))
(def module-metadata (->Module 3 "Metadata"  course-dam))
(def module-digipres (->Module 4 "DigiPres"  course-dam))

(def test-modules [
  module-xml
  module-python
  module-metadata
  module-digipres]) 

(def test-preferences [
                       ])

(def test-assignments [
    (->Assignment module-xml student-1 0 0)
    (->Assignment module-xml student-2 0 0)
                       ])

(deftest test-assigner
  (testing "Assignment"
    (is (= [] (assign test-modules test-preferences 10 4)))))

(deftest test-get-course
  (testing "Get course"
    (is (= course-dh (get-in student-1 [:course])))))

(deftest test-by-modules-empty
  (testing "Assignments by modules"
    (is (= {} (by-modules [])))))

(deftest test-module-count
  (testing "Module count"
    (is (= 2 (module-count test-assignments module-xml)))))

(deftest test-by-modules
  (testing "Assignments by modules"
    (is (= {module-xml #{student-1}} (by-modules [(->Assignment module-xml student-1 0 0)])))
    (is (= {module-xml #{student-1 student-2}} (by-modules test-assignments)))))

(deftest test-move-candidates
  (testing "Move candidates"
    (is (= [] (move-candidates test-assignments)))
    (let [bad-assignments [
                           ;; the order here is intentional. Student 5 is
                           ;; not on the same course as module-xml so is
                           ;; a higher priority move candiate the students
                           ;; 1-4.
                          (->Assignment module-xml student-5 0 0)
                          (->Assignment module-xml student-4 0 0)
                          (->Assignment module-xml student-3 0 0)
                          (->Assignment module-xml student-2 0 0)
                          (->Assignment module-xml student-1 0 0) ;; more than cap
                           ]]
      (is (= bad-assignments (move-candidates bad-assignments))))))

(deftest test-is-favoured
  (testing "Favoured modules"
    (is (= true (is-favoured (->Assignment module-xml student-1 0, 0))))
    (is (= false (is-favoured (->Assignment module-metadata student-1 0, 0))))))



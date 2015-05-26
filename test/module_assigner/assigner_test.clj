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
  (->Preference student-1 [module-xml module-python module-metadata module-digipres])
  (->Preference student-2 [module-xml module-python module-metadata module-digipres])
  (->Preference student-3 [module-xml module-python module-metadata module-digipres])
  (->Preference student-4 [module-xml module-python module-metadata module-digipres])
  (->Preference student-5 [module-xml module-python module-metadata module-digipres])
  (->Preference student-6 [module-xml module-python module-metadata module-digipres])
  (->Preference student-7 [module-xml module-python module-metadata module-digipres])
  (->Preference student-8 [module-xml module-python module-metadata module-digipres])])

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
    (let [ ;; the order here is intentional. Student 5 is
           ;; not on the same course as module-xml so is
           ;; a higher priority move candiate the students
           ;; 1-4.
           p1 (->Assignment module-xml student-5 0 0)
           p2 (->Assignment module-xml student-4 1 0)
           p3 (->Assignment module-xml student-3 2 0)
           p4 (->Assignment module-xml student-2 3 0)
           p5 (->Assignment module-xml student-1 4 0)] ;; more than cap
      (is (= [p1 p2 p3 p4 p5] (move-candidates [p5 p4 p3 p2 p1]))))))

(deftest test-current-assignments-for
  (testing "Assignments for student"
    (let [a1 (->Assignment module-xml student-1 0 0)
          a2 (->Assignment module-python student-1 0 0)
          a3 (->Assignment module-digipres student-2 0 0)]
      (is (= [a1 a2] (current-assignments-for [a1 a2 a3] student-1))))))

(deftest test-is-favoured
  (testing "Favoured modules"
    (is (= true (is-favoured (->Assignment module-xml student-1 0, 0))))
    (is (= false (is-favoured (->Assignment module-metadata student-1 0, 0))))))

(deftest test-preferences-for-student
  (testing "Preferences for student"
    (let [p1 (->Preference student-1 [module-xml module-python])
          p2 (->Preference student-2 [module-xml])]
      (is (=
            [
             (->ModulePreference module-xml student-1 0)
             (->ModulePreference module-python student-1 1)]
            (preferences-for-student [p1 p2] student-1))))))

(deftest test-remaining-preferences
  (testing "Remaining preferences for student"
    (let [p1 (->Preference student-1 [module-xml module-python module-digipres])
          p2 (->Preference student-2 [module-xml module-python])
          a1 (->Assignment module-xml student-1 0 0)
          a2 (->Assignment module-xml student-2 0 0)
          ]
      (is (= [
              (->ModulePreference module-python student-1 1)
              (->ModulePreference module-digipres student-1 2)
              ]
             (remaining-preferences [a1 a2] [p1 p2] student-1)))
      (is (= [
              (->ModulePreference module-python student-2 1)
              ]
             (remaining-preferences [a1 a2] [p1 p2] student-2))))))

(deftest test-next-slot-candidate
  (testing "Next slot for an over-cap assignment"
    (let [p1 (->Preference student-1 [module-xml module-python module-digipres])
          p2 (->Preference student-2 [module-xml module-python])
          a1 (->Assignment module-xml student-1 0 0)
          a2 (->Assignment module-xml student-2 0 0)
          ]
      (is (= (->ModulePreference module-python student-1 1)
             (next-slot-candidate [a1 a2] [p1 p2] a1))))))

(deftest test-reassign-and-move
  (testing "Reassignment and move"
    (let [
         a1 (->Assignment module-xml student-1 0 0)
         a2 (->Assignment module-xml student-2 0 0)
         a3 (->Assignment module-xml student-3 0 0)
         a4 (->Assignment module-xml student-4 0 0)
         a5 (->Assignment module-xml student-5 0 0)
         p1 (->Preference student-1 [module-xml module-python])
         p2 (->Preference student-2 [module-xml module-python])
         p3 (->Preference student-3 [module-xml module-python])
         p4 (->Preference student-4 [module-xml module-python])
         p5 (->Preference student-5 [module-xml module-python])
          ]
    (let [[assign modpref] (reassign [a1 a2 a3 a4 a5] [p1 p2 p3 p4 p5])]
     (is (= student-5 (:student assign)))
     (is (= 1 (:choice modpref)))
     (is (= module-python (:module modpref)))
     (is (= module-xml (:module assign)))

      (let [newassignments (move-student [a1 a2 a3 a4 a5] [p1 p2 p3 p4 p5] assign modpref)
            assign (->Assignment module-python student-5 1 1)]
        (is (= 5 (count newassignments)))
        (is (= assign (last newassignments)))
        )))))


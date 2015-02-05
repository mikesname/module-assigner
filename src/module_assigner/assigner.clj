(ns module-assigner.assigner)

;; A record representing a module to which a student can be assigned
(defrecord Module 
  [num course term code name])

;; A record representing a student's preferences for term1 and term2 modules
(defrecord Preference 
  [sid name course t1-p1 t1-p2 t1-p3 t1-p4 t2-p1 t2-p2 t2-p3 t2-p4])

;; A record representing a student's term1 and term2 assignments
(defrecord Assignment 
  [sid name t1-a1 t1-a2 t2-a1 t2-a2]) 

(defn assign 
  "Given the available modules, a student preferences and the maximum
  and minimum students per module, generate a set of student assignments"
  [modules prefs module-max module-min]
  
  [])

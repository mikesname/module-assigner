(ns module-assigner.assigner)

(defrecord Course [name])

;; A record representing a module to which a student can be assigned
(defrecord Module [id name course])

(defrecord Student [id name course])

(defrecord Assignment
  [module student choice try])


(defn is-favoured [assignment]
  (= 
    (get-in assignment [:module :course])
    (get-in assignment [:student :course])))

(defn by-modules [assignments]
  "turn a list of assignments into a map of module -> set(students)"
  (reduce
    (fn [m assignment]
      (let [module (:module assignment) student (:student assignment)]
        (let [sset (if (contains? m module)
                     (conj (get m module) student)
                     #{student})]
          (assoc m module sset))))
    {}
    assignments))

(defn module-count [assignments module]
  "count the number of students in a module"
  (count (get (by-modules assignments) module)))


(defn assign 
  "Given the available modules, a student preferences and the maximum
  and minimum students per module, generate a set of student assignments"
  [modules prefs module-max module-min]
  
  [])

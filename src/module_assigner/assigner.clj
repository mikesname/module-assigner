(ns module-assigner.assigner)

(def module-cap 4)
(def modules-per-student 2)
(def max-tries 200)

(defrecord Course [name])

;; A record representing a module to which a student can be assigned
(defrecord Module [id name course])

(defrecord Student [id name course])

(defrecord Preference [student modules])

(defrecord ModulePreference [module student choice])

(defrecord Assignment
  [module student choice try])

(defrecord Board
  [modules preferences assignments attempt])

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

(defn over-cap-assignments [assignments]
  "filter assignments that exist in over-assigned modules"
  (filter #(> (module-count assignments (:module %)) module-cap)
          assignments))

(defn move-candidates [assignments]
  """get move candidates (assignments to over-subscribed modules)
     and order them by priority"""
  (sort-by
    (juxt :choice :try is-favoured)
    (over-cap-assignments assignments)))

(defn next-move-candidate [assignments]
  "get the highest priority move candidate"
  (first (move-candidates assignments)))

(defn current-assignments-for [assignments student]
  "get the assignments that pertain to a given student"
  (filter #(= student (:student %)) assignments))

(defn preferences-for-student [preferences student]
  "get the preferences for a given student as a module preference"
  (let [p (first (filter #(= student (:student %)) preferences))]
    (let [modules (:modules p)]
      (map-indexed #(->ModulePreference %2 student %1) modules))))

(defn remaining-preferences [assignments preferences student]
  "get the remaining preferences for a given student"
  (let [
        ;; modules currently assigned for the student
        current (map #(:module %) (current-assignments-for assignments student))
        ;; full list of module preferences
        mod-prefs (preferences-for-student preferences student)]
    ;; get the prefs to which the student isn't currently assigned
    (let [rems (into [] (filter
              (fn [mp] (not (some #(= (:module mp) %) current)))
            mod-prefs))]
      (sort-by (juxt :choice #(module-count assignments (:module %)) ) rems))))

(defn next-slot-candidate [assignments preferences assignment]
  "get the next best empty slot for a to-move assignment"
  (let [remaining (remaining-preferences assignments preferences (:student assignment))]
    (first remaining)))

(defn reassign [assignments preferences]
  "determine how to move someone from an over-cap module to the next best"
    (let [assign (next-move-candidate assignments)
          modpref (next-slot-candidate assignments preferences assign)]
      [assign modpref]))

(defn move-student [assignments preferences assign modpref]
  "move a student from an over-cap module to the next best"
  (let [remassignments (into [] (remove #(= assign %) assignments))
        reassignment (->Assignment (:module modpref) (:student assign) (:choice modpref) (+ 1 (:try assign)))
       ]
    (conj remassignments reassignment)))

(defn is-solved [assignments]
  (= 0 (count (over-cap-assignments assignments))))

(defn move-step [assignments preferences]
  "make one reassignment"
  (let [[assign modpref] (reassign assignments preferences)]
    (move-student assignments preferences assign modpref)))

(defn solve [assignments preferences iteration]
  "attempt to solve over-cap preferences"
  (if (> iteration 200)
    (throw (Exception. "Over 200 iterations!"))
    (if (is-solved assignments)
      assignments
      (solve (move-step assignments preferences) preferences (inc iteration)))))

(defn- prefs-to-initial-assignment [pref]
  """given the module count per student, generate a set of 
     initial assignments."""
  (let [student (:student pref) modules (:modules pref)]
    (map-indexed (fn [i,m] (->Assignment m student i 0)) (take modules-per-student modules))))

(defn assign-initial [preferences]
  "Given a set of student preferences and the maximum
  and minimum students per module, generate a set of student assignments"
  (flatten (into [] (map prefs-to-initial-assignment preferences))))


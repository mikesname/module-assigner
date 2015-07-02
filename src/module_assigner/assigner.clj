(ns module-assigner.assigner)

(use '[clojure.set :only (difference)]
     '[clojure.string :only (join)])

;;(def module-cap 4)
(def modules-per-student 2)
(def max-tries 200)

(defrecord Course [name])

;; A record representing a module to which a student can be assigned
(defrecord Module [id name course])

;; A record representing a student with a given course
(defrecord Student [id name course])

;; A student's module preferences, in order
(defrecord Preference [student modules])

;; A single student module preference
(defrecord ModulePreference [module student choice])

;; An assignment of a student to a given course
(defrecord Assignment
  [module student choice try tag])

;; A reassignment from one module to another
(defrecord Move [student from to])

(defrecord Board
  [modules preferences assignments moves cap])

(defn- prefs-to-initial-assignment [pref]
  """given the module count per student, generate a set of 
    initial assignments."""
  (let [student (:student pref) modules (:modules pref)]
    (let [inits (map-indexed (fn [i, module] (->Assignment module student i 0 0)) (take modules-per-student modules))]
      ;; set the assignment tag with the index of the list
      (map-indexed (fn [i, assign] (assoc-in assign [:tag] i)) inits))))

(defn assign-initial [preferences]
  "Given a set of student preferences and the maximum
  and minimum students per module, generate a set of student assignments"
  (flatten (into [] (map prefs-to-initial-assignment preferences))))

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

(defn by-students [assignments]
  "turn a list of assignments into a map of student -> seq(modules)"
  (reduce
    (fn [s assignment]
      (let [module (:module assignment) student (:student assignment)]
        (let [sset (if (contains? s student)
                     (conj (get s student) module)
                     #{module})]
          (assoc s student sset))))
    {}
    assignments))

(defn init-board [preferences assignments cap]
  (->Board
    (into [] (keys (by-modules assignments)))
    preferences
    assignments
    []
    cap))

(defn init-board-with-modules [modules preferences cap]
  (->Board modules preferences (assign-initial preferences) [] cap))

(defn is-favoured [assignment]
  """an assignment is favoured if the module course is the
     same as the student's course"""
  (=
   (-> assignment :module :course)
   (-> assignment :student :course)))

(defn module-count [assignments module]
  "count the number of students in a module"
  (count (get (by-modules assignments) module)))

(defn over-cap-assignments [assignments cap]
  "filter assignments that exist in over-assigned modules"
  (filter #(> (module-count assignments (:module %)) cap)
          assignments))

(defn assignment-weight [assignment]
  """the relative weight on an assignment, based on the
    choice, the try, and whether or not it is favoured
    because the student is on the same course as the
    module"""
  (vec ((juxt (comp not is-favoured) :choice :try) assignment)))

(defn move-candidates [assignments cap]
  """get move candidates (assignments to over-subscribed modules)
    and order them by priority"""
  (reverse (sort-by
    assignment-weight
    (over-cap-assignments assignments cap))))

(defn next-move-candidate [assignments cap]
  "get the highest priority move candidate"
  (first (move-candidates assignments cap)))

(defn current-assignments-for [assignments student]
  "get the assignments that pertain to a given student"
  (filter #(= student (:student %)) assignments))

(defn modules-for-student [preferences student]
  "get the list of modules in the preferences for a given student"
  (:modules (first (filter #(= student (:student %)) preferences))))

(defn preferences-for-student [preferences student]
  "get the preferences for a given student as a module preference"
  (let [p (first (filter #(= student (:student %)) preferences))]
    (let [modules (:modules p)]
      (map-indexed #(->ModulePreference %2 student %1) modules))))

(defn under-cap-modules [assignments modprefs cap]
  "get modules still not at capacity"
  (filter #(< (module-count assignments (:module %)) cap) modprefs))

(defn format-assignment [assign preferences]
  (let [student (:student assign) module (:module assign)]
    (format "%-10s %-5s : %-15s (choice %d try %d) : %s" 
          (:name student)
          (str "(" (-> student :course :name) ")")
          (:name module)
          (:choice assign)
          (:try assign)
          (into [] (map :name (modules-for-student preferences student))))))

(defn format-module-count [module students] 
  (format "%-10s : %d" (:name module) (count students)))

(defn print-solution [board]
  "print out a set of assignments in a readable way"
  (let [assignments (:assignments board)
        preferences (:preferences board)]
    (doseq [f (sort-by (fn [a] (-> a :student :name)) assignments)]
      (println (format-assignment f preferences)))))

(defn print-report [board]
  "print out details of the current state"
  (let [assignments (:assignments board)
        preferences (:preferences board)
        iteration (count (:moves board))
        cap (:cap board)]
    (print-solution board)
    (println)
    (doseq [[m s] (by-modules assignments)] (println (format-module-count m s)))
    (println)
    (println "Over-cap:  " (count (over-cap-assignments assignments cap)))
    (println "Iteration: " iteration)
    (println)))

(defn print-moves [board]
  """print a report of which students have been reassigned
  from their initial preferences"""
  (let [moves (:moves board) preferences (:preferences board)]
    (doseq [move moves]
      (let [student (:student move) from (:from move) to (:to move)]
        (println (format "Moved student %s from %s to %s"
                 (:name student)
                 (:name from)
                 (:name to)))))))

(defn remaining-preferences [assignments preferences student cap]
  "get the remaining preferences for a given student"
  (let [
        ;; modules currently assigned for the student
        current (map #(:module %) (current-assignments-for assignments student))
        ;; full list of module preferences
        modprefs (preferences-for-student preferences student)]
    ;; get the prefs to which the student isn't currently assigned
    (let [rems (into [] (filter
                          (fn [mp] (not (some #(= (:module mp) %) current)))
                          modprefs))]
      (let [sorted (sort-by (juxt :choice #(module-count assignments (:module %)) ) rems)
            undercap (under-cap-modules assignments sorted cap)]
      ;; (let [sorted (sort-by #(module-count assignments (:module %)) rems)]
      ;;   (println "REMAINING, SORTED: " (-> student :name) (map #(-> % :module :name) sorted))
        ;;
        ;;(println "Capacity: " cap)
        ;;(println "Sorted: " (map #(-> % :module :name) sorted))
        ;;(println "Undercap: " (map #(-> % :module :name) undercap))
        undercap))))

(defn- pick-random-slot [assignments preferences student]
  "given that none of a student's preferences can be fulfilled, pick a random slot for them."
  ;; TODO
  nil)

(defn next-slot-candidate [assignments preferences assignment cap]
  "get the next best empty slot for a to-move assignment"
  (let [remaining (remaining-preferences assignments preferences (:student assignment) cap)]
    ;;(println "Remaining prefs for " (-> assignment :student :name) (map #(-> % :module :name) remaining))
    (if (empty? remaining)
      (pick-random-slot assignments preferences (:student assignment))
      (first remaining))))

(defn find-best-candidate [assignments candidates preferences cap]
  (loop [cands candidates]
    ;;(println "Looking for best candidate in" (map #(-> % :module :name) assignments))
    (if (empty? cands)
      nil
      (let [assign (first cands)
            modpref (next-slot-candidate assignments preferences assign cap)]
        (if (nil? modpref)
          (recur (rest cands))
          [assign modpref])))))

(defn reassign [board]
  "determine how to move someone from an over-cap module to the next best"
  (let [assignments (:assignments board)
        cap (:cap board)
        preferences (:preferences board)
        candidates (move-candidates assignments cap)]
    (let [[assign modpref] (find-best-candidate assignments candidates preferences cap)]
      (when (nil? assign)
        (print-report board)
        (throw (ex-info "No move candidate found!" {:board board})))
      (assert assign)
      (assert modpref)
      [assign modpref])))

(defn move-student [assignments preferences assign modpref]
  "move a student from an over-cap module to the next best"
  ;; (println (format "moving %s from %s (%d) to %s (%d)"
  ;;                  (-> assign :student :name)
  ;;                  (-> assign :module :name)
  ;;                  (module-count assignments (-> assign :module))
  ;;                  (-> modpref :module :name)
  ;;                  (module-count assignments (-> modpref :module))))
  (let [remassignments (into [] (remove #(= assign %) assignments))
        reassignment (->Assignment (:module modpref) (:student assign) (:choice modpref) (+ 1 (:try assign)) (:tag assign))
        ]
    (conj remassignments reassignment)))

(defn is-solved [board]
  (= 0 (count (over-cap-assignments (:assignments board) (:cap board)))))

(defn move-step [board]
  "make one reassignment"
  (let [
        [assign modpref] (reassign board)
        newassignments (move-student (:assignments board) (:preferences board) assign modpref)
        move (->Move (:student assign) (:module assign) (:module modpref))]
    (update-in (assoc board :assignments newassignments) [:moves] conj move)))

(defn solve [board]
  "attempt to solve over-cap preferences"
  (loop [newboard board
         iteration 0]
    ;;(print-report assigns (:preferences board) (:cap board) iteration)
    (if (> iteration 200)
      (throw (Exception. "Over 200 iterations!"))
      (if (is-solved newboard)
        newboard
        (recur (move-step newboard) (inc iteration))))))


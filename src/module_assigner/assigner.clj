(ns module-assigner.assigner)

(use '[clojure.set :only (difference)]
     '[clojure.string :only (join)])

;;(def module-cap 4)
(def modules-per-student 2)
(def max-tries 200)

(defrecord Course [name])

;; A record representing a module to which a student can be assigned
(defrecord Module [id name course term])

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

(defn- prefs-to-initial-assignment
  "given the module count per student, generate a set of initial assignments."
  [pref]
  (let [student (:student pref) modules (:modules pref)]
    (let [inits (map-indexed (fn [i, module] (->Assignment module student i 0 0)) (take modules-per-student modules))]
      ;; set the assignment tag with the index of the list
      (map-indexed (fn [i, assign] (assoc-in assign [:tag] i)) inits))))

(defn assign-initial
  "Given a set of student preferences and the maximum
  and minimum students per module, generate a set of student assignments"
  [preferences]
  (flatten (into [] (map prefs-to-initial-assignment preferences))))

(defn by-modules
  "turn a list of assignments into a map of module -> set(students)"
  [assignments]
  (reduce
    (fn [m assignment]
      (let [module (:module assignment) student (:student assignment)]
        (let [sset (if (contains? m module)
                     (conj (get m module) student)
                     #{student})]
          (assoc m module sset))))
    {}
    assignments))

(defn by-students
  "turn a list of assignments into a map of student -> seq(modules)"
  [assignments]
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

(defn is-favoured
  "an assignment is favoured if the module course is the same as the student's course"
  [assignment]
  (=
   (-> assignment :module :course)
   (-> assignment :student :course)))

(defn module-count
  "count the number of students in a module"
  [assignments module]
  (count (get (by-modules assignments) module)))

(defn over-cap-assignments
  "filter assignments that exist in over-assigned modules"
  [assignments cap]
  (filter #(> (module-count assignments (:module %)) cap)
          assignments))

(defn assignment-weight
  "the relative weight on an assignment, based on the
  choice, the try, and whether or not it is favoured
  because the student is on the same course as the
  module"
  [assignment]
  (vec ((juxt (comp not is-favoured) :choice :try) assignment)))

(defn move-candidates
  "get move candidates (assignments to over-subscribed modules)
  and order them by priority"
  [assignments cap]
  (reverse (sort-by
             assignment-weight
             (over-cap-assignments assignments cap))))

(defn current-assignments-for
  "get the assignments that pertain to a given student"
  [assignments student]
  (filter #(= student (:student %)) assignments))

(defn modules-for-student
  "get the list of modules in the preferences for a given student"
  [preferences student]
  (:modules (first (filter #(= student (:student %)) preferences))))

(defn preferences-for-student
  "get the preferences for a given student as a module preference"
  [preferences student]
  (let [p (first (filter #(= student (:student %)) preferences))]
    (let [modules (:modules p)]
      (map-indexed #(->ModulePreference %2 student %1) modules))))

(defn under-cap-modules
  "get modules still not at capacity"
  [assignments modules cap] 
  (filter #(< (module-count assignments %) cap) modules))

(defn under-cap-module-prefs
  "get modules still not at capacity"
  [assignments modprefs cap]
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

(defn print-solution
  "print out a set of assignments in a readable way"
  [board]
  (let [assignments (:assignments board)
        preferences (:preferences board)]
    (doseq [f (sort-by (fn [a] (-> a :student :name)) assignments)]
      (println (format-assignment f preferences)))))

(defn print-report
  "print out details of the current state"
  [board]
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

(defn print-moves
  "print a report of which students have been reassigned
    from their initial preferences"
  [board]
  (let [moves (:moves board) preferences (:preferences board)]
    (doseq [move moves]
      (let [student (:student move) from (:from move) to (:to move)]
        (println (format "Moved student %s from %s to %s"
                         (:name student)
                         (:name from)
                         (:name to)))))))

(defn remaining-preferences
  "get the remaining preferences for a given student"
  [assignments preferences student cap]
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
            undercap (under-cap-module-prefs assignments sorted cap)]
        ;; (let [sorted (sort-by #(module-count assignments (:module %)) rems)]
        ;;   (println "REMAINING, SORTED: " (-> student :name) (map #(-> % :module :name) sorted))
        ;;
        ;;(println "Capacity: " cap)
        ;;(println "Sorted: " (map #(-> % :module :name) sorted))
        ;;(println "Undercap: " (map #(-> % :module :name) undercap))
        undercap))))

(defn- pick-random-slot
  "given that none of a student's preferences can be fulfilled, pick a random slot for them."
  [modules assignments preferences student cap]
  ;; TODO
  (under-cap-modules assignments modules cap))

(defn next-slot-candidate
  "get the next best empty slot for a to-move assignment"
  [modules assignments preferences assignment cap]
  (let [remaining (remaining-preferences assignments preferences (:student assignment) cap)]
    ;;(println "Remaining prefs for " (-> assignment :student :name) (map #(-> % :module :name) remaining))
    (if (empty? remaining)
      (pick-random-slot modules assignments preferences (:student assignment) cap)
      (first remaining))))

(defn find-best-candidate [modules assignments candidates preferences cap]
  (loop [cands candidates]
    ;;(println "Looking for best candidate in" (map #(-> % :module :name) assignments))
    (if (empty? cands)
      nil
      (let [assign (first cands)
            modpref (next-slot-candidate modules assignments preferences assign cap)]
        (if (nil? modpref)
          (recur (rest cands))
          [assign modpref])))))

(defn reassign
  "determine how to move someone from an over-cap module to the next best"
  [board]
  (let [modules (:modules board)
        assignments (:assignments board)
        cap (:cap board)
        preferences (:preferences board)
        candidates (move-candidates assignments cap)]
    (let [[assign modpref] (find-best-candidate modules assignments candidates preferences cap)]
      (when (nil? assign)
        (print-report board)
        (throw (ex-info "No move candidate found!" {:board board})))
      (assert assign)
      (assert modpref)
      [assign modpref])))

(defn move-student
  "move a student from an over-cap module to the next best"
  [assignments preferences assign modpref]
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

(defn move-step
  "make one reassignment"
  [board]
  (let [
        [assign modpref] (reassign board)
        newassignments (move-student (:assignments board) (:preferences board) assign modpref)
        move (->Move (:student assign) (:module assign) (:module modpref))]
    (update-in (assoc board :assignments newassignments) [:moves] conj move)))

(defn solve
  "attempt to solve over-cap preferences"
  [board]
  (loop [newboard board
         iteration 0]
    ;;(print-report assigns (:preferences board) (:cap board) iteration)
    (if (> iteration max-tries)
      (throw (Exception. (format "Over %d iterations!" max-tries)))
      (if (is-solved newboard)
        newboard
        (recur (move-step newboard) (inc iteration))))))

(defn combine-boards
  "combine two boards"
  [board other]
  (->Board
    (concat (:modules board) (:modules other))
    (concat (:preferences board) (:preferences other))
    (concat (:assignments board) (:assignments other))
    (concat (:moves board) (:moves other))
    (:cap board)))

(defn calculate-for-term
  "solve for a particular term"
  [modules preferences per-module term]
  (let [mf (fn [m] (= (:term m) term))
        pf (fn [p] (assoc p :modules (filter mf (:modules p))))
        mt (filter mf modules)
        pt (map pf preferences)]
    (solve (init-board-with-modules mt pt per-module))))

(defn calculate-terms
  "solve board for each term"
  [modules preferences per-module terms]
  (let [boards (map (fn [t] (calculate-for-term modules preferences per-module t)) terms)]
    (reduce combine-boards boards)))



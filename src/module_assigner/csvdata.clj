(ns module-assigner.csvdata)

(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(use '[module-assigner.assigner :refer :all])

(defn- parse-id
  "parse a numeric id at a given column. The line is for info only"
  [desc line col data]
  (println "Parsing data", desc, line, col, data)
  (try
    (Integer/parseInt (.trim (nth data col)))
    (catch NumberFormatException e
      (throw (ex-info (str (format "Bad data at line: %d, column %d ", line, col)
                           (format "attempting to read %s as a number", desc))
                      {:line line :column col :description desc})))))


(defn- read-module [line & args]
  "create a module from flat data: id, course id, mod-id, name"
  (when (not (= 5 (count args)))
    (throw (ex-info (str (format "Bad data at line: %d. " line)
                                           "Module data should consist of: "
                                           "id, course id, module id, module name")
                    {:line line})))
  (->Module
    (parse-id "module id" line 0 args)
    (nth args 4)
    (->Course (nth args 1))))

(defn- read-preference [mods line & args]
  "create a student preference from flat data: sid, name, course, p1-p4"
  (println "read prefs" mods line args)
  (defn find-mod [desc col]
    (let [mod
          (first (filter #(= (:id %) (parse-id desc line col args)) mods))]
      (if (nil? mod)
        (throw (ex-info (str (format "Unknown module reference at line: %d, column %d ", line, col)
                             (format "for %s", desc))
                        {:line line :column col :description desc}))
        mod)))

  (when (not (= 7 (count args)))
    (throw (ex-info (str (format "Bad data at line: %d. " line)
                                           "Preference data should consist of: "
                                           "student id, student name, course id, "
                                           "choice 1, choice 2, choice 3, choice 4")
                    {:line line})))
  (->Preference
    (->Student 
      (parse-id "student id", line, 0, args)
      (nth args 1)
      (->Course (nth args 2)))
    [(find-mod "choice 1", 3)
     (find-mod "choice 2", 4)
     (find-mod "choice 3", 5)
     (find-mod "choice 4", 6)]))

(defn read-modules 
  "read a set of module data from a reader"
  [reader]
  (doall
    (map-indexed (partial apply read-module) (csv/read-csv reader))))

(defn read-preferences
  "read a set of student preferences, given a set of modules"
  [mods reader]
  (doall
    (map-indexed (partial apply read-preference mods) (csv/read-csv reader))))

(defn write-results
  "write results to a csv"
  [results])


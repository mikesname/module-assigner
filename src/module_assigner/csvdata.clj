(ns module-assigner.csvdata)

(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(use '[module-assigner.assigner :refer :all])

(defn- read-module [& args]
  "create a module from flat data: id, course id, mod-id, name"
  (->Module
    (read-string (nth args 0))
    (nth args 4)
    (->Course (nth args 1))))

(defn- read-preference [mods & args]
  "create a student preference from flat data: sid, name, course, p1-p4"
  (defn find-mod [id]
    (first (filter #(= (:id %) id) mods)))

  (->Preference
    (->Student 
      (read-string (nth args 0))
      (nth args 1)
      (->Course (nth args 2)))
    [(find-mod (read-string (nth args 3)))
     (find-mod (read-string (nth args 4)))
     (find-mod (read-string (nth args 5)))
     (find-mod (read-string (nth args 6)))]))

(defn read-modules 
  "read a set of module data from a reader"
  [reader]
  (doall
    (map (partial apply read-module) (csv/read-csv reader))))

(defn read-preferences
  "read a set of student preferences, given a set of modules"
  [mods reader]
  (doall
    (map (partial apply read-preference mods) (csv/read-csv reader))))


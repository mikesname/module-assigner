(ns module-assigner.csvdata
  (:require [clojure.tools.logging :as log]))

(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(use '[module-assigner.assigner :refer :all])

(defn- parse-id
  "parse an id at a given column. The line is for info only"
  [desc line col data]
  (log/debug "Parsing data", desc, line, col, data)
  (let [id (.trim (nth data col))]
    (if (clojure.string/blank? id)
      (throw (ex-info (str (format "Bad id %s at line: %d, column %d. " desc line col)
                                             "Cannot be empty/blank")
                       {:line line :column col :description desc}))
      id)))

(defn- parse-int-id
  "parse an integer id at a given column. The line is for info only"
  [desc line col data]
  (try
    (Integer/parseInt (parse-id desc line col data))
    (catch NumberFormatException e
      (throw (ex-info (str (format "Bad id %s at line: %d, column %d" desc line col)
                                           "Should be an integer number"))
                       {:line line :column col :description desc}))))

(defn- read-module
  "create a module from flat data: id, course id, mod-id, name"
  [line & args]
  (let [data (map #(.trim %) args)]
    (when (not (= 5 (count data)))
      (throw (ex-info (str (format "Bad data at line: %d. " line)
                                             "Module data should consist of: "
                                             "id, course id, module id, module name")
                      {:line line})))
    (->Module
      (parse-id "module id" line 0 data)
      (nth data 4)
      (->Course (nth data 1))
      (parse-int-id "term" line 2 data))))

(defn- read-preference
  "create a student preference from flat data: sid, name, course, p1-p4"
  [mods line & args]
  (let [data (map #(.trim %) args)]
    (log/debug "read prefs" mods line data)

    (defn find-mod [desc col]
      (let [mod
            (first (filter #(= (:id %) (parse-id desc line col data)) mods))]
        (if (nil? mod)
          (throw (ex-info (str (format "Unknown module reference at line: %d, column %d ", line, col)
                               (format "for %s", desc))
                          {:line line :column col :description desc}))
          mod)))

    (when (not (= 7 (count data)))
      (throw (ex-info (str (format "Bad data at line: %d. " line)
                                             "Preference data should consist of: "
                                             "student id, student name, course id, "
                                             "choice 1, choice 2, choice 3, choice 4")
                      {:line line})))
    (->Preference
      (->Student
        (parse-id "student id", line, 0, data)
        (nth args 1)
        (->Course (nth data 2)))
      [(find-mod "choice 1", 3)
       (find-mod "choice 2", 4)
       (find-mod "choice 3", 5)
       (find-mod "choice 4", 6)])))

(defn- filtered-data
  "clean up the csv by removing blank lines"
  [reader]
  (filter #(> (count %) 1) (csv/read-csv reader)))

(defn- reader-from-file-path [path]
  (io/reader path))

(defn read-modules
  "read a set of module data from a reader"
  [reader]
  (doall
    (map-indexed (partial apply read-module) (filtered-data reader))))

(defn read-modules-from-file
  "read module data from a file path"
  [path]
  (read-modules (io/reader path)))

(defn read-preferences
  "read a set of student preferences, given a set of modules"
  [mods reader]
  (doall
    (map-indexed (partial apply read-preference mods) (filtered-data reader))))

(defn read-preferences-alt
  "read preferences in the alternate format
    consisting of student id, name, and course
    followed by numbers expressing preference for
    modules in subsequent columns"
  [mods reader]
  (let [raw (map-indexed (fn [& args] args) (filtered-data reader))]
    (let [[[_ & [titles & _]] & rows] raw
          modidx (drop 3 titles)]

      (defn lookup [[r data]]
        (let [[sid & [name & [course & prefs]]] data
          m (vals (into (sorted-map) (map-indexed (fn [i p]
                         [p (nth modidx i)]) prefs)))]
          (concat [r sid name course] m)))

      (map (comp (partial apply read-preference mods) lookup) rows))))

(defn read-preferences-from-file
  "read preferences data from a file path with the given modules"
  [mods path]
  (read-preferences mods (io/reader path)))

(defn read-preferences-alt-from-file
  "read alt-format preferences data from a file path with the given modules"
  [mods path]
  (read-preferences-alt mods (io/reader path)))

(defn write-results
  "write results to a csv, one record per student"
  [board]
  (defn assigns-to-data [[student modules]]
    (into [(str (:id student)) (:name student)] (sort (map #(str (:id %))  modules))))
  (let [
        data (sort-by #(:id (first %)) (by-students (:assignments board)))
        swriter (new java.io.StringWriter)]
    (csv/write-csv swriter (map assigns-to-data data))
   (.toString swriter)))


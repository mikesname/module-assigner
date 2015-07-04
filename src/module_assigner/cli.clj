(ns module-assigner.cli
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(use '[module-assigner.assigner :refer :all]
     '[module-assigner.csvdata :refer :all])

(def cli-options
  [["-n" "--per-module NUM" "Students per module"
    :default 10
    :parse-fn #(Integer/parseInt %)]   
   ["-v" nil "Verbosity level"
    :id :verbosity
    :default 0
    :assoc-fn (fn [m k _] (update-in m [k] inc))]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Usage: module-assigner [options] modules preferences"
        ""
        "Options:"
        options-summary
        ""]
       (clojure.string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (clojure.string/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) (exit 0 (usage summary))
      (not= (count arguments) 2) (exit 1 (usage summary))
      errors (exit 1 (error-msg errors)))
    (try 
      (let [modules (read-modules-from-file (first arguments))
            preferences (read-preferences-from-file modules (second arguments))]
        (let [solved (solve (calculate-terms modules preferences (:per-module options) (range 1 3)))]
          (print (write-results solved))))
      (catch java.io.FileNotFoundException e
        (exit 2 (error-msg [(.getMessage e)]))))))


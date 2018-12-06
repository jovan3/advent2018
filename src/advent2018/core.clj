(ns advent2018.core
  (:require [advent2018.day1 :as day1]
            [advent2018.day2 :as day2]
            [clojure.java.io :as io]))

(defn resource-to-string [resource_filename]
  (slurp (io/file (io/resource resource_filename))))

(defn -main
  [& args]
  (comment (println (day1/day1 (resource-to-string "day1_input"))))
  (println (day2/day2 (resource-to-string "day2_input"))))


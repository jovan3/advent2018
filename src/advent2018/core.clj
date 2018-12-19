(ns advent2018.core
  (:require [advent2018.day1 :as day1]
            [advent2018.day2 :as day2]
            [advent2018.day3 :as day3]
            [advent2018.day4 :as day4]
            [advent2018.day5 :as day5]
            [advent2018.day6 :as day6]
            [advent2018.day7 :as day7]
            [advent2018.day8 :as day8]
            [advent2018.day9 :as day9]
            [clojure.java.io :as io]))

(defn resource-to-string [resource_filename]
  (slurp (io/file (io/resource resource_filename))))

(defn -main
  [& args]
  (comment (println (day1/day1 (resource-to-string "day1_input")))
           (println (day2/day2 (resource-to-string "day2_input")))
           (println (day3/day3 (resource-to-string "day3_input")))
           (println (day4/day4 (resource-to-string "day4_input")))
           (println (day5/day5 (resource-to-string "day5_input")))
           (println (day6/day6 (resource-to-string "day6_input")))
           (println (day7/day7 (resource-to-string "day7_input")))
           (println (day8/day8 (resource-to-string "day8_input"))))
  (println (day9/day9 (resource-to-string "day9_input"))))


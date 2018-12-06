(ns advent2018.day1
  (:require [clojure.string :as str]))

(defn parse-input [input-string]
  (let [values-string (str/split-lines input-string)]
    (map #(Integer/parseInt %) values-string)))

(defn incl? [element s] (some #(= element %) s))

(defn fresh-seq [initial seq] (if (empty? seq) (vec initial) seq))

(defn find-first-repeated [input-string]
  (loop [current 0
         visited-freqs [0]
         rest-freqs (vec input-string)]
    (let [rest-freqs (fresh-seq (vec input-string) rest-freqs)
          new-current (+ current (first rest-freqs))]
      (if (incl? new-current visited-freqs)
        new-current
        (recur new-current (conj visited-freqs new-current) (rest rest-freqs))))))

(defn day1 [input]
  (println "day 1 part 1" (reduce + (parse-input input)))
  (println "day 1 part 2" (find-first-repeated (parse-input input))))

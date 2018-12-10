(ns advent2018.day5
  (:require [clojure.string :as str]))

(defn regex-matcher [str]
  (re-matcher #"\[(.*)\] (.+)" str))

(defn upper? [string] (= string (str/upper-case string)))
(defn lower? [string] (= string (str/lower-case string)))

(defn remove-letter? [last-char next-char]
  (cond
    (or (nil? last-char) (nil? next-char)) false
    (not= (str/upper-case last-char) (str/upper-case next-char)) false
    (and (upper? last-char) (lower? next-char)) true
    (and (lower? last-char) (upper? next-char)) true
    :else false))

(defn remove-letter-pairs [input]
  (let [letters (str/split input #"")]
    (loop [remaining-letters letters
           processed-letters []
           i 0]
      (if (empty? remaining-letters)
        processed-letters
        (let [last-processed (last processed-letters)
              next-processed (first remaining-letters)
              remove? (remove-letter? last-processed next-processed)]
          (if remove?
            (recur (rest remaining-letters) (vec (butlast processed-letters)) (inc i))
            (recur (rest remaining-letters) (conj processed-letters next-processed) (inc i))))))))

(defn day5 [input]
  (println "day 5 part 1" (count (remove-letter-pairs input))))

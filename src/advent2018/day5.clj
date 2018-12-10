(ns advent2018.day5
  (:require [clojure.string :as str]))

(defn upper? [string] (= string (str/upper-case string)))
(defn lower? [string] (= string (str/lower-case string)))

(defn all-letters [string] (-> string
                               str/lower-case
                               (#(str/split % #""))
                               set
                               sort
                               vec))

(defn remove-letter? [last-char next-char]
  (cond
    (or (nil? last-char) (nil? next-char)) false
    (not= (str/upper-case last-char) (str/upper-case next-char)) false
    (and (upper? last-char) (lower? next-char)) true
    (and (lower? last-char) (upper? next-char)) true
    :else false))

(defn remove-letter-pairs [letters]
  (loop [remaining-letters letters
         processed-letters []
         i 0]
    (if (= (mod i 5000) 0) (println i))
    (if (empty? remaining-letters)
      processed-letters
      (let [last-processed (last processed-letters)
            next-processed (first remaining-letters)
            remove? (remove-letter? last-processed next-processed)]
        (if remove?
          (recur (rest remaining-letters) (vec (butlast processed-letters)) (inc i))
          (recur (rest remaining-letters) (conj processed-letters next-processed) (inc i)))))))

(defn remove-input-letter [letter letter-seq]
  (filter #(and (not= % letter) (not= % (str/upper-case letter))) letter-seq))

(defn find-minimum-length [input letters]
  (first (sort (pmap (fn [letter] (-> input
                                      (#(remove-input-letter letter %))
                                      remove-letter-pairs
                                      count)) letters))))

(defn day5 [input]
  (let [letters (str/split input #"")]
    (comment (println "day 5 part 1" (count (remove-letter-pairs letters))))
    (println (println "day 5 part 2" (find-minimum-length letters (all-letters input))))))

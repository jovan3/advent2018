(ns advent2018.day2
  (:require [clojure.string :as str]))

(defn parse-input [input-string]
  (str/split-lines input-string))

(defn incl? [element s] (some #(= element %) s))

(defn letter-freqs [str]
  (frequencies (str/split str #"")))

(defn frequency-count [string-seqs frequency]
  (let [letter-frequencies (map letter-freqs string-seqs)]
    (count (filter identity (map #(incl? frequency (vals %)) letter-frequencies)))))

(defn num-diff-letters [str1 str2]
  (let [str1-seq (str/split str1 #"") str2-seq (str/split str2 #"")]
    (loop [total 0
           string1-seq str1-seq
           string2-seq str2-seq]
      (if (empty? string1-seq)
        total
        (let [new-total (if (not= (first string1-seq) (first string2-seq)) (inc total) total)]
          (recur new-total (rest string1-seq) (rest string2-seq)))))))

(defn find-one-letter-diff-ids [string-seqs]
  (let [id-pairs (for [x string-seqs y string-seqs] [x y])]
    (map (fn [[x y]] [x y (num-diff-letters x y)]) id-pairs)))

(defn day2 [input]
  (let [letters-2x (frequency-count (parse-input input) 2)
        letters-3x (frequency-count (parse-input input) 3)]
    (println "day 2 part 1" (* letters-2x letters-3x)))
    (println "day 2 part 2" (filter #(= (nth % 2) 1) (find-one-letter-diff-ids (parse-input input)))))

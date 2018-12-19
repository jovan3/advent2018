(ns advent2018.day9
  (:require [clojure.string :as str]))

(defn str->int [str]
  (Integer/parseInt str))

(defn parse-input [str]
  (let [matcher (re-matcher #"(\d+) players; last marble is worth (\d+) points" str)]
    (re-find matcher)
    (let [[_ players points] (re-groups matcher)]
      {:players (str->int players) :points (str->int points)})))

(defn insert-at [list el n]
  (let [len (count list)
        first-part (conj (into [] (take n list)) el)]
    (into first-part (take-last (- len n) list))))

(defn remove-at [list index]
  (let [len (count list)
        first-part (take index list)
        last-part (take-last (- len (inc index)) list)]
    (into (into [] first-part) last-part)))

(defn empty-scoreboard [players]
  (into (sorted-map) (map (fn [player] [player 0]) (range 1 (inc players)))))

(defn add-to-player-score [scores player score-to-add]
  (update-in scores [player] #(+ % score-to-add)))

(defn play [turns players]
  (loop [marbles [0]
         current-player 1
         current-position 0
         turn 1
         scores (empty-scoreboard players)]
    (if (> turn turns)
      scores
      (let [next-player (inc (mod current-player players))
            marbles-count (count marbles)]
        (if (zero? (mod turn 23))
          (let [new-position (mod (- current-position 7) marbles-count)
                removed-marble (nth marbles new-position)
                updated-scores (add-to-player-score scores current-player (+ turn removed-marble))]
            (recur (remove-at marbles new-position) next-player new-position (inc turn) updated-scores))
          (let [new-position (mod (+ 2 current-position) marbles-count)]
            (recur (insert-at marbles turn new-position) next-player new-position (inc turn) scores)))))))

(defn day9 [input]
  (let [{players :players turns :points} (parse-input input)]
    (println "day 9 part 1" (first (second (sort-by second > (play turns players)))))))

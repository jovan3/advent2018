(ns advent2018.day4
  (:require [clojure.string :as str]))

(defn regex-matcher [str]
  (re-matcher #"\[(.*)\] (.+)" str))

(defn parse-event [event-text]
  (let [guard-matches (re-matches #"Guard (#\d+).*" event-text)]
    (cond
      (some? guard-matches) {:name :shift-start :id (last guard-matches)}
      (str/starts-with? event-text "falls") {:name :falls-asleep}
      (str/starts-with? event-text "wakes") {:name :wakes})))
      
(defn parse-line [line]
  (let [matcher (regex-matcher line)]
    (re-find matcher)
    (let [groups (re-groups matcher)]
      { :timestamp (nth groups 1)
       :text (last groups)
       :event (parse-event (last groups)) })))

(defn parse-input [input]
  (map parse-line (sort (str/split-lines input))))

(defn str->int [str]
  (Integer/parseInt str))

(defn diff-minutes [date-from-str date-to-str]
  (let [min-position (- (count date-from-str) 2)
        from-mins (str->int (subs date-from-str min-position))
        to-mins (str->int (subs date-to-str min-position))]
    {:total (- to-mins from-mins)
     :range (range from-mins to-mins)}))

(defn process-data [data]
  (loop [items data
         id (((first data) :event) :id)
         last-asleep-min nil
         sleep-times {}]
    (let [current-item (first items)]
      (if (nil? current-item)
        sleep-times
        (let [event-name ((current-item :event) :name)]
          (cond
            (= event-name :shift-start) (recur (rest items) ((current-item :event) :id) last-asleep-min sleep-times)
            (= event-name :falls-asleep) (recur (rest items) id (current-item :timestamp) sleep-times)
            (= event-name :wakes) (recur (rest items) id nil (update-in sleep-times [id :times] conj (diff-minutes last-asleep-min (current-item :timestamp))))))))))

(defn get-item-sleep-total [item]
  [(key item) (reduce + (map #(% :total) ((val item) :times)))])

(defn most-frequent-sleeping-minute [sleep-data]
  (first (first (sort-by val > (frequencies (flatten (map #(% :range) (sleep-data :times))))))))

(defn day4 [input]
  (let [sleep-data (process-data (parse-input input))]
    (let [sleep-times (apply hash-map (flatten (map get-item-sleep-total sleep-data)))
          id-most-asleep (first (sort-by val > sleep-times))
          minute-most-asleep (most-frequent-sleeping-minute (sleep-data (first id-most-asleep)))]
      (println "ID with longest sleep time" (first id-most-asleep))
      (println "minute most asleep" minute-most-asleep)
      (println "day 4 part 1" (* (str->int (subs (first id-most-asleep) 1)) minute-most-asleep)))))

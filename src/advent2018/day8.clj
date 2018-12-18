(ns advent2018.day8
  (:require [clojure.string :as str]
            [clojure.walk :as w]))


(defn process-input [input-str]
  (read-string (str "[" input-str "]")))

(defn metadata-items [data]
  (println (drop 2 data))
  (let [header (take 2 data)
        [children-count metadata-count] header
        items []]
    (if (= 0 (count data)) items)
    (if (= 0 children-count)
      (let [metadata (take-last metadata-count data)]
        (conj items [(count metadata) metadata]))
      (conj items (metadata-items (vec (drop 2 data)))))))

(defn f1 [data]
  (let [header (take 2 data)
        [children-count metadata-count] header
        remaining (drop 2 data)]
    (loop [children []
           remaining-children children-count
           remaining-data remaining]
      (if (= 0 remaining-children)
        [{:total (apply + (take metadata-count remaining-data))
          :children children}
         (drop metadata-count remaining-data)]
        (let [[child d] (f1 remaining-data)]
          (recur (conj children child)
                 (dec remaining-children)
                 d))))))

(defn parse-level [data]
  (let [header (take 2 data)
        [children-count metadata-count] header
        children-data (drop 2 (drop-last metadata-count data))
        metadata (take-last metadata-count data)]
    {:header header
     :children-data children-data
     :metadata metadata}))

(defn parse [input]
  (loop [tree {:metadata nil :children []}
         level-stack [tree]
         remaining input]
    (if (empty? remaining)
      tree
      (let [[children metadata] (take 2 input)
            current-level (peek level-stack)
            level-children (:children current-level)]))))

(defn extract-totals [m]
  (flatten (w/postwalk (fn [x] (if (map? x)
                                 (into [] (vals x))
                                 x)) m)))

(defn day8 [input]
  (let [processed-input (process-input input)]
    (println "day 8 part 1" (apply + (extract-totals (f1 processed-input))))))

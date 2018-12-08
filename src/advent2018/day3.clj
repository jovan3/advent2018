(ns advent2018.day3
  (:require [clojure.string :as str]))

(def matrix-x 1000)
(def matrix-y 1000)
(def empty-matrix (vec
                   (take matrix-x
                         (repeat
                          (vec
                           (take matrix-y (repeat 0)))))))

(defn regex-matcher [str]
  (re-matcher #"(#\d+) @ (\d+),(\d+): (\d+)x(\d+)" str))

(defn str->int [str]
  (Integer/parseInt str))

(defn parse-line [line]
  (let [matcher (regex-matcher line)]
    (re-find matcher)
    (let [match-groups (re-groups matcher)
          [_ id top-left-x-str top-left-y-str bottom-right-x-str bottom-right-y-str] match-groups
          tl_x (str->int top-left-x-str) tl_y (str->int top-left-y-str)
          br_x (str->int bottom-right-x-str) br_y (str->int bottom-right-y-str)]
      {:id id
       :tl [tl_x tl_y]
       :br [(+ tl_x br_x) (+ tl_y br_y)]})))

(defn parse-input [input]
  (map parse-line (str/split-lines input)))

(defn pos-inside [pos-x pos-y tl br matrix]
  (if (and
       (>= pos-x (first tl))
       (>= pos-y (last tl))
       (< pos-x (first br))
       (< pos-y (last br)))
    (inc (get-in matrix [pos-x pos-y]))
    (get-in matrix [pos-x pos-y])))

(defn put-on-matrix [matrix tl br]
  (vec (for [x (range matrix-x)]
    (let [y (range matrix-y)]
      (vec (map #(pos-inside x % tl br matrix) y))))))

(defn put-all-on-matrix [items]
  (loop [matrix empty-matrix
         items-seq items
         i 0]
    (println i)
    (let [item (first items-seq)]
      (if (nil? item)
        matrix
        (let [tl (item :tl)
              br (item :br)
              new-matrix (put-on-matrix matrix tl br)]
          (recur new-matrix (rest items-seq) (inc i)))))))

(defn day3 [input]
  (println "day 3 part 1"
           (let [parsed-input (parse-input input)
                 result (put-all-on-matrix parsed-input)]
             (count (filter #(> % 1) (flatten result))))))
  

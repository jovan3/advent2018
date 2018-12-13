(ns advent2018.day6
  (:require [clojure.string :as str]))

(defn abs [number] (Math/abs number))

(defn generate [radius center]
  (let [range (range (- radius) (inc radius))]
    (vec (for [x range y range :when (= radius (+ (abs x) (abs y)))]
      (vec (map + center [x y]))))))

(defn coordinate-value [coordinate radius origin grid]
  (let [point-occupied? (and (not= grid {}) (.contains (keys grid) coordinate))]
  (cond
    (not point-occupied?) (conj grid [coordinate {:state origin :radius radius}])
    (and point-occupied? (= ((grid coordinate) :radius) radius)) (assoc-in grid [coordinate] {:state "." :radius radius})
    :else grid)))

(defn make-circles [points radius]
  (map (fn [point] [point (generate radius point)]) points))

(defn update-grid-for-circle [point-circle grid radius]
  (let [[origin circle-points] point-circle]
    (reduce (fn [prev-grid point] (coordinate-value point radius origin prev-grid)) grid circle-points)))
 
(defn update-grid [grid point-circles radius]
  (reduce (fn [prev-grid point-circle] (update-grid-for-circle point-circle prev-grid radius)) grid point-circles))

(defn process-input [input-str]
  (map (fn [line] (vec (map #(Integer/parseInt %) (str/split line #", ")))) (str/split-lines input-str)))

(defn point-surface [point-coordinate grid]
  (count (filter #(= ((second %) :state) point-coordinate) grid)))

(defn find-convergent-areas [prev-point-areas new-point-areas]
  (let [hash-prev (into {} prev-point-areas) hash-new (into {} new-point-areas)]
    (filter #(= (hash-prev (first %)) (hash-new (first %))) new-point-areas)))

(defn do-iterations [n input-points]
  (loop [grid {}
         i 0
         current-point-areas []
         convergent-areas []]
    (println i)
    (if (> i n)
      {:grid grid :convergent-areas convergent-areas}
      (let [point-circles (make-circles input-points i)
            updated-grid (update-grid grid point-circles i)
            new-point-areas-surfaces (map (fn [p] [p (point-surface p updated-grid)]) input-points)
            convergent-areas (find-convergent-areas current-point-areas new-point-areas-surfaces)]
        (recur updated-grid (inc i) new-point-areas-surfaces convergent-areas)))))

(defn max-convergent-area [convergent-areas]
  (second (first (sort-by second > convergent-areas))))

(defn dist [point1 point2]
  (let [[x1 y1] point1
        [x2 y2] point2]
    (+ (abs (- x1 x2)) (abs (- y1 y2)))))

(defn sum-of-distances [point all-points]
  (reduce + (map #(dist point %) all-points)))

(defn part2 [input-points]
  (for [x (range -100 500) y (range -100 500) :when (< (sum-of-distances [x y] input-points) 10000)]
    [x y]))

(defn day6 [input]
  (let [input-points (process-input input)]
    (println "day 6 part 1" (max-convergent-area ((do-iterations 100 input-points) :convergent-areas)))
    (println "day 6 part 2" (count (part2 input-points)))))

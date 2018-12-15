(ns advent2018.day7
  (:require [clojure.string :as str]
            [clojure.set :as s]
            [ubergraph.core :as uber]))

(defn extract-graph-edges [str]
  (let [matcher (re-matcher #"Step (\w+) must be finished before step (\w+) can begin." str)]
    (re-find matcher)
    (let [match-groups (re-groups matcher)
          [_ step_before step_after] match-groups]
      [step_before step_after])))

(defn process-input [input-str]
  (map extract-graph-edges (str/split-lines input-str)))

(defn valid-successors [graph successor-nodes visited]
  (set (filter (fn [successor-node]
                 (every? (fn [predecessor] (contains? visited predecessor)) (uber/predecessors graph successor-node))) successor-nodes)))

(defn next-nodes [graph current-node available visited]
  (let [successor-nodes (set (uber/successors graph current-node))
        valid-successors (valid-successors graph successor-nodes visited)]
    (sort (s/difference (s/union valid-successors available) (set visited)))))
  
(defn traverse-graph [graph start-node]
  (loop [current-node start-node
         available #{}
         result [start-node]]
    (let [visited (set result)
          next-nodes (next-nodes graph current-node available visited)]
      (if (empty? next-nodes)
        result
        (let [next-node (first next-nodes)]
          (recur next-node (set (remove #{next-node} next-nodes)) (conj result next-node)))))))

(defn day7 [input]
  (let [graph (apply uber/digraph (process-input input))]
    (println "day 7 part 1" (str/join "" (traverse-graph graph "S")))))

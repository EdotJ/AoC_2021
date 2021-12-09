(ns aoc-9.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn get-point [curr-row, curr-col, heightmap, offset-col, offset-row]
  (let [row (+ curr-row offset-row)
        col (+ curr-col offset-col)]
    (if (and (> (count heightmap) row) (> (count (first heightmap)) col) (> row -1) (> col -1))
      (nth (nth heightmap row) col)
      nil)))

(defn top [curr-row, curr-col, heightmap]
  (get-point curr-row curr-col heightmap 0 -1))

(defn right [curr-row, curr-col, heightmap]
  (get-point curr-row curr-col heightmap 1 0))

(defn left [curr-row, curr-col, heightmap]
  (get-point curr-row curr-col heightmap -1 0))

(defn bot [curr-row, curr-col, heightmap]
  (get-point curr-row curr-col heightmap 0 1))

(defn get-adjacent [curr-row, curr-col, heightmap]
  (filter some? (vector (top curr-row curr-col heightmap)
    (right curr-row curr-col heightmap)
    (bot curr-row curr-col heightmap)
    (left curr-row curr-col heightmap))))

(defn filter-heightmap-cols [heightmap, curr-row-num, curr-row-vals]
  (keep-indexed #(if (every? (fn [num] (> num %2)) (get-adjacent curr-row-num %1 heightmap)) %2) curr-row-vals))

(defn filter-heightmap [heightmap]
  (keep-indexed #(filter-heightmap-cols heightmap %1 %2) heightmap))

(defn main []
  (let [heightmap (map #(map (fn [num] (Integer/parseInt num)) (string/split % #"")) (string/split (slurp (io/resource "input.txt")) #"\n"))
        low-points (filter-heightmap heightmap)]
    (println (apply + (map #(inc %) (flatten low-points))))
    ))

;keep-indexed #(if (= (apply min (get-adjacent %1 2 input)) %2) %2) input

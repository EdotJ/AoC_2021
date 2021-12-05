(ns aoc-5.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn get-input [line]
  (let [coords (string/split line #" -> ")
        x1y1 (string/split (first coords) #",")
        x2y2 (string/split (second coords) #",")]
    (hash-map :x1 (Integer/parseInt (first x1y1)) :y1 (Integer/parseInt (second x1y1))
              :x2 (Integer/parseInt (first x2y2)) :y2 (Integer/parseInt (second x2y2)))))

(defn get-minmax-point [minmax, coords, keys]
  (val (apply minmax val (select-keys coords keys))))

(defn get-line-covered-points [coords]
  (let [range-x (range (get-minmax-point min-key coords [:x1 :x2]) (+ 1 (get-minmax-point max-key coords [:x1 :x2])))
        range-y (range (get-minmax-point min-key coords [:y1 :y2]) (+ 1 (get-minmax-point max-key coords [:y1 :y2])))]
    (if (= (get coords :x1) (get coords :x2))
      (map #(hash-map :x (get coords :x1) :y %) range-y)
      (map #(hash-map :x % :y (get coords :y1)) range-x))))

(defn get-covered-points [coords]
  (map #(get-line-covered-points %) coords))

(defn get-answer-pt1 [covered-points]
  (count (filter #(> (second %) 1) (frequencies covered-points))))

(defn get-diagonal-range [entry, keys]
  (let [diag-range (range (get-minmax-point min-key entry keys) (+ 1 (get-minmax-point max-key entry keys)))]
    (if (apply > (map #(val %) (select-keys entry keys)))
      (reverse diag-range)
      diag-range)))

(defn vector-to-coords [vector]
  (hash-map :x (first vector) :y (second vector)))

(defn get-diagonal-covered-points [coords]
  (flatten (map #(map vector-to-coords (map vector (get-diagonal-range % [:x1 :x2]) (get-diagonal-range % [:y1 :y2]))) coords)))

(defn main []
  (let [lines (string/split (slurp (io/resource "input.txt")) #"\n")
        coordinates-pt1 (filter #(or (= (get % :x1) (get % :x2)) (= (get % :y1) (get % :y2))) (map #(get-input %) lines))
        covered-points-pt1 (flatten (get-covered-points coordinates-pt1))
        coordinates-pt2 (filter #(and (not (= (get % :x1) (get % :x2))) (not (= (get % :y1) (get % :y2)))) (map #(get-input %) lines))
        covered-points-pt2 (concat covered-points-pt1 (get-diagonal-covered-points coordinates-pt2))]
    (println "Part One")
    (println "Line Count: " (count coordinates-pt1))
    (println "Point Count: " (count (flatten covered-points-pt1)))
    (println "Answer: " (get-answer-pt1 covered-points-pt1))
    (println)
    (println "Part Two")
    (println "Point Count: " (count covered-points-pt2))
    (println "Answer: " (get-answer-pt1 covered-points-pt2))
    ))
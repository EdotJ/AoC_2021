(ns aoc-1.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


(defn solve-first [input]
  (count (filter #(not (zero? %)) (map-indexed #(if (> %1 0) (if (> %2 (nth input (- %1 1))) 1 0) 0) input))))

(defn num1 [coll, ind, val]
  (+ val (nth coll (- ind 1)) (nth coll (- ind 2))))

(defn num2 [coll, ind]
  (+ (nth coll (- ind 1)) (nth coll (- ind 2)) (nth coll (- ind 3))))

(defn solve-second [input]
  (count (filter #(not (zero? %)) (map-indexed #(if (> %1 2) (if (> (num1 input %1 %2) (num2 input %1)) 1 0) 0) input))))

(defn main []
  (let [file-contents (slurp (io/resource "input.txt"))
        nums-as-strings (string/split file-contents #"\n")
        numbers (map read-string nums-as-strings)]
    (println "Part 1: " (solve-first numbers))
    (println "Part 2: " (solve-second numbers))))
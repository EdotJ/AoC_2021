(ns aoc-8.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn get-part1 [input]
  (map #(map (fn [str] (count str)) (string/split (second (string/split % #" \| ")) #" ")) input))

(defn main []
  (let [input (string/split (slurp (io/resource "input.txt")) #"\n")]
    (println input)
    (println (count (filter #(some (fn [char-count] (= char-count %)) [2 3 4 7]) (flatten (get-part1 input)))))))

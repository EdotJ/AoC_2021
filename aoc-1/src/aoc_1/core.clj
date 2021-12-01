(ns aoc-1.core
    (:require [clojure.java.io :as io]
      [clojure.string :as string]))


(defn solve [input]
      (println (count (filter #(not (zero? %)) (map-indexed #(if (> %1 0) (if (> %2 (nth input (- %1 1))) 1 0) 0) input)))))

(defn main []
      (let [file-contents (slurp (io/resource "input.txt"))
            nums-as-strings (string/split file-contents #"\n")
            numbers (map read-string nums-as-strings)]
           (solve numbers)))


(ns aoc-6.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn get-val [current-val]
  (if (= current-val 0)
    6
    (dec current-val)))

(defn model [day-counter, input]
  (if (> day-counter 0)
    (recur (dec day-counter) (let [new-input (concat input (repeat (count (filter zero? input)) 9))] ; return 9 because it will be dec'ed in next step
                               (map #(get-val %) new-input)))
    input))

(defn get-new-vals [input]
  (vector (get input 1 0) (get input 2 0) (get input 3 0) (get input 4 0) (get input 5 0) (get input 6 0) (+ (get input 7 0) (get input 0 0)) (get input 8 0) (get input 0 0)))

(defn get-new-input [input]
  (zipmap [0 1 2 3 4 5 6 7 8] (get-new-vals input)))

(defn model-pt2 [day-counter, input]
  (if (> day-counter 0)
    (recur (dec day-counter) (get-new-input input))
    input))

(defn main
  []
  (let [input (map #(read-string %) (string/split (slurp (io/resource "input.txt")) #","))
        input-pt2 (frequencies input)
        day-counter 256
        results-pt2 (model-pt2 day-counter input-pt2)]
    (when (< day-counter 81)
                       (println "Recursive: " (count (model day-counter input))))
    (println "Ugly calc: " (reduce + (vals results-pt2)))))

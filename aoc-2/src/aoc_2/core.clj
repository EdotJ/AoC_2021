(ns aoc-2.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn get-aim [prev-commands]
  (reduce + (map #(case (get % :command)
                    "down" (get % :value)
                    "up" (- (get % :value))
                    "forward" 0)
                 prev-commands)))

(defn get-single-depth [step, prev-commands]
  (when (= (get step :command) "forward")
    (let [multiplier (if (zero? (get step :value)) 1 (get step :value))] (* multiplier (get-aim prev-commands)))))

(defn get-depth [command-list]
  (reduce + (filter some? (map-indexed #(get-single-depth %2 (take %1 command-list)) command-list))))

(defn calc-step [command-list, command]
  (reduce + (map #(get % :value) (filter #(= (get % :command) command) command-list))))

(defn solve-first [command-list]
  (let [horizontal-pos (calc-step command-list "forward")
        vertical-pos (- (calc-step command-list "down") (calc-step command-list "up"))]
    (* horizontal-pos vertical-pos)))

(defn solve-second [command-list]
  (let [depth (get-depth command-list)
        horizontal-pos (calc-step command-list "forward")]
    (* depth horizontal-pos)))

(defn main
  []
  (let [file-contents (slurp (io/resource "input.txt"))
        commands (string/split file-contents #"\n")
        command-list (map #(let [strings (string/split % #" ")] (assoc {} :command (first strings) :value (read-string (last strings)))) commands)]
    (println "Part One: " (solve-first command-list))
    (println "Part Two: " (solve-second command-list))))

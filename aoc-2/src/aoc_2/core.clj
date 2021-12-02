(ns aoc-2.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn calc-command [command-list, command]
  (reduce + (map #(get % :value) (filter #(= (get % :command) command) command-list))))

(defn solve-first [command-list]
  (let [horizontal-pos (calc-command command-list "forward")
        vertical-pos (- (calc-command command-list "down") (calc-command command-list "up"))]
    (* horizontal-pos vertical-pos)))


(defn get-pos [horizontal-pos, depth, aim]
  ((* horizontal-pos (* depth aim))))

(defn get-aim [prev-commands]
  (reduce + (map #(case (get % :command)
                    "down" (get % :value)
                    "up" (- (get % :value))
                    "forward" 0)
                 prev-commands)))

(defn get-depth [command-list]
  (reduce + (filter some? (map-indexed #(when (= (get %2 :command) "forward")
                                (let [multiplier (if (zero? (get %2 :value)) 1 (get %2 :value))] (* multiplier (get-aim (take %1 command-list)))))
                             command-list ))))

  (defn main
    []
    (let [file-contents (slurp (io/resource "input.txt"))
          commands (string/split file-contents #"\n")
          command-list (map #(let [strings (string/split % #" ")] (assoc {} :command (first strings) :value (read-string (last strings)))) commands)]
      (println "Part One: " (solve-first command-list))
      (println "Part Two: " (* (get-depth command-list) (calc-command command-list "forward")))))

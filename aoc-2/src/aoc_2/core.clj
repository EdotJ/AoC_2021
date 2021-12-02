(ns aoc-2.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn calc-command [command-list, command]
  (reduce + (map #(get % :value) (filter #(= (get % :command) command) command-list))))

(defn main
  []
  (let [file-contents (slurp (io/resource "input.txt"))
        commands (string/split file-contents #"\n")
        command-list (map #(let [strings (string/split % #" ")] (assoc {} :command (first strings) :value (read-string (last strings)))) commands)
        horizontal-pos (calc-command command-list "forward")
        vertical-pos (- (calc-command command-list "down") (calc-command command-list "up"))]
    (println (* horizontal-pos vertical-pos))))

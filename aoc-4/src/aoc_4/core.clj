(ns aoc-4.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(defn read-board [board]
  (map #(seq (filter (fn [str] (not (string/blank? str))) (string/split (string/trim %) #" "))) board))

(defn read-boards [lines]
  (let [board-lines (rest lines)]
    (map #(read-board %) (partition 5 board-lines))))

(defn get-col [board, col-num]
  (map #(nth % col-num) board))

(defn is-winning-vector [vector, drawn-nums]
  (every? #(some #{%} drawn-nums) vector))

(defn is-winning-board [board, drawn-nums]
  (or (some #(is-winning-vector % drawn-nums) board)
      (some #(is-winning-vector % drawn-nums) (map #(get-col board %) (range 5)))))

(defn get-winners [drawn-nums, boards]
  (filter #(is-winning-board % drawn-nums) boards))

(defn get-winner [drawn-nums, boards]
  (first (get-winners drawn-nums boards)))

(defn play-bingo [nums, boards]
  (loop [num-ind 0]
    (if (nil? (get-winner (take num-ind nums) boards))
      (recur (inc num-ind))
      {:index num-ind :board (get-winner (take num-ind nums) boards)})))

(defn diff-sequences [seq-a, seq-b]
  (filter #(not (some (fn [seq] (= % seq)) seq-b)) seq-a))

(defn play-bingo-pt-two [nums, boards]
  (loop [num-ind (count nums)]
    (if (= (get-winners (take (- num-ind 1) nums) boards) (get-winners (take num-ind nums) boards))
      (recur (dec num-ind))
      {:index num-ind :board (diff-sequences (get-winners (take num-ind nums) boards) (get-winners (take (- num-ind 1) nums) boards))})))

(defn get-final-result [results, drawn-nums]
  (let [final-num-ind (get results :index)
        final-num (nth drawn-nums (- final-num-ind 1))
        unmarked-nums (map (fn [num] (Integer/parseInt num)) (filter #(not (some #{%} (take final-num-ind drawn-nums))) (flatten (get results :board))))]
    (println "Results: " results)
    (println "Final Number: " final-num)
    (println "Unmarked: " unmarked-nums)
    (println "Unmarked Sum: " (reduce + unmarked-nums))
    (println "Answer: " (* (Integer/parseInt final-num) (reduce + unmarked-nums)))))

(defn main
  []
  (with-open [rdr (io/reader (io/resource "input.txt"))]
    (let [all-lines (filter #(not (string/blank? %)) (line-seq rdr))
          drawn-nums (string/split (first all-lines) #",")
          boards (read-boards all-lines)
          results (play-bingo drawn-nums boards)
          final-num-ind (get results :index)
          final-num (nth drawn-nums (- final-num-ind 1))
          unmarked-nums (map (fn [num] (Integer/parseInt num)) (filter #(not (some #{%} (take final-num-ind drawn-nums))) (flatten (get results :board))))
          results-pt-two (play-bingo-pt-two drawn-nums boards)]
      (println "Drawn Num Count: " (count drawn-nums))
      (println "PART ONE")
      (get-final-result results drawn-nums)
      (println "PART TWO")
      (println "Results: " results-pt-two)
      (get-final-result results-pt-two drawn-nums))))
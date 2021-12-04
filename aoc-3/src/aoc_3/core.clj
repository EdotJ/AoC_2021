(ns aoc-3.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn get-common-bit [list, index, comp]
  (first (first (sort-by val comp (frequencies (map #(subs % index (+ index 1)) list))))))

(defn get-gamma-vector [readings]
  (loop [i 0 vector (transient [])]
    (if (< i (count (first readings)))
      (recur (inc i) (conj! vector (get-common-bit readings i >)))
      (persistent! vector))))

(defn get-gamma [lines]
  (string/join (get-gamma-vector lines)))

(defn get-epsilon [gamma]
  (string/join (map #(case %
                       "1" 0
                       "0" 1) (string/split gamma #""))))

(defn solve-first [gamma]
  (* (Integer/parseInt (get-epsilon gamma) 2) (Integer/parseInt gamma 2)))

(defn get-common-bit-pt-two [list, index, comp, minmax]
  (let [values (sort-by val comp (frequencies (map #(subs % index (+ index 1)) list)))]
   (if (apply = (map #(val %) values))
     (first (apply minmax #(read-string (key %)) values))
     (first (first values)))))

(defn get-life-support-rating [readings, comp-sign, minmax]
  (
    loop [i 0
         filtered-readings readings]
    (if (> (count filtered-readings) 1)
      (recur (inc i) (filter #(= (get-common-bit-pt-two filtered-readings i comp-sign minmax) (get (string/split % #"") i)) filtered-readings))
      (first filtered-readings)
      )))

(defn main
  []
  (let [file-contents (slurp (io/resource "input.txt"))
        readings (string/split file-contents #"\n")
        gamma (get-gamma readings)
        oxygen (get-life-support-rating readings > max-key)
        co2 (get-life-support-rating readings < min-key)]
    (println "Answer pt. 1:" (solve-first gamma))
    (println "Answer pt. 2:" (* (Integer/parseInt oxygen 2) (Integer/parseInt co2 2)))
    ))
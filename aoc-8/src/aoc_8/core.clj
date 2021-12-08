(ns aoc-8.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn get-part1 [input]
  (map #(map (fn [str] (count str)) (string/split (second (string/split % #" \| ")) #" ")) input))

(defn get-encoded-digits [input]
  (map #(string/split % #" \| ") input))

(defn get-length-for-unique-digits [digits, cnt]
  (first (filter #(= (count %) cnt) digits)))

(defn get-occurrence-count [digit-codes, letter]
  (count (filter (fn [str] (string/includes? str letter)) digit-codes)))

(defn occurs-arg-times? [codes, string, count]
  (= (get-occurrence-count codes string) count))

(defn occurring-excluded? [digit-codes, string, count, not-in-digit]
  (and (occurs-arg-times? digit-codes string count) (not (string/includes? not-in-digit string))))

(defn occurring-included? [digit-codes, string, count, in-digit]
  (and (occurs-arg-times? digit-codes string count) (string/includes? in-digit string)))

(defn contains-all? [codes, digit]
  (every? #(string/includes? digit %) codes))

(defn maps-to-digit? [codes, code]
  (and (contains-all? codes code) (= (count code) (count codes))))

(defn decode-digit [digit, symbols]
  (cond
    (maps-to-digit? [(symbols :top) (symbols :top-right) (symbols :bottom-right)
                       (symbols :bottom) (symbols :bottom-left) (symbols :top-left)] digit) "0"
    (= (count digit) 2) "1"
    (maps-to-digit? [(symbols :top) (symbols :top-right) (symbols :mid)
                       (symbols :bottom-left) (symbols :bottom)] digit) "2"
    (maps-to-digit? [(symbols :top) (symbols :top-right) (symbols :mid)
                       (symbols :bottom-right) (symbols :bottom)] digit) "3"
    (= (count digit) 4) "4"
    (maps-to-digit? [(symbols :top) (symbols :top-left) (symbols :mid)
                       (symbols :bottom-right) (symbols :bottom)] digit) "5"
    (maps-to-digit? [(symbols :top) (symbols :top-left) (symbols :mid)
                       (symbols :bottom-right) (symbols :bottom) (symbols :bottom-left)] digit) "6"
    (= (count digit) 3) "7"
    (= (count digit) 7) "8"
    (maps-to-digit? [(symbols :top) (symbols :top-right) (symbols :top-left)
                       (symbols :bottom-right) (symbols :bottom) (symbols :mid)] digit) "9"
    ))

(defn decode-line [digits, to-decode]
  (let [digit-vec (string/split digits #" ")
        abc ["a" "b" "c" "d" "e" "f" "g"]
        one (get-length-for-unique-digits digit-vec 2)
        four (get-length-for-unique-digits digit-vec 4)
        top-right (first (filter #(= (get-occurrence-count digit-vec %) 8) (string/split one #"")))
        bottom-right (string/replace one top-right "")
        top (first (filter #(occurring-excluded? digit-vec % 8 one) abc))
        top-left (first (filter #(occurring-excluded? digit-vec % 6 one) abc))
        bottom-left (first (filter #(occurring-excluded? digit-vec % 4 one) abc))
        bottom (first (filter #(and (occurring-excluded? digit-vec % 7 one) (occurring-excluded? digit-vec % 7 four)) abc))
        mid (first (filter #(and (occurring-excluded? digit-vec % 7 one) (occurring-included? digit-vec % 7 four)) abc))
        symbol-map {:top-right   top-right :top top :top-left top-left :mid mid :bottom-right bottom-right
                    :bottom-left bottom-left :bottom bottom}]
    (string/join (map #(decode-digit % symbol-map) (string/split to-decode #" ")))))

(defn main []
  (let [input (string/split (slurp (io/resource "input.txt")) #"\n")
        digit-codes (get-encoded-digits input)]
    (println "PART ONE:" (count (filter #(some (fn [char-count] (= char-count %)) [2 3 4 7]) (flatten (get-part1 input)))))
    (println (second (first digit-codes)))
    (println (Integer/parseInt (decode-line (first (first digit-codes)) (second (first digit-codes)))))
    (println "PART TWO:" (apply + (map #(->> (decode-line (first %) (second %))
                                             Integer/parseInt) digit-codes)))
    ))

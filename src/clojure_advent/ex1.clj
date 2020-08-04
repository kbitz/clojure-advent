(ns clojure-advent.ex1
  (:gen-class)
  (:require [clojure.string :as str]))

(def input (map read-string (str/split-lines (slurp "resources/ex1-input.txt"))))

(defn fuel 
  [x]
  (let [y (- (quot x 3) 2)]
    (if (> y 0)
      y
      0)))

(defn fuel-fuel
  [x]
  (loop [x x total 0]
    (if (= 0 x)
      total
      (recur 
       (fuel x)
       (+ total (fuel x))))))

(defn part-1
  [input]
  (reduce + (map fuel input)))

(defn part-2
  [input]
  (reduce + (map fuel-fuel input)))
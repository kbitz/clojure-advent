(ns clojure-advent.ex2
  (:gen-class)
  (:require [clojure.string :as str]))

(def input (vec (map #(Integer/parseInt %) (str/split (slurp "resources/ex2-input.txt") #","))))

(def opcode
  {1 +
   2 *})

(defn memory [in & [vals]]
  (loop [vals (seq vals) in in]
    (if (empty? vals)
      in
      (recur
       (rest vals)
       (assoc in ((first vals) 0) ((first vals) 1))))))

(defn computer [in & [vals]]
  (let [program (if (empty? vals) 
                  in 
                  (memory in vals))]
    (loop [iter 0 result program]
      (let [remain (subvec result iter)]
       (if (= 99 (remain 0))
        result
        (recur
           (+ iter 4)
           (assoc result (remain 3) ((opcode (remain 0)) (result (remain 1)) (result (remain 2))))))))))

(defn intcode [in result & [vals]]
  ((computer in vals) result))

(defn part-1
  []
  (intcode input 0 {1 12 2 2}))

(defn part-2
  []
  (for [n (range 0 99) v (range 0 99) :when (= 19690720 (intcode input 0 {1 n 2 v}))]
    (+ (* n 100) v)))

(ns clojure-advent.ex3
  (:gen-class)
  (:require [clojure.string :as str])
  (:require [clojure.set]))

(def input (str/split-lines (slurp "resources/ex3-input.txt")))

(defn vectorize [inst]
  (let [inst (vec (map str (vec inst)))]
    [(inst 0) (Integer/parseInt (str/join (subvec inst 1)))]))

(def wire1 (map vectorize (str/split (input 0) #",")))
(def wire2 (map vectorize (str/split (input 1) #",")))

; gets absolute val of x
(defn abs [x] (max x (-' x)))

(defn new-coords [dir dist x y]
  (cond
    (= dir "L") [(- x dist) y]
    (= dir "R") [(+ x dist) y]
    (= dir "U") [x (+ y dist)]
    (= dir "D") [x (- y dist)]))

(defn next-coord [c1 c2]
  (let [x0 (c1 0) y0 (c1 1) x1 (c2 0) y1 (c2 1)]
    (cond
      (< x0 x1) [(+ x0 1) y0]
      (< x1 x0) [(- x0 1) y0]
      (< y0 y1) [x0 (+ y0 1)]
      (< y1 y0) [x0 (- y0 1)])))

(defn fill-coords [c1 c2]
  (loop [coords [] last c1]
    (if (= last c2)
      coords
      (recur
       (conj coords (next-coord last c2))
       (next-coord last c2)))))

(defn wiremap [wire]
  (loop [remain wire coords (empty [[]]) prev [0 0]]
    (if (empty? remain)
      coords
      (let [inst (first remain) new-coord (new-coords (inst 0) (inst 1) (prev 0) (prev 1))]
        (recur
         (rest remain)
         (concat coords (fill-coords prev new-coord))
         new-coord)))))

(defn cross [wiremap1 wiremap2]
  (clojure.set/intersection (set wiremap1) (set wiremap2)))

(defn part-1 []
  (let [c (cross(wiremap wire1) (wiremap wire2))]
    (apply min (into (empty c) (for [[k v] c] (+ (abs k) (abs v)))))))

;; "Elapsed time: 4500.751137 msecs"
(defn part-2 []
  (let [wm1 (wiremap wire1) wm2 (wiremap wire2) cross (cross wm1 wm2)]
    (loop [remain cross result (empty cross)]
      (if (empty? remain)
        (apply min result)
        (recur
         (rest remain)
         (conj result (+
                       (reduce + (for [p wm1 :while (false? (= (first remain) p))] 1))
                       (reduce + (for [p wm2 :while (false? (= (first remain) p))] 1)) 2)))))))

;; Part 2 - Second Attempt
;; "Elapsed time: 3874.786926 msecs"
(defn count-steps [c wm]
  (+ 1 (reduce + (for [p wm :while (false? (= c p))] 1))))

(defn part-2-2 []
  (let [wm1 (wiremap wire1) wm2 (wiremap wire2) cross (cross wm1 wm2)]
    (apply min (map + 
                    (map #(count-steps % wm1) cross)
                    (map #(count-steps % wm2) cross)))))

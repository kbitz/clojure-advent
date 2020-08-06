(ns clojure-advent.ex4
  (:gen-class))

(def input (range 128392 (inc 643281)))

(defn has-six-digits? [n]
  (= 6 (count (str n))))

(defn has-adjacent-pair? [n]
  (boolean (seq (filter (fn [[a b]] (= a b)) (partition 2 1 (str n))))))

(defn not-decreasing? [n]
  (apply <= (map #(Integer/parseInt %) (vec (map str (vec (str n)))))))

(defn has-iso-adjacent-pair? [n]
  (if (false? (has-adjacent-pair? n))
    false
    (let [pairs (filter (fn [[a b]] (= a b)) (partition 2 1 (str n)))]
      (boolean (seq (filter #(= 1 %) (for [p pairs] (count (filter #(= p %) pairs)))))))))

(defn part-1 []
  (count (filter (every-pred
                  has-six-digits?
                  has-adjacent-pair?
                  not-decreasing?) input)))

(defn part-2 []
  (count (filter (every-pred
                  has-six-digits?
                  has-iso-adjacent-pair?
                  not-decreasing?) input)))
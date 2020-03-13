(ns kidoku.core
  (:require [clojure.math.combinatorics :as combo]))

(defn all-rows [n]
  "Returns all rows of 1 through n (inclusive)"
  (combo/permutations (range 1 (inc n))))

(defn permitted-next-row [n grid candidate-row]
  "Return whether the given candidate row could conceivably come next"
  (if (empty? grid)
    true
    (let [bans (for [col (range n)] (set (map #(nth % col) grid)))
          contains (for [col (range n)] ((nth bans col) (nth candidate-row col)))]
      (= #{nil} (set contains)))))

(defn all-latin-squares
  "Return a lazy sequence of all possible n by n latin squares."
  ([n] (all-latin-squares n []))
  ([n grid] ; grid is what we've built so far
   (if (= n (count grid))
     (list grid) ; base case
     (let [permitted-next-rows (filter #(permitted-next-row n grid %) (all-rows n))
           grids (map #(conj grid %) permitted-next-rows)
           ;dummy (println (str "n=" n
           ;                    "\ngrid=" grid
           ;                    "\ngrids=" (into [] grids)
           ;                    "\nmapped=" (into [] (map #(all-latin-squares n %) grids))
           ;                                      ))
           ]
       (apply concat (map #(all-latin-squares n %) grids))))))

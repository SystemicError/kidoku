(ns kidoku.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(defn all-rows [n]
  "Returns all rows of 1 through n (inclusive)"
  (combo/permutations (range 1 (inc n))))

(defn permitted-next-row [n cell-w cell-h grid candidate-row]
  "Return whether the given candidate row could conceivably come next"
  (if (empty? grid)
    true
    (let [column-bans (for [col (range n)] (set (map #(nth % col) grid)))
          column-contains (for [col (range n)] ((nth column-bans col) (nth candidate-row col)))
          valid-columns (= #{nil} (set column-contains))
          ; now check the current cell
          row (count grid)
          row-in-cell (mod row cell-h)
          recent-rows (take-last row-in-cell grid)
          recent-col-bans (for [col (range n)] (set (map #(nth % col) recent-rows)))
          cell-bans (for [cell-col (range (/ n cell-w))]
                      (apply set/union (take cell-w (drop (* cell-w cell-col) recent-col-bans))))
          cell-contains (for [col (range n)] ((nth cell-bans (/ col cell-w)) (nth candidate-row col)))
          valid-cells (= #{nil} (set cell-contains))
          ;dummy (println (str "cell-bans" (into [] cell-bans)))
          ]
      (and valid-columns valid-cells))))

(defn all-kidoku
  "Return a lazy sequence of all possible n by n latin squares where no w by h cell contains repeats."
  ([n w h] (all-kidoku n w h []))
  ([n w h grid] ; grid is what we've built so far
   (if (= n (count grid))
     (list grid) ; base case
     (let [permitted-next-rows (filter #(permitted-next-row n w h grid %) (all-rows n))
           grids (map #(conj grid %) permitted-next-rows)
           dummy (println (str "n=" n
                               "\ngrid=" grid
                               "\ngrids=" (into [] grids)
                                                 ))
           ]
       (apply concat (map #(all-kidoku n w h %) grids))))))

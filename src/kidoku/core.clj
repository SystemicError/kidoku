(ns kidoku.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(defn all-rows [n]
  "Returns all rows of 1 through n (inclusive)"
  (combo/permutations (range 1 (inc n))))

(defn permitted-next-row [n cell-w cell-h clues grid candidate-row]
  "Return whether the given candidate row could conceivably come next"
  (let [row (count grid)
        ; does it match the given clues?
          ; clues a grid with nil in non-set entries
        matches-clues (reduce = true
                              (for [col (range n)]
                                (or (= (nth (nth clues row) col) nil)
                                    (= (nth (nth clues row) col)
                                       (nth candidate-row col)))))]
    (if (empty? grid)
      matches-clues
      (let [; does it match the columns thus far?
            column-bans (for [col (range n)] (set (map #(nth % col) grid)))
            column-contains (for [col (range n)] ((nth column-bans col) (nth candidate-row col)))
            valid-columns (= #{nil} (set column-contains))
            ; now check the current cell
            row-in-cell (mod row cell-h)
            recent-rows (take-last row-in-cell grid)
            recent-col-bans (for [col (range n)] (set (map #(nth % col) recent-rows)))
            cell-bans (for [cell-col (range (/ n cell-w))]
                        (apply set/union (take cell-w (drop (* cell-w cell-col) recent-col-bans))))
            cell-contains (for [col (range n)] ((nth cell-bans (/ col cell-w)) (nth candidate-row col)))
            valid-cells (= #{nil} (set cell-contains))
            ;dummy (println (str "cell-bans" (into [] cell-bans)))
            ]
        (and matches-clues valid-columns valid-cells)))))

(defn all-kidoku
  "Return a lazy sequence of all possible n by n latin squares where no w by h cell contains repeats."
  ([n w h clues] (all-kidoku n w h clues []))
  ([n w h clues grid] ; grid is what we've built so far
   (if (= n (count grid))
     (list grid) ; base case
     (let [permitted-next-rows (filter #(permitted-next-row n w h clues grid %) (all-rows n))
           grids (map #(conj grid %) permitted-next-rows)
           ;dummy (println (str "n=" n
           ;                    "\ngrid=" grid
           ;                    "\ngrids=" (into [] grids)
           ;                                      ))
           ]
       (apply concat (map #(all-kidoku n w h clues %) grids))))))


(defn blank-grid [n]
  "Returns a list of rows, each a list of cells, each a list of possible numbers for this cell."
  (for [row (range n)]
    (for [col (range n)]
      (set (map inc (range n))))))

(defn pigeonhole-set? [cells]
  "Determines if the given list of cells has exactly as many candidates as there are cells.  If so, return those candidates.  If not, return nil."
  (let [n (count cells)
        letters (reduce set/union cells)
        m (count letters)]
    (if (= n m)
      letters
      nil)))

(defn apply-pigeonhole [ph cells]
  "Removes from any cell containing letters outside ph any letters inside ph."
  ; For each pigeonhole subset
    ; (1 2) (1 2) (1 2 3) (3 4) -> {1 2} {1 2 3}
  ; remove the pigeonhole subset from any cell that contains things outside that subset
    ; (1 2) (1 2) (3) (4)
    (let [cleave (fn [cell]
                   (if (= ph (set/union cell ph))
                     cell
                     (set/difference cell ph)))
          ]
      (map cleave cells)
  ))

(defn remove-duplicates [row]
  "For a list representing a row/col/cell, enforce the rule banning duplicates by returning a version with fewer candidates in each cell."
  ; if any subset S has exactly (count S) candidates in its union, remove those candidates from all other cells
  (let [n (count row)
        letters (range n)
        index-subsets (apply concat (map #(combo/combinations letters %) (range 1 n))) ; a list of all combinations of indices
        indices-to-cells (fn [indices] (map #(nth row %) indices))
        subsets (map indices-to-cells index-subsets)
        pigeonhole-subsets (filter #(not= nil %) (map pigeonhole-set? subsets))
        ph-fns (map #(partial apply-pigeonhole %) pigeonhole-subsets)

        dummy (println (str "(indices-to-cells [0 1])=" (into [] (indices-to-cells (list 0 1)))
                            "\nsubsets=" (into [] subsets)
                            "\npigeonhole-subsets = " (into [] pigeonhole-subsets)
                            ))
        ]
    ((apply comp ph-fns) row)
    ))

(defn solved? [grid]
  "Returns true iff every cell is a set of order 1."
  (let [cells (apply concat grid)
        orders (map count cells)]
    (apply = (conj orders 1)))
  )

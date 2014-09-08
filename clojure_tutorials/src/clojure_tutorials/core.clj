(ns clojure-tutorials.core)


;; Mostly koans from 4clojure, trying to get better solutions if possible


;;Find the maximum without using max. Most solutions use a composition on sort
;; and last, but this would be O(nlogn), this is O(n)
(defn maximum
  "Returns the maximum value from the arguments, for the moment doesn't work with negative values"
  [& args]
  (loop 
    [x args max-val 0]
    (if (empty? x)
      max-val
      (recur (rest x) (if (> max-val (first x)) max-val (first x))))))

;;Filter out all the non-capital letters from a string
(defn filter-caps
  "Removes all non capital letters from a string"
  [x]
  (clojure.string/replace x #"[^A-Z]" ""))

;; Must reverse the reverse for the right order
(defn duplicate 
  "Duplicates the elements of a sequence"
  [x]
  (loop [y x new_seq '()]
    (if (empty? y)
      new_seq
      (recur (rest y) (conj new_seq (first y) (first y))))))

;; This seems like the best solution, using interleave, which
;; creates a sequence by taking the 1st elements of the 2 collections, then
;; the second, etc..
;; #(interleave % %)
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

(defn my-range
  "MY implementation of range"
  [min-val max-val]
  (loop [x min-val result '()]
    (if (>= x max-val)
      (reverse result)
      (recur (inc x) (conj result x)))))

;; I like this solution the best, I think we are looking for take-while here
;; #(take-while (fn [x] (< x %2)) (iterate inc %1))
;; here, the x represents the values in the list created by iterate

(defn factorial
  "[int] Calculates the factorial of a value."
  [x]
  (if (= x 1)
    1
    (* (factorial (dec x)) x)))

;; An interesting solution, using range
;;#(reduce * (range 1 (inc %)))

;;Interleave two sequences without using interleave
;;This wouldn't work for collections of lists, for example, because of
;; the use of flatten
(defn my-interleave
  "[coll1 coll2] Interleaves the values of both collections.
   We only include as many elements in the interleave as there are
   in the shortest collection."
  [coll1 coll2]
  (flatten (take (min (count coll1) (count coll2)) (map list coll1 coll2))))

;;Here is a solution without flatten, but it uses loop again
(defn my-interleave2 
  "[coll coll] interleaves elements from the two collections"
  [coll1 coll2]
  (loop [x coll1 y coll2 result ,()]
    (if (or (empty? x) (empty? y))
      (reverse result)
      (recur (rest x) (rest y) (conj result (first x) (first y))))))

;; Simplest solution here seems to be 
;;mapcat list, mapcat creating a concat of the application of the function
;; to the arguments


;;Compress a sequence: remove all elements that are the same as the one before
(defn compress
  "[coll] Removes elements that are the same, one after the other"
  [coll]
  (reduce #(if (= (last %1) %2) %1 (concat %1 (list %2))) '() (seq coll)))

;;I couldn't get a loop version to work. Also, I can't use when in reduce, because
;;I always have to return a value in the fn given to reduce.
;;This seems like the most popular solution:
;;#(map first (partition-by identity %))
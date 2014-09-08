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

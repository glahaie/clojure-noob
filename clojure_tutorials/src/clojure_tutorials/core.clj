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

;;Replicate a sequence a number of times
(defn my-replicate
  "[coll] int Replicate a sequence a given number of times"
  [coll nb]
  (mapcat #(take nb (iterate identity %)) coll))

;;My solution is close to the most interesting one. I was looking for the repeat
;;function, instead of having to use take and iterate
;; that result is: #(mapcat (partial repeat %2) %1) 

(defn my-pack
  "Packs a sequence into subsequences of consecutive same items"
  [coll]
  (partition-by identity coll))
;; this seems like the most popular solution

(defn my-interpose
  "[seq val] Interposes a sequence by a given value"
  [value coll]
  (drop-last
    (mapcat #(conj [] % value) coll)))

;; my original version used reduce instead, i think the problem was mostly
;; that I was not dropping the last element, so I was failing the unit tests.

(defn my-interpose2
  "[seq val] Interposes seq with value val"
  [value coll]
  (vec (drop-last (interleave coll (repeat value)))))

;; Another version, I like this one, makes use of infinite lazy lists

;;Drop every nth item
(defn ndrop
  "[coll n] Drops every nth element of the given collection"
  [coll n]
  (if (empty? coll) [] (concat (take (- n 1) coll) (ndrop (drop  n coll) n)))) 

;;Split a sequence without using split-at
(defn mysplit
  "[n coll] Split a sequence into two parts"
  [n coll]
  [(take n coll ) (drop n coll)])
;;seems close to most solutions
;; this is interesting, but my version of clojure says this juxt is still in
;; alpha: (juxt take drop)

;;Half-truth
(defn half-truth
  "[& bools] Returns true if at least one element is true, but not all of them"
  [& bools]
  (= (not (every? #(= true %) bools)) (not (every? #(= false %) bools))))

;;Be careful with and/or functions, they are not logical operators, they return
;;the 1st value in the collection that is true or false, depending
;; the simplest form here would be not=

;;Map construction
(defn mymap 
  "[k v] creates a map from the vectors of keys and values"
  [k v]
  (reduce merge {} (map hash-map k v)))

;; My solution works, but I was looking for the assoc function here to add
;; a new k/v pair to a map

;;Greatest common divisor
;; I am using Euclid's algorithm, it seems pretty good for clojure
(defn gcd
  "[a b] returns the greatest common divisor of a and b"
  [a b]
  (if (= b 0) a (gcd b (rem a b))))

;; Problem 166 - define your own comparison function
(defn my-compare
  "[p x y] returns a keyword that says if x < y, p being the comparator used to
   compare them"
  [p x y]
  (if (p x y)
    :lt
    (if (p y x)
      :gt
      :eq)))

;; it seems like the best solution here is to use the macro cond:
;; #(cond (% %2 %3) :lt (% %3 %2) :gt :else :eq), but the logic is the same

;; Problem 81: intersection on sets
;; Let's try to reduce
(defn my-intersection
  "[s1 s2] Returns the intersection of both sets"
  [s1 s2]
  (reduce #(if (contains? s2 %2) (conj % %2) %) #{} s1))
;;This works, but the use of filter seems like the best solution
;;(comp set filter)
;; or #(set (filter %2 %))

;;Problem #62
;;A possible solution form clojure.org/lazy
(defn my-iterate
  "[f x] Returns a lazy sequence containing x, f(x), f(f(x)), etc..."
  [f x]
  (lazy-seq
    (cons x (my-iterate f (f x)))))

;; This took a while, my original idea was close, but I was trying to use
;; cons in a wrong way, and this seems like the only solution coming up
;; in other solutions. I was also trying to have 

;; Problem 107 - simple closure
(defn my-pow 
  "[x] Returns a function that calculates y^x"
  [x]
  (fn 
    [y]
    (reduce * (repeat x y))))

;;I could use MATH/pow here too

;;Problem 99: Product digits
(defn product-digits
  "[x y] Returns the result of x*y in a sequence containing the digits"
  [x y]
  (map #(java.lang.Character/getNumericValue %) (vec (str (* x y)))))
;; The use of a string here seems pretty much to be everyone's solution.

;;Problem 90: Cartesian product
(defn cartesian-product
  "[s1 s2] returns the cartesian product of both sets"
  [s1 s2]
  (set (mapcat (fn [x] (map #(vec ((partial list x) %)) s2)) s1)))

;; Works, but it seems like the for was the best idea here:
;; #(set (for [x % y %2] [x y]))

;;Group a sequence, without using group-by, problem 63
(defn my-group-by
  "[f s] Groups the elements of s in a map by the result of f"
  [f s]
  (reduce 
    (fn 
      [x y]
      (let [k (f y) v (get x k)] 
        (assoc x k (if (nil? v) [y] (into v [y]))))) {} s)) 

;;Problem 122
(defn stuff
  "[x] takes a string representing a binary number and gives the decimal value"
  [x]
  (let [y (reverse (map #(java.lang.Character/getNumericValue %) x))]
    (first (reduce #(list (+ (* %2 (second %)) (first %)) (* (second %) 2)) '(0 1) y))))

;;Bleh, it seems the point here is to use java methods in clojure. Two options:
;; either you use read-string, this way:
;; #(read-string (str 2 \r %))
;; or you use java's parseInt:
;; #(Integer/parseInt % 2)

;; Problem 88: Symmetric difference
(defn sym-dif
  "[s1 s2] Returns the symmetric difference of sets 1 and 2"
  [s1 s2]
  (clojure.set/difference
    (clojure.set/union s1 s2)
    (clojure.set/intersection s1 s2)))

;; Most solutions are similar to mine, this one use disj(disjoint) with apply,
;; I should try to use apply sometimes :)
;;#((comp set concat)  (apply disj %1 %2) (apply disj %2 %1))


;;Problem 143: dot product
(defn dot
  "[v1 v2] Returns the dot product of the vectors, we assume the vectors have
   the same length"
  [v1 v2]
  (reduce #(+ % (apply * %2)) 0 (map vector v1 v2)))

;; Problem 135: Infix calculator
(defn infix
  "[x f y & args] Defining an infix calculator for clojure"
  [x f y & args]
  (let [a (f x y)]
    (if (empty? args)
      a
      (recur a (first args) (second args) (drop 2 args)))))

;; A solution with reduce, quite clever
;; (fn [& args] 
;;(reduce #((first %2) %1 (second %2)) (first args) (partition 2 (rest args)) ))
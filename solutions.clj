;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                  ;;
;; Getting back into clojure after a break, need a little refresher ;;
;;                                                                  ;;
;; Author : George Petrov                                           ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Greatest common divisor

(fn [a b]
  (first
   (filter
    #(and (zero? (mod a %)) (zero? (mod b %)))
    (range (min a b) 0 -1))) )


;; Compress a sequence

#(map first (partition-by identity %))


;; Flatten a sequence

(fn myflatten [c]
    (letfn [(_myflatten [c flat]
                        (if (seq c)
                          (let [f (first c)]
                            (cond (list? f) (recur (concat f (rest c)) flat)
                                  (vector? f) (recur (concat f (rest c)) flat)
                                  :else (recur (rest c) (conj flat (first c)))))
                          flat))]
      (apply list (_myflatten c []))))


;; Get the caps

#(apply str (re-seq #"[A-Z]" %))


;; Fibonacci sequence

(fn fib [n] (last (take (dec n)
                        (iterate (fn next-fib [fibs]
                                   (conj fibs (+ (last fibs) (last (butlast fibs)))))
                                 [1 1]))))


;; Palindrome detector

(fn pal [s]
   (= (seq s) (reverse s)))


;; Reverse a sequence

(fn [s]
    (letfn [(myreverse [s r]
                     (if (seq s)
                       (recur (rest s) (conj r (first s)))
                       r))]
      (myreverse s '())))


;; Find the odd numbers

filter odd?


;;Sum it all up

(partial apply +)



;; Count a sequence

(fn [s]
    (letfn [(mycount [s c]
                     (if (seq s)
                       (recur (rest s) (+ c 1))
                       c))]
      (mycount s 0)))



;; Nth element

(fn [s n] (->> s (drop n) first))



;; Penultimate element

#(-> % reverse second)


;; Last element

#(-> % reverse first)



;; Pascal's triangle

;; Note, the solution is the function, not the whole form
((fn pascal [n]
    (letfn [(next-pascal [this-pascal]
                         (map (fn [[a b]] (+ a b))
                              (partition 2 1
                                         (concat [0] this-pascal [0]))))]
      (apply vector (last (take n (iterate next-pascal [1]))))))
 1)


;; Equivalence classes

((fn [f d]
   (set
    (map #(set (map :x %))
         (partition-by :y (sort-by :y (map #(hash-map :x % :y (f %)) d))))))
 #(* % %) #{-2 -1 0 1 2})



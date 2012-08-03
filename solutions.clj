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



;; Product digits

((fn [a b]
   (map #(Integer/parseInt %) (map str (str (* a b)))))
 999 99)


((fn [a b]
   (letfn [(digits [n s]
                   (if (= n 0) s
                       (recur (int (/ n 10)) (conj s (rem n 10)))))]
     (digits (* a b) '())))
 999 99)



;; Least common multiple

;; This one fails the test for some fraction. Need a better one that
;; doesn't assume integers
((fn [& xs]
   (let [filt (apply every-pred
                     (map #(fn [x] (zero? (mod x %))) xs))]
     (first (filter filt (iterate #(+ % (min (apply min xs) 1)) 1)))))
 3/4 1/6)

;; This is better
((fn [& c]
   (let [multiples (into {} (map #(vector % (range % 10000 %)) c))
         min-multiple (multiples (apply min c))
         not-in-range-fn (fn [range] (fn [x] (not (contains? (set range) x))))
         contains-preds (map not-in-range-fn (vals multiples))]
     (first (drop-while (apply some-fn contains-preds)
                        min-multiple))))
 3/4 1/6)




;; Happy numbers

((fn happy-numbers [n]
   (letfn [(digits
            ([n s] ;;Copied from above, recursive, init with s
               (if (= n 0) s
                   (recur (int (/ n 10)) (conj s (rem n 10)))))
            ([n]
               (digits n '())))
           (next [n]
                 (reduce + (map #(* % %) (digits n))))]
     (= 1 (first (drop-while #(not (= 1 %)) (take 100000 (iterate next n)))))
     ))
 7)



;; Flipping out

(((fn reverse-args [fun]
     (fn [& args] (apply fun (reverse args))))
  nth) 2 [1 2 3 4 5])



;; Interpose a seq

((fn [v col]
   (drop 1 (mapcat (partial vector v) col)))
 0 [1 2 3])



;; Drop every nth item

((fn [col n]
   (take-while #(not (nil? %))
               (mapcat butlast (partition n n (repeat nil) col))))
 ;;[1 2 3 4 5 6 7 8] 3
 [1 2 3 4 5 6] 4
 )



;; Replicate a sequence

((fn [c n]
   (apply (partial mapcat vector) (repeat n c)))
 [1 2 3] 3)

;; Duplicating

((fn [c]
   (mapcat vector c c))
 [1 2 3])



;; Implement range

((fn [b e]
    (take (- e b) (iterate inc b)))
 1 4)



;; Pack a sequence

((fn [c]
    (partition-by identity c))
 [1 1 2 1 1 1 3 3])



;; Maximum value

((fn mymax [& xs]
   (letfn [(_mymax [c m]
                    (if (seq c)
                      (recur (rest c) (if (> (first c) m) (first c) m))
                      m))]
     (_mymax xs (first xs))))
 1 3 55 3)



;; Interleave two seqs

((fn [s1 s2]
   (mapcat vector s1 s2))
 [1 2 3] [:a :b :c])



;; Factorial

((fn fact [n]
   (letfn [(_fact [f i]
                  (if (<= i 0)
                    f
                    (do (println "next is " (dec i) ", " (* n i)) (recur (* f i) (dec i) ))))]
     (_fact 1 n)))
 8)



;; Reverse interleave

;; Study/draft 1
((fn [c]
   (let [p (partition 2 c)]
     (list (map first p) (map second p))))
 [1 :a 2 :b 3 :c])

;; Study/draft 2
((fn [c]
   (let [p (partition-all 3 c)]
     (list (map first p) (map second p) (map #(nth % 2) p))))
 [1 :a 2 :b 3 :c])

;; Study/draft 3
((fn [c]
   (let [p (partition-all 4 c)]
     (list (map #(nth % 0) p) (map #(nth % 1) p) (map #(nth % 2) p) (map #(nth % 3) p))))
 [1 10 :a :Z 2 20 :b :Y 3 30 :c :X])

;; Solution
((fn [c n]
   (let [p (partition-all n c)
         slicer-fns (for [pos (range 0 n)] (fn [l] (nth l pos)))]
     (map #(map % p) slicer-fns)))
 [1 10 :a :Z 2 20 :b :Y 3 30 :c :X] 4)




;; Rotate sequence

;; Study/draft
(time
 ( (fn rotate [n c]
     (take (count c) (drop n (concat c c))))
   2 [1 2 3 4 5]))

;; Study/draft
((fn rotate [n c]
   (let [reordered (if (< n 0) (reverse c) c)
         r (take (count c) (drop (if (< n 0) (- n) n) (apply concat (repeat 5 reordered))))]
     (if (> n 0)
       r
       (reverse r))))
 ;;6 [1 2 3 4 5]
 ;;1 '(:a :b :c)
 -4 '(:a :b :c)
 )


;; Split a sequence

((fn [n c]
    [(take n c) (drop n c)])
 3 [1 2 3 4 5 6]
 ;;1 [:a :b :c :d]
 ;;2 [[1 2] [3 4] [5 6]]
 )

;; Recursive, just for practice
((fn mysplit [n c]
   (letfn [(_mysplit [a b]
                     (let [va (vec a)
                           lb (apply list b)]
                       (if (= n (count va))
                         [va lb]
                         (recur (butlast va) (conj lb (last va))))))]
     (_mysplit c [])))
 3 [1 2 3 4 5 6])




;; Through the looking class

(let [x java.lang.Class]
  (and (= (class x) x) x))





;; Read a binary number

(time
 ((fn [s]
    (let [digits (map {\1 1 \0 0} (reverse s))
          powers-of-2 (map #(long (Math/pow 2 %)) (range 0 32))]
      (reduce + (map * digits powers-of-2))))
  "10101010101"))





;; Re-implement iterate

(letfn [(myiterate [f v]
                   (let [fv (f v)]
                     (lazy-seq (cons v (myiterate f (f v))))))]
  (take 20 (drop 100 (myiterate #(+ % 3) 3))))


(take 5 ( (fn myiterate [f v]
            (let [fv (f v)]
              (lazy-seq (cons v (myiterate f (f v))))))
          #(+ 3 %) 1))





;; Word sorting

((fn [text]
   (map :o (sort-by :u (map #(hash-map :o % :u (.toUpperCase %)) (re-seq #"[a-zA-Z]+" text)))))
 "Have a nice day.")





;; Merge with a function

;; Study/draft
((fn [& maps]
   (reduce (fn [a b] (apply (partial assoc a) (flatten (into [] b)))) maps))
 {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})

;; Study/draft
((fn [fn & maps]
   (reduce
    (fn [a b]
      (let [new-vals
            (concat
             (for [[b-key b-val] b]
               [b-key
                (if (a b-key)
                  (f (a b-key) (b b-key))
                  b-val)]))]
        (apply (partial assoc a) new-vals)))
    maps))
 * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})

;; Useful function in itself, this conbines merges two maps using the
;; function f. Reducing a bunch of maps with it should be this
;; problem's colution
((fn [a b f]
   (let [new-vals (map (fn [[k v]] [k (if (a k) (f (a k) v) v)]) b)
         new-vals-flat (apply concat new-vals)] ;; flatten
     (apply (partial assoc a) new-vals-flat)))
 {:a 2, :b 3, :c 4} {:a 3 :d 6} *)

;; Like this (the solution):
((fn [f & maps]
   (reduce
    (fn [a b]
      (let [new-vals (map (fn [[k v]] [k (if (a k) (f (a k) v) v)]) b)
            new-vals-flat (apply concat new-vals)] ;; flatten
        (apply (partial assoc a) new-vals-flat)))
    maps))
 * {:a 2, :b 3, :c 4} {:a 3 :d 6} {:b 7})






;; The balance of N
;; Uses the digits function from above

((fn [n]
   (let [digit-fn (fn digits [n s]
                    (if (= n 0) s
                        (recur (int (/ n 10)) (conj s (rem n 10)))))
         digit-seq (digit-fn n '() )
         half (int (/ (count digit-seq) 2))
         sum-first-half-fn (fn [seq] (reduce + (take half seq)))]
     (= (sum-first-half-fn digit-seq)
        (sum-first-half-fn (reverse digit-seq)))))
 1298912)



;; Rearranging code

(= (reduce + (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
   (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (reduce +))
   11)
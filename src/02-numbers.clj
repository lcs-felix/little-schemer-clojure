(defn one? [n]
  (= n 1))

(defn + [n m]
  (if (zero? n)
    m
    (inc (+ (dec n) m))))

(defn - [n m]
  (if (zero? m)
    n
    (dec (- n (dec m)))))

(defn x [n m]
  (if (zero? m)
    0
    (+ n (x n (dec m)))))

(defn addtup [tup]
  (if (empty? tup)
    0
    (+ (first tup) (addtup (rest tup)))))

(defn tup+ [tup1 tup2]
  (cond (empty? tup1) tup2
        (empty? tup2) tup1
        :else (cons (+ (first tup1) (first tup2))
              (tup+ (rest tup1) (rest tup2)))))

(defn > [n m]
  (cond (= 0 n) false
        (= 0 m) true
        :else (> (dec n) (dec m))))

(defn < [n m]
  (cond (= 0 m) false
        (= 0 n) true
        :else (< (dec n) (dec m))))

(defn my-eq [n m]
  (if (or (< n m) (> n m))
    false
    true))

(defn expt [n m]
  (if (zero? m)
    1
    (* n (expt n (dec m)))))

(defn length [lat]
  (if (empty? lat)
    0
    (inc (length (rest lat)))))

(defn pick [n lat]
  (if (zero? (dec n))
    (first lat)
    (pick (dec n) (rest lat))))

(defn rempick [n lat]
  (if (one? n)
    (rest lat)
    (cons (first lat) (rempick (dec n) (rest lat)))))

(defn no-nums [lat]
  (cond (empty? lat) '()
        (number? (first lat)) (no-nums (rest lat))
        :else (cons (first lat) (no-nums (rest lat)))))

(defn all-nums [lat]
  (cond (empty? lat) '()
        (number? (first lat)) (cons (first lat) (all-nums (rest lat)))
        :else (all-nums (rest lat))))

(defn occur [a lat]
  (cond (empty? lat) 0
        (= a (first lat)) (inc (occur a (rest lat)))
        :else (occur a (rest lat))))

(occur 5 '(5 "a" 3 "b" 5 6 "c" 5))

(all-nums '("a" 3 "b" 5 6 "c"))

(no-nums '(5 "pears" 6 "prunes" 9 "dates"))
; ("pears" "prunes" "dates")

(rempick 3 '("hotdogs" "with" "hot" "mustard"))
; ("hotdogs" "with" "mustard")

(pick 4 '("lasagna" "spaghetti" "ravioli" "macaroni" "meatball"))
; "macarroni"

(length '("ham" "and" "cheese" "on" "rye"))

(expt 5 3)

(my-eq 10 10)
(my-eq 10 8)
(my-eq 8 10)

(< 12 133)
(< 133 12)
(< 12 12)

(> 12 133)
(> 120 11)
(> 5 5)

(tup+ '(3 6 9 11 4) '(8 5 2 0 7))
; (11 11 11 11 11)

(tup+ '(3 7) '(4 6 8 1))
; (7 13 8 1)

(tup+ '(3 7 10 11) '(4 6))
; (7 13 10 11)

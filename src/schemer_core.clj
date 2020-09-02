(ns schemer-core)

(defn atom?
  [x]
  (not (list? x)))

(defn equal? [s1 s2]
  (cond (and (sc/atom? s1) (sc/atom? s2)) (= s1 s2)
        (or (sc/atom? s1) (sc/atom? s2)) false
        :else (eqlist? s1 s2)))

(defn eqlist? [l1 l2]
  (cond
    (and (empty? l1) (empty? l2)) true
    (or (empty? l1) (empty? l2)) false
    :else (and (equal? (first l1) (first l2))
               (eqlist? (rest l1) (rest l2)))))

(eqlist? (list "banana" (list (list "split"))) (list (list "banana") (list "split")))
; true
(eqlist? (list "banana" (list (list "split"))) (list "banana" (list (list "split"))))
; true
(eqlist? (list "strawberry" "ice" "cream") (list "strawberry" "ice" "cream"))
; false
(eqlist? (list "strawberry" "ice" "cream") (list "strawberry" "cream" "ice"))
; false
(eqlist? (list "beef" (list (list "sausage")) (list "and" (list "soda")))
         (list "beef" (list (list "salami")) (list "and" (list "soda"))))
; true
(eqlist? (list "beef" (list (list "sausage")) (list "and" (list "soda")))
         (list "beef" (list (list "sausage")) (list "and" (list "soda"))))

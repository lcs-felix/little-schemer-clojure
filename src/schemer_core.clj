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

(defn lat?
  [lat]
  (cond
    (empty? lat) true
    (sc/atom? (first lat)) (recur (rest lat))
    :else false))

(defn member?
  [element lat]
  (cond
    (empty? lat) false
    :else (or (= element (first lat))
              (recur element (rest lat)))))

(defn rember
  [element lat]
  (let [first (first lat) rest (rest lat)]
    (cond (empty? lat) '()
          (= element first) rest
          :else (cons first (rember element rest)))))

(defn firsts
  [lists]
  (if (empty? lists)
    '()
    (cons (first (first lists))
          (firsts (rest lists)))))

(defn insert-r
  [new old lat]
  (cond (empty? lat) '()
        (= old (first lat)) (cons (first lat) (cons new (rest lat)))
        :else (cons (first lat) (insert-r new old (rest lat)))))

; mulirember

(defn multi-rember
  [a lat]
  (cond (empty? lat) '()
        (= (first lat) a) (multi-rember a (rest lat))
        :else (cons (first lat) (multi-rember a (rest lat)))))

; multiinsertR
(defn multi-insert-r
  [new old lat]
  (cond (empty? lat) '()
        (= (first lat) old) (cons (first lat) (cons new (multi-insert-r new old (rest lat))))
        :else (cons (first lat) (multi-insert-r new old (rest lat)))))

; tests

(multi-rember "lucas" (list "lucas" "joao" "vicente" "lucas" "pedro" "lucas"))

(insert-r "topping" "fudge" (list "ice" "cream" "with" "fudge" "for" "desert"))

(multi-insert-r "topping" "fudge" (list "fudge" "ice" "cream" "with" "fudge" "for" "desert" "fudge"))

(lat? (list "lucas" "felix"))
(lat? (list (list "a") "b" "c"))

(lat? (list "a" (list "b" "c") "d"))

(rember "bacon" (list "bacon" "lettuce" "and" "tomato"))
(rember "bacon" (list "lettuce" "bacon" "and" "tomato"))
(rember "bacon" (list "bacon" "lettuce" "bacon" "and" "tomato"))
(rember "bacon" (list "lettuce" "bacon" "salsa" "bacon" "and" "tomato"))

(firsts (list (list "apple" "peach" "pumpkin")
              (list "plum" "pear" "cherry")
              (list "grape" "raisin" "pea")
              (list "bean" "carrot" "eggplant")))

(firsts (list (list "five" "plums")
              (list "four")
              (list "eleven" "green" "oranges")))

(firsts '())
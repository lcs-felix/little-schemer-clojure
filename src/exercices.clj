(defn atom?
  [x]
  (not (list? x)))

; write the function lat?

(defn lat?
  [lat]
  (cond
    (empty? lat) true
    (atom? (first lat)) (recur (rest lat))
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
  (if (empty? lists) '()
    (cons (first (first lists))
          (firsts (rest lists)))))

; tests

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
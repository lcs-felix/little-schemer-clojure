(ns sets
  (:require [schemer-core :as sc]))

(defn set? [lat]
  (cond
    (empty? lat) true
    (sc/member? (first lat) (rest lat)) false
    :else (set? (rest lat))))

(comment
  (set? (list "apple" "peaches" "apple" "plum"))
  (set? (list "apples" "peaches" "pears" "plums"))
  )

(defn makeset [lat]
  (if (empty? lat)
    '()
    (let [el (first lat)
          sub-lat (rest lat)]
      (cons el (makeset (sc/multi-rember el sub-lat))))))

(comment
  (makeset (list "apple" "peach" "pear" "peach" "plum" "apple" "lemon" "peach"))
  )

(defn subset? [lat1 lat2]
  (if (empty? lat1)
    true
    (and (sc/member? (first lat1) lat2)
         (subset? (rest lat1) lat2))))

(comment
  (let [set1 (list 5 "chicken" "wings")
        set2 (list 5 "hamburgers" 2 "pieces" "fried" "chicken" "and" "light" "duckling" "wings")]
    (subset? set1 set2))
  (let [set1 (list 4 "pounds" "of" "horseradish")
        set2 (list "four" "pounds" "chicken" "and" 5 "ounces" "horseradish")]
    (subset? set1 set2))
  )

(defn eqset? [set1 set2]
  (and (subset? set1 set2)
         (subset? set2 set1)))

(comment
  (let [set1 (list 6 "large" "chickens" "with" "wings")
        set2 (list 6 "chickens" "with" "large" "wings")]
    (eqset? set1 set2))
  )

(defn intersect? [set1 set2]
  (if (empty? set1) false
        (or (sc/member? (first set1) set2)
        (intersect? (rest set1) set2))))

(comment
  (let [set1 (list "stewed" "tomatoes" "and" "macaroni")
        set2 (list "macaroni" "and" "cheese")]
    (intersect? set1 set2))
  )

(defn intersect [set1 set2]
  (cond
    (empty? set1) '()
    (sc/member? (first set1) set2) (cons (first set1) (intersect (rest set1) set2))
    :else (intersect (rest set1) set2)))

(comment
  (let [set1 (list "stewed" "tomatoes" "and" "macaroni")
        set2 (list "macaroni" "and" "cheese")]
    (intersect set1 set2))
  )

(defn union [set1 set2]
  (cond
    (empty? set1) set2
    (sc/member? (first set1) set2) (union (rest set1) set2)
    :else (cons (first set1) (union (rest set1) set2))))

(comment
  (let [set1 (list "stewed" "tomatoes" "and" "macaroni" "casserole")
        set2 (list "macaroni" "and" "cheese")]
    (union set1 set2))
  )

(defn intersectall [l-set]
  (if (empty? (rest l-set))
    (first l-set)
    (intersect (first l-set) (intersectall (rest l-set)))))

(comment
  (intersectall (list (list 6 "pears" "and")
                 (list 3 "peaches" "and" 6 "peppers")
                 (list 8 "pears" "and" 6 "plums")
                 (list "and" 6 "prunes" "with" "some" "apples")))
  )

(defn fun? [rel]
  (set? (sc/firsts rel)))

(comment
  (fun? (list (list "d" 4) (list "b" 0) (list "b" 9) (list "e" 5) (list "g" 4)))
  )
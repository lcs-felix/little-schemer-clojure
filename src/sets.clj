(ns sets
  (require '[schemer-core :as sc]))

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

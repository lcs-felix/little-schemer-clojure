(ns sets
  (require [schemer-core :refer :all]))

(defn set? [lat]
  (cond
    (empty? lat) true
    (member? (first lat) (rest lat)) false
    :else (set? (rest lat))))

(comment
  (set? (list "apple" "peaches" "apple" "plum"))
  (set? (list "apples" "peaches" "pears" "plums"))
  )


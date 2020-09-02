(ns stars
  (require '[schemer-core :as sc]))

(defn rember* [a l]
  (cond (empty? l) '()
        (sc/atom? (first l))
          (if (= a (first l))
            (rember* a (rest l))
            (cons (first l) (rember* a (rest l))))
        :else (cons (rember* a (first l)) (rember* a (rest l)))))

;  ((coffee) cup ((tea) cup) (and (hick)) cup)
(rember* "cup" (list (list "coffee") "cup" (list (list "tea") "cup") (list "and" (list "hick")) "cup"))
; (("coffee") (("tea")) ("and" ("hick")))

(defn insert-r* [new old l]
  ; (println "new: " new ", old: " old ", l: " l)
  (cond (empty? l) '()
        (sc/atom? (first l))
          (if (= (first l) old)
            (cons (first l) (cons new (insert-r* new old (rest l))))
            (cons (first l) (insert-r* new old (rest l))))
          :else (cons (insert-r* new old (first l)) (insert-r* new old (rest l)))))

(insert-r* "roast" "chuck" (list "roast"))

(insert-r* "roast" "chuck" (list (list "how" "much" (list "wood")) "could"
      (list (list "a" (list "wood") "chuck"))
      (list (list (list "chuck")))
      (list "if" (list "a") (list (list "wood" "chuck"))) "could" "chuck" "wood"))

(defn occur* [a l]
  ; (println (str "a: " a ", l: " l))
  (cond (empty? l) 0
        (sc/atom? (first l)) (if (= (first l) a)
                            (inc (occur* a (rest l)))
                            (occur* a (rest l)))
        :else (+ (occur* a (first l)) (occur* a (rest l)))))

(occur* "banana" (list (list "banana") (list "split" (list (list (list (list "banana" "ice")))
                                        (list "cream" (list "banana")) "sherbet"))
                  (list "banana") (list "bread")
                  (list "banana" "brandy")))

(defn subst* [new old l]
  (cond (empty? l) '()
        (sc/atom? (first l))
          (if (= old (first l)) (cons new (subst* new old (rest l)))
                                (cons (first l) (subst* new old (rest l))))
        :else (cons (subst* new old (first l)) (subst* new old (rest l)))))

(subst* "orange" "banana" (list (list "banana")
                                (list "split" (list (list (list (list "banana" "ice")))
                                  (list "cream" (list "banana")) "sherbet")) (list "banana")
                                (list "bread") (list "banana" "brandy")))

(defn insert-l* [new old l]
  (cond (empty? l) '()
        (sc/atom? (first l))
          (if (= old (first l)) (cons new (cons old (insert-l* new old (rest l))))
                                (cons (first l) (insert-l* new old (rest l))))
        :else (cons (insert-l* new old (first l))
                    (insert-l* new old (rest l)))))

(insert-l* "orange" "banana" (list (list "banana")
                                (list "split" (list (list (list (list "banana" "ice")))
                                                    (list "cream" (list "banana")) "sherbet")) (list "banana")
                                (list "bread") (list "banana" "brandy")))

(defn member* [a l]
  (cond (empty? l) false
        (sc/atom? (first l))
          (or (= a (first l)) (member* a (rest l)))
        :else (or (member* a (first l)) (member* a (rest l)))))

(member* "blah" (list (list "banana")
                                   (list "split" (list (list (list (list "banana" "ice")))
                                                       (list "cream" (list "banana")) "sherbet")) (list "banana")
                                   (list "bread") (list "banana" "brandy")))

(defn leftmost [l]
  ; (println "l:" l)
  (if (sc/atom? (first l))
    (first l)
    (leftmost (first l))))

(leftmost (list (list (list "hot") (list "tuna" (list "and"))) "cheese")) ; false

(defn rember
  [s l]
  (cond (empty? l) '()
        (sc/equal? s (first l)) (rest l)
        :else (cons (first l) (rember* s (rest l)))))

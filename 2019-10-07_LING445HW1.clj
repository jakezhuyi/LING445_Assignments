;; q1
(defn abs [x] (Math/sqrt(* x x)))

;; q2
(defn take-square [x] (* x x))
(defn sum-of-squares [x y] (+ (take-square x) (take-square y)))

;; q3
(def exp-13-1 (+ 6 7))
(def exp-13-2 (+ (* 2 6) 1))
(def exp-13-3 (/ (* 13 13) 13))
(def exp-13-4 (/ 65 5))
;; exp-13-1
;; exp-13-2
;; exp-13-3
;; exp-13-4

;; q4
(defn third [lst] (first (rest (rest lst))))
;; (third (list 4 5 6))

;; q5
(defn compose [f g]
  (fn [& arg]
    (f (apply g arg))))
;; (defn sqrt [x] (Math/sqrt x))
;; (defn abs [x] (Math/abs x))
;; ((compose sqrt abs) -36)

;; q6
(defn first-two [lst]
  (list (first lst) (first (rest lst))))
;; (first-two (list 4 5 6))

;; q7
(defn remove-second [lst]
  (cons (first lst) (rest (rest lst))))
;; (remove-second (list 3 1 4))

;; q8
(defn add-to-end [lst e]
  (if (empty? lst)
    (list e)
    (cons (first lst) (add-to-end (rest lst) e))))
;; (add-to-end (list 5 6 4) 0)

;; q9
;; (defn reverse [lst]
;;   (if (empty? lst)
;;     (list)
;;     (concat (reverse (rest lst)) (list (first lst)))))
(defn reverse [lst]
  (if (empty? lst)
    (list)
    (add-to-end (reverse (rest lst)) (first lst))))
;; (reverse (list 1 2 3))

;; q10
(defn count-to-1 [n]
  (if (zero? n)
    (list)
    (cons n (count-to-1 (- n 1)))))
;; (count-to-1 3)

;; q11
(defn count-to-n [n] (reverse (count-to-1 n)))
;; (count-to-n 3)

;; q12
(defn max-int [lst e]
  (if (empty? lst)
    e
    (if (> (first lst) e)
      (max-int (rest lst) (first lst))
      (max-int (rest lst) e))))
(defn get-max [lst]
  (max-int lst (first lst)))
;; (get-max (list 1 5 8 100 2))

;; q13
(defn greater-than-five? [lst] (map (fn [num] (> num 5)) lst))
;; (greater-than-five? (list 5 4 7))

;; q14
(defn concat-two [x y]
  (if (empty? x)
    y
    (cons (first x) (concat-two (rest x) y))))
(defn concat-three [x y z]
  (concat-two (concat-two x y) z))
;; (concat-two '(1 2 3) '(4 5 6))
;; (concat-three '(1 2 3) '(4 5 6) '(7 8 9))

;; q15
;; (defn sequence-to-power [lst n] 
;;   (if (zero? n)
;;     (list)
;;     (concat lst (sequence-to-power lst (- n 1)))))
(defn sequence-to-power [lst n] 
  (if (zero? n)
    (list)
    (concat-two lst (sequence-to-power lst (- n 1)))))
;; (sequence-to-power (list 1 2 3) 3)

;; q16
(defn in-L? [x]
  (if (empty? x)
    true
    (if (= (quote a) (first x))
      (in-L? (rest x))
      false)))
;; (in-L? (quote (a a)))
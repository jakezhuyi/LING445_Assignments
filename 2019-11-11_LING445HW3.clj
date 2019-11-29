(defn log2 [n] (/ (Math/log n) (Math/log 2)))

(def vocabulary '(call me ishmael))

(def theta1 (list (/ 1 2 ) (/ 1 4 ) (/ 1 4 )))
(def theta2 (list (/ 1 4 ) (/ 1 2 ) (/ 1 4 )))

(def thetas (list theta1 theta2))

(def theta-prior (list (/ 1 2) (/ 1 2)))

(defn score-categorical [outcome outcomes params]
  (if (empty? params)
      (throw "no matching outcome")
      (if (= outcome (first outcomes))
          (first params)
          (score-categorical outcome (rest outcomes) (rest params)))))

(defn list-foldr [f base lst]
  (if (empty? lst)
      base
      (f (first lst)
         (list-foldr f base (rest lst)))))

(defn score-BOW-sentence [sen probabilities]
  (list-foldr 
   (fn [word rest-score] 
     (+ (log2 (score-categorical word vocabulary probabilities))
        rest-score))
   0
   sen))

(defn score-corpus [corpus probabilities]
  (list-foldr
   (fn [sen rst]
     (+ (score-BOW-sentence sen probabilities) rst))
   0
   corpus))

(defn logsumexp [log-vals]
  (let [mx (apply max log-vals)]
    (+ mx
       (log2
           (apply +
                  (map (fn [z] (Math/pow 2 z))
                       (map (fn [x] (- x mx)) log-vals)))))))

(defn normalize [params]
  (let [sum (apply + params)]
    (map (fn [x] (/ x sum)) params)))

(defn flip [weight]
  (if (< (rand 1) weight)
      true
      false))

(defn sample-categorical [outcomes params]
  (if (flip (first params))
      (first outcomes)
      (sample-categorical (rest outcomes) 
                          (normalize (rest params)))))

(defn sample-BOW-sentence [len probabilities]
        (if (= len 0)
          '()
          (cons (sample-categorical vocabulary probabilities)
            (sample-BOW-sentence (- len 1) probabilities))))

(defn get-theta [theta-corpus]
  (first theta-corpus))

(defn get-corpus [theta-corpus]
  (first (rest theta-corpus)))
  
(def my-corpus '((call me)
                      (call ishmael)))

;; q1
(defn theta-corpus-joint [theta corpus theta-probs]
  (if (= theta (first thetas))
    (+ (score-corpus corpus theta)(log2 (first theta-probs)))
    (+ (score-corpus corpus theta)(log2 (first (rest theta-probs))))))
(theta-corpus-joint theta1 my-corpus theta-prior)
(theta-corpus-joint theta2 my-corpus theta-prior)

;; q2
(defn compute-marginal [corpus theta-probs]
  (logsumexp (list (theta-corpus-joint theta1 corpus theta-probs)
    (theta-corpus-joint theta2 corpus theta-probs))))
(compute-marginal my-corpus theta-prior)
(Math/pow 2 (compute-marginal my-corpus theta-prior))

;; q3
(defn compute-conditional-prob [theta corpus theta-probs]
  (- (theta-corpus-joint theta corpus theta-probs)
    (compute-marginal corpus theta-probs)))
(compute-conditional-prob theta1 my-corpus theta-prior)

;; q4
(defn compute-conditional-dist [corpus theta-probs]
  (list (compute-conditional-prob theta1 corpus theta-probs)
    (compute-conditional-prob theta2 corpus theta-probs)))

;; q5
(compute-conditional-dist my-corpus theta-prior)
(defn exponentiate [lst]
  (if (empty? lst)
    '()
    (cons (Math/pow 2 (first lst)) (exponentiate (rest lst)))))
(exponentiate (compute-conditional-dist my-corpus theta-prior))

;; q6
(defn compute-posterior-predictive [observed-corpus new-corpus theta-probs]
  (let [conditional-dist (exponentiate 
    (compute-conditional-dist observed-corpus theta-probs))]
  (compute-marginal new-corpus conditional-dist)))
(compute-posterior-predictive my-corpus my-corpus theta-prior)
(Math/pow 2 (compute-posterior-predictive my-corpus my-corpus theta-prior))

;; q7
(defn sample-BOW-corpus [theta sent-len corpus-len]
  (repeatedly corpus-len (fn [] (sample-BOW-sentence sent-len theta))))
(sample-BOW-corpus theta1 2 2)

;; q8
(defn sample-theta-corpus [sent-len corpus-len theta-probs]
  (let [theta (sample-categorical thetas theta-probs)]
  (list theta (sample-BOW-corpus theta sent-len corpus-len))))
(sample-theta-corpus 2 2 theta1)

(defn sample-thetas-corpora [sample-size sent-len corpus-len theta-probs]
  (repeatedly sample-size 
    (fn [] (sample-theta-corpus sent-len corpus-len theta-probs))))

;; q9
(defn estimate-corpus-marginal [corpus sample-size sent-len 
  corpus-len theta-probs]
  (/ (apply + 
    (let [sample-corpora 
      (sample-thetas-corpora sample-size sent-len corpus-len theta-probs)] 
    (map (fn [c] (if (= c corpus) 1 0)) 
      (map (fn [x] (get-corpus x)) sample-corpora)))) sample-size))

;; q10
;; (estimate-corpus-marginal my-corpus 50 2 2 theta-prior)
;; (estimate-corpus-marginal my-corpus 50 2 2 theta-prior)
;; (estimate-corpus-marginal my-corpus 50 2 2 theta-prior)
;; (estimate-corpus-marginal my-corpus 50 2 2 theta-prior)
;; (estimate-corpus-marginal my-corpus 50 2 2 theta-prior)
;; (estimate-corpus-marginal my-corpus 10000 2 2 theta-prior)
;; (estimate-corpus-marginal my-corpus 10000 2 2 theta-prior)
;; (estimate-corpus-marginal my-corpus 10000 2 2 theta-prior)
;; (estimate-corpus-marginal my-corpus 10000 2 2 theta-prior)
;; (estimate-corpus-marginal my-corpus 10000 2 2 theta-prior)

;; q11
(defn get-count [obs observation-list count]
  (if (empty? observation-list)
    count
    (if (= obs (first observation-list))
      (get-count obs (rest observation-list) (+ 1 count))
      (get-count obs (rest observation-list) count))))

(defn get-counts [outcomes observation-list]
  (let [count-obs (fn [obs] (get-count obs observation-list 0))]
    (map count-obs outcomes)))

(defn rm-2 [observed-corpus corpora-pairs]
  (filter (fn [x] (= observed-corpus (first (rest x)))) corpora-pairs))

(defn rejection-sampler [theta observed-corpus sample-size sent-len 
  corpus-len theta-probs]
  (let [corpora-pairs 
    (sample-thetas-corpora sample-size sent-len corpus-len theta-probs)]
    (let [matching-corpora (rm-2 observed-corpus corpora-pairs)]
      (let [sample-theta (map (fn [x] (get-theta x)) matching-corpora)]
        (if (= 0 (count matching-corpora))
          Double/NaN
          (/ (get-count theta sample-theta 0) (count matching-corpora)))))))

;; q12
;; (rejection-sampler theta1 my-corpus 100 2 2 theta-prior)
;; (rejection-sampler theta1 my-corpus 100 2 2 theta-prior)
;; (rejection-sampler theta1 my-corpus 100 2 2 theta-prior)
;; (rejection-sampler theta1 my-corpus 200 2 2 theta-prior)
;; (rejection-sampler theta1 my-corpus 500 2 2 theta-prior)
;; (rejection-sampler theta1 my-corpus 1000 2 2 theta-prior)
;; (rejection-sampler theta1 my-corpus 5000 2 2 theta-prior)
;; (rejection-sampler theta1 my-corpus 10000 2 2 theta-prior)
;; (rejection-sampler theta1 my-corpus 15000 2 2 theta-prior)
;; (rejection-sampler theta1 my-corpus 15000 2 2 theta-prior)
;; (rejection-sampler theta1 my-corpus 15000 2 2 theta-prior)
;; (rejection-sampler theta1 my-corpus 17500 2 2 theta-prior)
;; (rejection-sampler theta1 my-corpus 17500 2 2 theta-prior)
;; (rejection-sampler theta1 my-corpus 17500 2 2 theta-prior)
;; (rejection-sampler theta1 my-corpus 20000 2 2 theta-prior)
;; (rejection-sampler theta1 my-corpus 20000 2 2 theta-prior)
;; (rejection-sampler theta1 my-corpus 20000 2 2 theta-prior)
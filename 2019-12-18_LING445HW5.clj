(defn log2 [n] (/ (Math/log n) (Math/log 2)))

(def hidden-states '(Start N V))
(def vocabulary '(Call me Ishmael)) 
(def theta-transition-Start '(0 0.9 0.1))
(def theta-transition-N '(0 0.3 0.7))
(def theta-transition-V '(0 0.8 0.2))
(def theta-transition-dists-1 
  (list theta-transition-Start theta-transition-N theta-transition-V)) 
(def theta-observation-Start '(0 0 0))
(def theta-observation-N '(0.1 0.5 0.4))
(def theta-observation-V '(0.8 0.1 0.1))
(def theta-observation-dists-1 
  (list theta-observation-Start theta-observation-N theta-observation-V))

(defn dist-lookup [state states dists] 
  (if (= state (first states)) 
    (first dists) 
    (dist-lookup state (rest states) (rest dists))))

(defn logsumexp [log-vals] 
  (let [mx (apply max log-vals)] 
    (+ mx 
      (log2 
        (apply + 
          (map (fn [z] (Math/pow 2 z)) 
            (map (fn [x] (- x mx)) log-vals)))))))

(defn logscore-categorical [outcome outcomes params] 
  (if (= outcome (first outcomes)) 
    (log2 (first params)) 
    (logscore-categorical outcome (rest outcomes) (rest params))))

;; q1
(defn score-next-state-word [current-hidden next-hidden next-observed 
  theta-transition-dists theta-observation-dists]
  (+ 
  (logscore-categorical next-observed vocabulary 
    (dist-lookup next-hidden hidden-states theta-observation-dists))
  (logscore-categorical next-hidden hidden-states 
    (dist-lookup current-hidden hidden-states theta-transition-dists))))

;; q2
(defn compute-next-observation-marginal [current-state next-observation 
  theta-transition-dists theta-observation-dists]
  (logsumexp (map (fn [x] (score-next-state-word current-state x next-observation 
    theta-transition-dists theta-observation-dists))
       (rest hidden-states))))

;; q3
(defn score-next-states-words [current-hidden next-hidden-states next-words 
  theta-transition-dists theta-observation-dists]
  (if (empty? next-words)
    0
    (+ 
    (score-next-state-word current-hidden (first next-hidden-states) (first next-words) 
      theta-transition-dists theta-observation-dists)
    (score-next-states-words (first next-hidden-states) (rest next-hidden-states) 
      (rest next-words) theta-transition-dists theta-observation-dists))))

;; q4
(defn compute-next-words-marginal [current-hidden next-words theta-transition-dists 
  theta-observation-dists]
  (if (empty? next-words)
    0
    (logsumexp 
    (list 
    (+ (score-next-state-word current-hidden (first (rest hidden-states)) 
      (first next-words) theta-transition-dists theta-observation-dists) 
      (compute-next-words-marginal (first (rest hidden-states)) (rest next-words) 
        theta-transition-dists theta-observation-dists))
    (+ (score-next-state-word current-hidden (first (rest (rest hidden-states))) 
      (first next-words) theta-transition-dists theta-observation-dists) 
      (compute-next-words-marginal (first (rest (rest hidden-states))) 
        (rest next-words) theta-transition-dists theta-observation-dists))))))

;; q5
(compute-next-words-marginal 
  'Start '(Call me) theta-transition-dists-1 theta-observation-dists-1)
;; -4.227016447861896
(compute-next-words-marginal 
  'Start '(me Call) theta-transition-dists-1 theta-observation-dists-1)
;; -1.9002335137070705
(compute-next-words-marginal 
  'Start '(me Ishmael) theta-transition-dists-1 theta-observation-dists-1)
;; -3.4916727707196795
(compute-next-words-marginal 
  'Start '(Ishmael me) theta-transition-dists-1 theta-observation-dists-1)
;; -3.583808806104786

;; q6
(defn compute-hidden-prior [k thetas]
  (if (= (count k) 1)
    (logscore-categorical (first k) hidden-states (first thetas)) 
    (+ (logscore-categorical (first (rest k)) hidden-states
      (dist-lookup (first k) hidden-states thetas))
       (compute-hidden-prior (rest k) thetas))))

;; q7
(defn compute-likelihood-of-words [k words thetas]
  (if (empty? k)
    0
    (+ (logscore-categorical (first words) vocabulary 
      (dist-lookup (first k) hidden-states thetas)) 
    (compute-likelihood-of-words (rest k) (rest words) thetas))))

;; q8
(defn compute-hidden-posterior [k words transition observation]
  (- (+ (compute-hidden-prior k transition) 
      (compute-likelihood-of-words k words observation))
    (compute-next-words-marginal 'Start words transition observation)))

;; q10
(def compute-next-words-marginal-mem (memoize (fn 
  [current-hidden next-words theta-transition-dists 
    theta-observation-dists]
  (if (empty? next-words)
    0
    (logsumexp 
    (list 
    (+ (score-next-state-word current-hidden (first (rest hidden-states)) 
      (first next-words) theta-transition-dists theta-observation-dists) 
      (compute-next-words-marginal (first (rest hidden-states)) (rest next-words) 
        theta-transition-dists theta-observation-dists))
    (+ (score-next-state-word current-hidden (first (rest (rest hidden-states))) 
      (first next-words) theta-transition-dists theta-observation-dists) 
      (compute-next-words-marginal (first (rest (rest hidden-states))) 
        (rest next-words) theta-transition-dists theta-observation-dists))))))))
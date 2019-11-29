;; q1
(def moby-word-tokens '(CALL me Ishmael . Some years ago never mind
     how long precisely having little or no money in my purse , and
     nothing particular to interest me on shore , I thought I would
     sail about a little and see the watery part of the world .  It is
     a way I have of driving off the spleen , and regulating the
     circulation . Whenever I find myself growing grim about the mouth
     whenever it is a damp , drizzly November in my soul whenever I
     find myself involuntarily pausing before coffin warehouses , and
     bringing up the rear of every funeral I meet and especially
     whenever my hypos get such an upper hand of me , that it requires
     a strong moral principle to prevent me from deliberately stepping
     into the street , and methodically knocking people's hats off
     then , I account it high time to get to sea as soon as I can .
     This is my substitute for pistol and ball . With a philosophical
     flourish Cato throws himself upon his sword I quietly take to the
     ship .  There is nothing surprising in this . If they but knew it
     , almost all men in their degree , some time or other , cherish
     very nearly the same feelings toward the ocean with me .))
(defn member-of-list? [w l]
  (if (empty? l)
    false
    (if (= w (first l))
      true
      (member-of-list? w (rest l)))))
(defn get-vocabulary [word-tokens vocab]
  (if (empty? word-tokens)
    vocab
    (if (member-of-list? (first word-tokens) vocab)
      (get-vocabulary (rest word-tokens) vocab)
      (get-vocabulary (rest word-tokens)
        (cons (first word-tokens) vocab)))))
;; (def word-tokens '(the man is man the))
;; (def vocab (get-vocabulary word-tokens '()))
(def moby-vocab (get-vocabulary moby-word-tokens '()))

;; q2
(defn get-count-of-word [w word-tokens count]
  (if (empty? word-tokens)
    count
    (if (= w (first word-tokens))
      (get-count-of-word w (rest word-tokens) (+ count 1))
      (get-count-of-word w (rest word-tokens) count))))
;; (get-count-of-word 'the word-tokens 0)
(defn get-word-counts [vocab word-tokens]
  (let [count-word (fn [w](get-count-of-word w word-tokens 0))]
  (map count-word vocab)))
;; (def word-tokens '(the man is is))
;; (get-word-counts vocab word-tokens)

;; q3
(defn flip [p]
  (if (< (rand 1) p)
    true
    false))
(defn normalize [params]
  (let [sum (apply + params)]
    (map (fn [x] (/ x sum)) params)))
(defn sample-categorical [outcomes params]
  (if (flip (first params))
    (first outcomes)
    (sample-categorical (rest outcomes) 
      (normalize (rest params)))))
(defn create-uniform-distribution [outcomes]
  (let [num-outcomes (count outcomes)]
    (map (fn [x] (/ 1 num-outcomes))
   outcomes)))
(def moby-word-frequencies (get-word-counts moby-vocab moby-word-tokens))
;; moby-word-frequencies
;; (create-uniform-distribution '(the a every))

;; q4
;; https://foundations-computational-linguistics.github.io/chapters/11-BagOfWords.html
(defn list-unfold [generator n]
  (if (= n 0)
    '()
    (cons (generator)
      (list-unfold generator (- n 1)))))
(defn sample-uniform-BOW-sentence [n vocab]
  (list-unfold 
    (fn [] (sample-categorical vocab
      (create-uniform-distribution vocab))) n))
;; (sample-uniform-BOW-sentence 4 '(the a every))

;; q5
;; https://rosettacode.org/wiki/Exponentiation_operator
(defn ** [x n] (reduce * (repeat n x)))
(defn compute-uniform-BOW-prob [vocab sentence]
  (** (first (create-uniform-distribution vocab))
    (count sentence)))
;; (compute-uniform-BOW-prob '(the a every) '(b))
;; (compute-uniform-BOW-prob '(the a every) '(every every))

;; q6
(def moby-sentence (sample-uniform-BOW-sentence 3 moby-vocab))
;; moby-sentence
;; (compute-uniform-BOW-prob moby-vocab moby-sentence)

;; q7
(def moby-word-probabilities (normalize moby-word-frequencies))
(defn sample-BOW-sentence [len vocabulary probabilities]
  (if (= len 0)
    '()
    (cons (sample-categorical vocabulary probabilities)
    (sample-BOW-sentence (- len 1) vocabulary probabilities))))
;; moby-word-probabilities

;; q8
(sample-BOW-sentence 3 moby-vocab moby-word-probabilities)

;; q9
(defn product [l]
  (apply * l))
(defn lookup-probability [w outcomes probs]
  (if (= (first outcomes) w)
    (first probs)
    (lookup-probability w (rest outcomes) (rest probs))))
(defn product [l]
  (apply * l))
;; (lookup-probability 'the '(the a every) '(0.2 0.5 0.3))

;; q10
(defn get-prob [sentence vocabulary probabilities]
  (if (empty? sentence)
    '()
    (cons (lookup-probability (first sentence) vocabulary probabilities)
    (get-prob (rest sentence) vocabulary probabilities))))
(defn compute-BOW-prob [sentence vocabulary probabilities]
  (product (get-prob sentence vocabulary probabilities)))
;; (compute-BOW-prob '(to with account) moby-vocab moby-word-probabilities)
;; (compute-BOW-prob '(a every) '(the every a) '(0.2 0.3 0.5))

;; q11
(compute-BOW-prob moby-sentence moby-vocab moby-word-probabilities)
(compute-BOW-prob '(CALL degree whenever) moby-vocab moby-word-probabilities)
(compute-BOW-prob '(to with account) moby-vocab moby-word-probabilities)
(compute-BOW-prob '(interest other nothing) moby-vocab moby-word-probabilities)
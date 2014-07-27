(include prelude.ll)

(seed: 1) ; we could be more random here... 

(random: []
  (let ((m (- (pow 2 31) 1))
        (a 48271)
        (c 0))
       (seed: (mod (+ (* a seed) c) m))))

(genrandom: [n]
  (if (= n 0)
      nil
      (cons (random) (self (- n 1)))))

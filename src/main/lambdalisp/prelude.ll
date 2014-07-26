(progn
  (nil: -999)
  (seed: 1)
  (zero?: [x] (= x 0))
  (empty?: [l] (if (atom? l) (= l nil) 0))

  (reverse: [l]
    (let ((reverseaux [l res]
            (tif (empty? l)
                 res
                 (recur (cdr l) (cons (car l) res)))))
         (reverseaux l nil)))

  (nth: [li n]
    (tif (zero? n)
         (car li)
         (recur (cdr li) (- n 1))))

  (map: [l f]
    (let ((mapaux [l f res]
             (tif (empty? l)
                  (reverse res)
                  (recur (cdr l) f (cons (f (car l)) res)))))
         (mapaux l f nil)))

  (expt: [v n]
     (if (= n 0)
         1
         (* v (self v (- n 1)))))

  (mod: [n m]
    (- n (* (/ n m) m)))

  (random: []
    (let ((m (- (expt 2 31) 1))
           (a 48271)
           (c 1))
      (defvar seed (mod (+ (* a seed) c) m))))

  (genrandom: [n]
    (if (= n 0)
        nil
        (cons (random) (self (- n 1)))))

  (length: [l]
    (let ((lengthaux [l s]
             (tif (empty? l)
                  s
                  (recur (cdr l) (+ s 1)))))
      (lengthaux l 0)))

  (length (cons 1 (cons 2 (cons 3 (cons 4 nil))))))

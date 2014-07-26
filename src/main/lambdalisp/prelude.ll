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

  (filter: [l f]
    (let ((filteraux [l f res]
             (tif (empty? l)
                  (reverse res)
                  (recur (cdr l) f (if (f (car l))
                                       (cons (car l) res)
                                       res)))))
         (filteraux l f nil)))

  (find: [l f]
    (let ((findaux [l f n]
             (tif (empty? l)
                  nil
                  (tif (f (car l))
                       n
                       (recur (cdr l) f (+ n 1))))))
         (findaux l f 0)))

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

  (genrandom 10)
)
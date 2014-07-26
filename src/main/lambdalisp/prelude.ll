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

  (nnth: [li i j]
    (tif (zero? i)
         (nth (car li) j)
         (recur (cdr li) (- i 1) j)))

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

  (nnth (cons (cons 1 (cons 2 (cons 3 (cons 4 nil)))) (cons (cons 4 (cons 5 (cons 6 (cons 7 nil)))) (cons (cons 8 (cons 9 (cons 10 (cons 11 nil)))) nil))) 2 3))


(progn
  (nil: -999)
  (seed: 1)
  (empty?: [l] (if (atom? l) (= l nil) 0))

  (reverse: [l]
    (let ((reverseaux [l res]
            (tif (empty? l)
                 res
                 (recur (cdr l) (cons (car l) res)))))
         (reverseaux l nil)))

  (zero?: [x] (= x 0))

  (expt: [v n]
     (if (= n 0)
         1
         (* v (self v (- n 1)))))

  (mod: [n m]
    (- n (* (/ n m) m)))

  (random: []
    (let ((m (- (expt 2 31) 1))
           (a 48271)
           (c 0))
      (defvar seed (mod (+ (* a seed) c) m))))

  (nth: [li n]
    (tif (zero? n)
         (car li)
         (recur (cdr li) (- n 1))))

  (nnth: [li i j]
    (tif (zero? i)
         (nth (car li) j)
         (recur (cdr li) (- i 1) j)))

  (randomnot: [v m]
     (let ((r (mod (random) m)))
       (tif (= r v)
            (recur v m)
            r)))

  (main: [state]
    (let ((map (car state))
          (lambdaman (car (cdr state)))
          (ghosts (car (cdr (cdr state))))
          (fruit (cdr (cdr (cdr state)))))

      (let ((location (car (cdr lambdaman)))
            (direction (car (cdr (cdr lambdaman)))))
        
          (if (or (or (or (and (= direction 0) (= (nnth map (- (cdr location) 1) (car location)) 0))
                          (and (= direction 1) (= (nnth map (cdr location) (+ (car location) 1)) 0)))
                          (and (= direction 2) (= (nnth map (+ (cdr location) 1) (car location)) 0)))
                          (and (= direction 3) (= (nnth map (cdr location) (- (car location) 1)) 0)))
           (cons 0 (randomnot direction 4))
           (cons 0 direction)))))

  (cons 0 main))

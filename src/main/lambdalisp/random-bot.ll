(include prelude.ll)
(include random.ll)

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

(cons 0 main)

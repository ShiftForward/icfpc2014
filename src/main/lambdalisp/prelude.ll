(progn
  (nil: -999)
  (zero?: [x] (= x 0))
  (empty?: [li] (= li nil))

  (nth: [li n]
    (tif (zero? n)
         (car li)
         (recur (cdr li) (- n 1))))

  (map: [l f]
    (if (if (atom? l) (= l nil) 0)
        l
        (cons (f (car l)) (self (cdr l) f))))

  (map (cons 1 (cons 2 (cons 3 (cons 4 nil)))) [x] (+ 2 x))
)
(progn
  (nil: -999)
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

  (map (cons 1 (cons 2 (cons 3 (cons 4 nil)))) [x] (+ 2 x))
)

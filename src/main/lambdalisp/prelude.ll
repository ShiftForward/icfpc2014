; Some constants
(nil: -2147483648) ; basically -2^31
(true: 1)
(false: 0)

; Predicates
(zero?: [x] (= x 0))
(empty?: [l] (and (atom? l) (= l nil)))
(negative?: [x] (< x 0))
(positive?: [x] (>= x 0))

; Higher-order functions over lists
(foldLeft: [b l f]
  (let ((foldaux [l f res]
           (tif (empty? l)
                res
                (recur (cdr l) f (f res (car l))))))
       (foldaux l f b)))

(reverse: [l f] (foldLeft nil l [acc e] (cons e acc)))
(map: [l f] (reverse (foldLeft nil l [acc e] (cons (f e) acc))))
(filter: [l f] (reverse (foldLeft nil l [acc e] (if (f e) (cons e acc) acc))))
(forall: [l f] (foldLeft true l [acc e] (and (f e) acc)))
(exists: [l f] (foldLeft false l [acc e] (or (f e) acc)))
(last: [l] (foldLeft nil l [acc e] e))

(takeWhile: [l f]
  (let ((takeWhileAux [l f res]
           (tif (or (empty? l) (not (f (car l))))
                (reverse res)
                (recur (cdr l) f (cons (car l) res)))))
       (takeWhileAux l f nil)))

(dropWhile: [l f]
   (tif (or (empty? l) (not (f (car l))))
        l
        (recur (cdr l) f)))

; List manipulation
(length: [l] (foldLeft 0 l [acc e] (+ acc 1)))

(nth: [li n]
  (tif (zero? n)
       (car li)
       (recur (cdr li) (- n 1))))

(nnth: [li i j]
  (tif (zero? i)
       (nth (car li) j)
       (recur (cdr li) (- i 1) j)))

(find: [l f]
  (let ((findaux [l f n]
           (tif (empty? l)
                nil
                (tif (f (car l))
                     n
                     (recur (cdr l) f (+ n 1))))))
       (findaux l f 0)))

(range: [n m]
  (let ((rangeaux [m res]
          (tif (< m 0)
               res 
               (recur (- m 1) (cons (+ n m) res)))))
        (rangeaux (- m n) nil)))

; Math
(pow: [v n]
   (if (zero? n)
       1
       (* v (self v (- n 1)))))

(mod: [n m]
  (- n (* (/ n m) m)))

(abs: [x] 
  (tif (positive? x) x (- 0 x)))
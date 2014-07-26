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

(nth: [li n]
  (tif (zero? n)
       (car li)
       (recur (cdr li) (- n 1))))

(nnth: [li i j]
  (tif (zero? i)
       (nth (car li) j)
       (recur (cdr li) (- i 1) j)))

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

(length: [l]
  (let ((lengthaux [l s]
           (tif (empty? l)
                s
                (recur (cdr l) (+ s 1)))))
    (lengthaux l 0)))
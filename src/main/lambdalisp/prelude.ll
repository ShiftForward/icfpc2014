(nil: -2147483648)
(seed: 1)
(zero?: [x] (= x 0))
(empty?: [l] (if (atom? l) (= l nil) 0))
(true: 1)
(false: 0)

(foldLeft: [b l f]
  (let ((foldaux [l f res]
           (tif (empty? l)
                res
                (recur (cdr l) f (f res (car l))))))
       (foldaux l f b)))

(reverse: [l f] (foldLeft nil l [acc e] (cons e acc)))

(map: [l f] (reverse (foldLeft nil l [acc e] (cons (f e) acc))))

(filter: [l f] (reverse (foldLeft nil l [acc e] (if (f e) (cons e acc) acc))))

(length: [l] (foldLeft 0 l [acc e] (+ acc 1)))

(forall: [l f] (foldLeft 1 l [acc e] (and (f e) acc)))

(exists: [l f] (foldLeft 0 l [acc e] (or (f e) acc)))

(nth: [li n]
  (tif (zero? n)
       (car li)
       (recur (cdr li) (- n 1))))

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

(expt: [v n]
   (if (= n 0)
       1
       (* v (self v (- n 1)))))

(mod: [n m]
  (- n (* (/ n m) m)))

(set: [l i v]
   (let ((setaux [l i v res in]
           (tif (empty? l)
                (reverse res)
                (tif (= i in)
                  (recur (cdr l) i v (cons v res) (+ in 1))
                  (recur (cdr l) i v (cons (car l) res) (+ in 1))))))
     (setaux l i v nil 0)))

(append: [l v]
   (let ((appendaux [l v res]
           (tif (empty? l)
                (reverse (cons v res))
                (recur (cdr l) v (cons (car l) res)))))
      (appendaux l v nil)))

(fill: [n v]
   (let ((fillaux [n v res]
            (tif (= n 0)
                 res
                 (recur (- n 1) v (cons v res)))))
      (fillaux n v nil)))

(min: [l]
   (foldLeft (car l) (cdr l) [a b] (if (< a b) a b)))

(max: [l]
   (foldLeft (car l) (cdr l) [a b] (if (> a b) a b)))

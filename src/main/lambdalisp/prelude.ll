; Some constants
(nil: -2147483648) ; basically -2^31
(true: 1)
(false: 0)

; Basic tests
(zero?: [x] (= x 0))
(empty?: [l] (and (atom? l) (= l nil)))

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
          (tif (= m 0)
               res 
               (recur (- m 1) (cons m res)))))
        (rangeaux (- m n) nil)))

; Math
(pow: [v n]
   (if (zero? n)
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

(concat: [l1 l2]
  (let ((concat-aux [l1 l2]
           (tif (empty? l1)
                l2
                (recur (cdr l1) (cons (car l1) l2)))))
    (concat-aux (reverse l1) l2)))

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

(split: [l i]
  (let ((split-aux [l1 i l2]
          (tif (or (empty? l1) (= i 0))
               (cons (reverse l2) l1)
               (recur (cdr l1) (- i 1) (cons (car l1) l2)))))
    (split-aux l i nil)))

(flatten: [l]
  (if (empty? l)
      nil
      (if (atom? l)
          (cons l nil)
          (concat (self (car l)) (self (cdr l))))))

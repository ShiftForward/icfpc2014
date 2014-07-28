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
(map: [l f] 
  (if (empty? l)
      nil
      (cons (f (car l)) (self (cdr l) f))))
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
  (if (empty? l1)
      l2
      (cons (car l1) (self (cdr l1) l2))))

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
  (let* ((take1 [l i]
           (if (or (empty? l) (= i 0))
               nil
               (cons (car l) (self (cdr l) (- i 1)))))
         (take2 [l i]
           (if (or (empty? l) (= i 0))
               l
               (self (cdr l) (- i 1))))
         (l1 (take1 l i))
         (l2 (take2 l i)))
     (cons l1 l2)))

(flatten: [l]
  (if (empty? l)
      nil
      (if (atom? l)
          (cons l nil)
          (concat (self (car l)) (self (cdr l))))))

(flatten1: [l]
  (tif (empty? l)
       nil
       (concat (car l) (self (cdr l)))))



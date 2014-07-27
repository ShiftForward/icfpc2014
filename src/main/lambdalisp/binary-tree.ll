(include prelude.ll)

(binary-tree-create: [l]
  (let ((binary-tree-create-aux [l i]
    (tif (empty? l)
         nil
         (let* ((len (length l))
                (m (/ len 2))
                (s (split l m))
                (left (car s))
                (right (cdr s)))
            (cons (cons (+ i m) (car right)) (cons (self left i) (self (cdr right) (+ i m 1))))))))
     (binary-tree-create-aux l 0)))

(binary-tree-get: [b i]
  (let ((m (caar b)))
    (tif (= i m)
         (cdar b) 
         (tif (< i m)
              (recur (cadr b) i)
              (recur (cddr b) i)))))

(binary-tree-set: [b i v]
  (let ((m (caar b)))
    (tif (= i m)
         (cons (cons m v) (cdr b))
         (let ((left (cadr b))
               (right (cddr b)))
            (tif (< i m)
                 (cons (car b) (cons (self left i v) right))
                 (cons (car b) (cons left (self right i v))))))))


(include prelude.ll)

(binary-tree-create: [l]
  (let ((binary-tree-create-aux [l i]
    (let* ((len (length l))
           (m (/ len 2)))
      (tif (= len 1)
           (cons i (car l))
           (let* ((s (split l m))
                  (left (car s))
                  (right (cdr s)))
               (cons (+ i m) (cons (self left i) (self right (+ i m)))))))))
     (binary-tree-create-aux l 0)))

(binary-tree-get: [b i]
  (let ((m (car b)))
    (tif (atom? (cdr b))
         (cdr b)
         (tif (< i m)
              (recur (car (cdr b)) i)
              (recur (cdr (cdr b)) i)))))



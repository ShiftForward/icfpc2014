(progn
  (defvar nil -999)
  (defvar zero?
    (defun (x) (= x 0)))
  (defvar empty?
    (defun (li) (= li nil)))
  (defvar nth
    (defun (li n)
      (tif (zero? n)
           (car li)
           (recur (cdr li) (- n 1)))))
  (nth (cons 1 (cons 2 (cons 3 (cons 4 nil)))) 3))

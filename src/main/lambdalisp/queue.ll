(include prelude.ll)

(queue-create: []
  (cons nil nil))

(queue-length: [q]
  (+ (length (car q)) (length (cdr q))))

(queue-empty?: [q]
  (empty? (car q)))

(queue-checkf: [q]
  (if (empty? (car q))
      (cons (reverse (cdr q)) nil)
      q))

(queue-enqueue: [q v]
  (queue-checkf (cons (car q) (cons v (cdr q)))))

(queue-enqueue-all: [q l]
  (foldLeft q l [r v] (queue-enqueue r v)))

(queue-dequeue: [q]
  (queue-checkf (cons (cdr (car q)) (cdr q))))

(queue-front: [q]
  (car (car q)))

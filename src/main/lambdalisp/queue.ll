(include prelude.ll)

(queue-create: [] nil)

(queue-length: [q]
  (length q))

(queue-enqueue: [q v]
  (append q v))

(queue-dequeue: [q]
  (cdr q))

(queue-front: [q]
  (car q))



                

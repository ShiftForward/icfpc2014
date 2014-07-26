(include prelude.ll)

(queue-create: [] nil)

(queue-enqueue: [q v]
  (append q v))

(queue-dequeue: [q]
  (cdr q))

(queue-front: [q]
  (car q))



                

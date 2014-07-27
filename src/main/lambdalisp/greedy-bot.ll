(include prelude.ll)
(include queue.ll)
(include binary-tree.ll)

(convert-list: [l]
  (let ((convert-list-aux [l res]
          (tif (atom? l)
               (reverse res)
               (recur (cdr l) (cons (car l) res)))))
    (convert-list-aux l nil)))

(convert-matrix: [m]
  (let ((convert-matrix-aux [m res]
          (tif (atom? m)
               (reverse res)
               (recur (cdr m) (cons (convert-list (car m)) res)))))
    (convert-matrix-aux m nil)))

(get-matrix-pos: [matrix matrix-width pos]
  (binary-tree-get matrix (+ (car pos) (* matrix-width (cdr pos)))))

(set-matrix-pos: [matrix matrix-width pos v]
  (binary-tree-set matrix (+ (car pos) (* matrix-width (cdr pos))) v))

(bfs: [from to map map-width map-height]
  (let ((directions (list (cons 0 -1) (cons 1 0) (cons 0 1) (cons -1 0)))
        (update-values [matrix positions v]
          (foldLeft matrix positions [m pos]
            (set-matrix-pos m map-width pos v))))

    (let ((initial-details-matrix (update-values (binary-tree-create (flatten1 (fill map-height (fill map-width (cons false (cons -1 (cons -1 -1))))))) (list from) (cons true (cons 0 (cons -1 -1)))))
          (bfsaux [q details-matrix]

            (tif (queue-empty? q)
                 details-matrix

                 (let ((current (debug (queue-front q)))
                       (next-q (queue-dequeue q)))

                    (tif (and (= (car current) (car to)) (= (cdr current) (cdr to)))
                         details-matrix

                      (let ((current-dist (car (cdr (get-matrix-pos details-matrix map-width current))))
                            (neighbors (foldLeft (list) directions [next-steps direction]

                         (let ((next-pos (cons (+ (car current) (car direction)) (+ (cdr current) (cdr direction)))))

                           (tif (and (= (car (get-matrix-pos details-matrix map-width next-pos)) false)
                                     (not (= (get-matrix-pos map map-width next-pos) 0)))
                               (cons next-pos next-steps)
                               next-steps)))))

                        (recur (queue-enqueue-all next-q neighbors)
                               (update-values details-matrix neighbors (cons true (cons (+ current-dist 1) current))))))))))

      (bfsaux (queue-enqueue (queue-create) from)
               initial-details-matrix))))

(main: [state]
  (let* ((map (convert-matrix (car state)))
         (map-width (length (car map)))
         (map-height (length map))
         (lambdaman (car (cdr state)))
         (ghosts (car (cdr (cdr state))))
         (fruit (cdr (cdr (cdr state))))
         (location (car (cdr lambdaman)))
         (direction (car (cdr (cdr lambdaman)))))

      (progn
        (debug map-width)
        (debug map-height)
        (debug (bfs (cons 11 16) (cons 1 1) (binary-tree-create (flatten1 map)) map-width map-height))
        (cons 0 direction))))

(cons 0 main)

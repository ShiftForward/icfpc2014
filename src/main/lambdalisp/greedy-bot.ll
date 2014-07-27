(include prelude.ll)
(include queue.ll)

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

(bfs: [from to map]
  (let ((directions (list (cons 0 -1) (cons 1 0) (cons 0 1) (cons -1 0)))
        (map-width (length (car map)))
        (map-height (length map))
        (update-values [matrix positions v]
          (foldLeft matrix positions [m pos]
            (let ((i (cdr pos))
                  (j (car pos)))
              (set m i (set (nth m i) j v))))))

    (let ((initial-details-matrix (update-values (fill map-height (fill map-width (cons false (cons -1 (cons -1 -1))))) (list from) (cons true (cons 0 (cons -1 -1)))))
          (bfsaux [q details-matrix]

            (tif (queue-empty? q)
                 details-matrix

                 (let ((current (queue-front q))
                       (next-q (queue-dequeue q)))

                    (tif (and (= (car current) (car to)) (= (cdr current) (cdr to)))
                         details-matrix

                      (let ((current-dist (car (cdr (nnth details-matrix (cdr current) (car current)))))
                            (neighbors (foldLeft (list) directions [next-steps direction]

                         (let ((next-pos (cons (+ (car current) (car direction)) (+ (cdr current) (cdr direction)))))

                           (tif (and (= (car (nnth details-matrix (cdr next-pos) (car next-pos))) false)
                                     (not (= (nnth map (cdr next-pos) (car next-pos)) 0)))
                               (cons next-pos next-steps)
                               next-steps)))))

                        (recur (queue-enqueue-all next-q neighbors)
                               (update-values details-matrix neighbors (cons true (cons (+ current-dist 1) current))))))))))

      (bfsaux (queue-enqueue (queue-create) from)
              initial-details-matrix))))

(main: [state]
  (let ((map (convert-matrix (car state)))
        (lambdaman (car (cdr state)))
        (ghosts (car (cdr (cdr state))))
        (fruit (cdr (cdr (cdr state)))))
     (let ((location (car (cdr lambdaman)))
           (direction (car (cdr (cdr lambdaman)))))

      (progn
        (debug (nnth (bfs (cons 11 16) (cons 1 1) map) 1 1))
        (cons 0 direction)))))

(cons 0 main)

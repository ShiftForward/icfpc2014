(include prelude.ll)
(include queue.ll)
(include binary-tree.ll)
(include coord.ll)
(include game.ll)

(get-matrix-pos: [matrix matrix-width pos]
  (binary-tree-get matrix (+ (coord-x pos) (* matrix-width (coord-y pos)))))

(set-matrix-pos: [matrix matrix-width pos v]
  (binary-tree-set matrix (+ (coord-x pos) (* matrix-width (coord-y pos))) v))

(bfs-node: [visited distance previous]
  (cons visited (cons distance previous)))

(bfs-visited: [details-matrix width height pos]
  (car (get-matrix-pos details-matrix width pos)))

(bfs-distance: [details-matrix width height pos]
  (car (cdr (get-matrix-pos details-matrix width pos))))

(bfs-previous: [details-matrix width height pos]
  (cdr (cdr (get-matrix-pos details-matrix width pos))))

(bfs-get-path: [details-matrix width height from to]
  (let ((bfs-get-path-aux [from to]
          (if (coord-equal to from)
              nil
              (cons to (self from (bfs-previous details-matrix width height to))))))
    (reverse (bfs-get-path-aux from to))))

(direction-to: [from to]
  (let ((disp (coord-displacement from to)))
    (cond ((coord-equal disp (coord-create 0 -1)) (coord-create 0 0))
          ((coord-equal disp (coord-create 1 0)) (coord-create 0 1))
          ((coord-equal disp (coord-create 0 1)) (coord-create 0 2))
          (else (coord-create 0 3)))))

(bfs: [from to map map-width map-height]
  (let ((directions (list (coord-create 0 -1) (coord-create 1 0) (coord-create 0 1) (coord-create -1 0)))
        (update-values [matrix positions v]
          (foldLeft matrix positions [m pos]
            (set-matrix-pos m map-width pos v))))

    (let ((initial-details-matrix (update-values (binary-tree-create (flatten1 (fill map-height (fill map-width (cons false (cons -1 (cons -1 -1))))))) (list from) (bfs-node true 0 (coord-create -1 -1))))
          (bfsaux [q details-matrix]

            (tif (queue-empty? q)
                 details-matrix

                 (let ((current (queue-front q))
                       (next-q (queue-dequeue q)))

                    (tif (coord-equal current to)
                          details-matrix

                      (let ((current-dist (bfs-distance details-matrix map-width map-height current))
                            (neighbors (foldLeft (list) directions [next-steps direction]

                         (let ((next-pos (coord-sum current direction)))

                           (tif (and (= (bfs-visited details-matrix map-width map-height next-pos) false)
                                     (not (= (get-matrix-pos map map-width next-pos) 0)))
                               (cons next-pos next-steps)
                               next-steps)))))

                        (recur (queue-enqueue-all next-q neighbors)
                               (update-values details-matrix neighbors (bfs-node true (+ current-dist 1) current)))))))))

      (bfsaux (queue-enqueue (queue-create) from)
               initial-details-matrix))))

(get-path: [from to map map-width map-height]
  (bfs-get-path (bfs from to map map-width map-height) map-width map-height from to))

(next-checkpoint: nil)

(get-next-checkpoint: [location]
  (if (or (empty? next-checkpoint) (coord-equal next-checkpoint location))
      (progn
        (defvar next-checkpoint (car position-power-pills))
        (defvar position-power-pills (cdr position-power-pills))
        next-checkpoint)
      next-checkpoint))

(main: [state]
  (let* ((lambdaman (car (cdr state)))
         (ghosts (car (cdr (cdr state))))
         (fruit (cdr (cdr (cdr state))))
         (location (car (cdr lambdaman)))
         (direction (car (cdr (cdr lambdaman))))
         (binary-tree-map (binary-tree-create (flatten1 map)))
         (checkpoint (get-next-checkpoint location)))

      (progn
        (direction-to location (car (get-path location checkpoint binary-tree-map map-width map-height))))))

(progn
  (debug initial-state)
  (cons 0 main))

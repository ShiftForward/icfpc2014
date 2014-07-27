(include prelude.ll)
(include queue.ll)
(include binary-tree.ll)
(include coord.ll)
(include game.ll)
(include heap.ll)

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

(bfs: [from to game-map map-width map-height]
  (let ((directions (list (coord-create 0 -1) (coord-create 1 0) (coord-create 0 1) (coord-create -1 0)))
        (update-values [matrix positions v]
          (foldLeft matrix positions [m pos]
            (set-matrix-pos m map-width pos v))))

    (let ((initial-details-matrix (update-values (binary-tree-create (flatten1 (fill map-height (fill map-width (cons false (cons -1 (cons -1 -1))))))) (list from) (bfs-node true 0 (coord-create -1 -1))))
          (bfsaux [q details-matrix]

            (tif (heap-empty? q)
                 details-matrix

                 (let* ((current-element (heap-find-min q))
                        (current (cdr current-element))
                        (next-q (cdr (heap-pop q))))

                    (tif (coord-equal current to)
                          details-matrix

                      (let ((current-dist (bfs-distance details-matrix map-width map-height current))
                            (neighbors (foldLeft (list) directions [next-steps direction]

                         (let ((next-pos (coord-sum current direction)))

                           (tif (and (= (bfs-visited details-matrix map-width map-height next-pos) false)
                                     (not (= (get-matrix-pos game-map map-width next-pos) 0)))
                               (cons next-pos next-steps)
                               next-steps)))))

                        (recur (foldLeft next-q neighbors [acc e] (heap-insert (cons (+ (coord-manhattan-distance e to) (+ current-dist 1)) e) acc))
                               (update-values details-matrix neighbors (bfs-node true (+ current-dist 1) current)))))))))

      (bfsaux (heap-from-list (list (cons 0 from)))
               initial-details-matrix))))

(get-path: [from to game-map map-width map-height]
  (bfs-get-path (bfs from to game-map map-width map-height) map-width map-height from to))

(next-checkpoint: nil)
(checkpoints: (heap-create))

(get-checkpoints: [state]
  (progn 
    (defvar checkpoints (heap-create))
    (let* ((game-map (state-map state))
           (row-update [row x y]
             (tif (empty? row)
                  nil
                  (let ((cell (car row)))
                      (tcond ((= cell encoding-pill)
                              (progn (defvar checkpoints (heap-insert (cons (- 0 points-pill) (coord-create x y)) checkpoints))
                                     (recur (cdr row) (+ x 1) y)))
                             ((= cell encoding-power-pill)
                              (progn (defvar checkpoints (heap-insert (cons (- 0 points-power-pill) (coord-create x y)) checkpoints))
                                     (recur (cdr row) (+ x 1) y)))
                             ((and (state-fruit state) (= cell encoding-fruit))
                              (progn (defvar checkpoints (heap-insert (cons (- 0 points-fruit) (coord-create x y)) checkpoints))
                                     (recur (cdr row) (+ x 1) y)))
                             (else (recur (cdr row) (+ x 1) y))))))
           (col-update [rows x y]
             (tif (empty? rows)
                  nil
                  (progn
                    (row-update (car rows) 0 y)
                    (recur (cdr rows) 0 (+ y 1))))))
      (col-update game-map 0 0))))

(get-next-checkpoint: [location state]
  (if (or (empty? next-checkpoint) (coord-equal next-checkpoint location))
      (progn
        (get-checkpoints state)
        (defvar next-checkpoint (cdr (heap-find-min checkpoints)))
        next-checkpoint)
      next-checkpoint))

(main: [state]
  (let* ((lambdaman (car (cdr state)))
         (ghosts (car (cdr (cdr state))))
         (fruit (cdr (cdr (cdr state))))
         (location (car (cdr lambdaman)))
         (direction (car (cdr (cdr lambdaman))))
         (binary-tree-map (binary-tree-create (flatten1 game-map)))
         (checkpoint (get-next-checkpoint location state)))

      (progn
        (direction-to location (car (get-path location checkpoint binary-tree-map map-width map-height))))))

(progn
  (cons 0 main))

(include prelude.ll)
(include queue.ll)
(include binary-tree.ll)
(include coord.ll)
(include game.ll)
(include heap.ll)
(include random.ll)

(get-matrix-pos: [matrix matrix-width pos]
  (binary-tree-get matrix (+ (coord-x pos) (* matrix-width (coord-y pos)))))

(set-matrix-pos: [matrix matrix-width pos v]
  (binary-tree-set matrix (+ (coord-x pos) (* matrix-width (coord-y pos))) v))

(astar-node: [visited distance previous]
  (cons visited (cons distance previous)))

(astar-visited: [details-matrix width height pos]
  (car (get-matrix-pos details-matrix width pos)))

(astar-distance: [details-matrix width height pos]
  (car (cdr (get-matrix-pos details-matrix width pos))))

(astar-previous: [details-matrix width height pos]
  (cdr (cdr (get-matrix-pos details-matrix width pos))))

(astar-get-path: [details-matrix width height from to]
    (let ((astar-get-path-aux [from to]
          (if (coord-equal to from)
              nil
              (cons to (self from (astar-previous details-matrix width height to))))))
      (reverse (astar-get-path-aux from to))))

(direction-to: [from to]
  (let ((disp (coord-displacement from to)))
    (cond ((coord-equal disp (coord-create 0 -1)) (coord-create 0 0))
          ((coord-equal disp (coord-create 1 0)) (coord-create 0 1))
          ((coord-equal disp (coord-create 0 1)) (coord-create 0 2))
          (else (coord-create 0 3)))))

(update-values: [matrix positions v]
  (foldLeft matrix positions [m pos]
            (set-matrix-pos m map-width pos v)))

(astar: [from to game-map map-width map-height state]
  (let* ((directions (list (coord-create 0 -1) (coord-create 1 0) (coord-create 0 1) (coord-create -1 0)))
         (ghosts (state-ghosts state))
         (fright-mode? (exists ghosts [x] (= (car x) 1)))
         (game-map (if fright-mode? game-map (update-values game-map (map ghosts [g] (cadr g)) 0))))
    (let (
          (initial-details-matrix (update-values (binary-tree-create (flatten1 (fill map-height (fill map-width (cons false (cons -1 (cons -1 -1))))))) (list from) (astar-node true 0 (coord-create -1 -1))))
          (astar-aux [q details-matrix]

            (tif (heap-empty? q)
                 details-matrix

                 (let* ((current-element (heap-find-min q))
                        (current (cdr current-element))
                        (next-q (cdr (heap-pop q))))

                    (tif (coord-equal current to)
                          details-matrix
                      (let ((current-dist (astar-distance details-matrix map-width map-height current))
                            (neighbors (foldLeft (list) directions [next-steps direction]

                         (let ((next-pos (coord-sum current direction)))

                           (tif (and (= (astar-visited details-matrix map-width map-height next-pos) false)
                                     (not (= (get-matrix-pos game-map map-width next-pos) 0)))
                               (cons next-pos next-steps)
                               next-steps)))))

                        (recur (foldLeft next-q neighbors [acc e] (heap-insert (cons (+ (coord-manhattan-distance e to) (+ current-dist 1)) e) acc))
                               (update-values details-matrix neighbors (astar-node true (+ current-dist 1) current)))))))))

      (astar-aux (heap-from-list (list (cons 0 from)))
                  initial-details-matrix))))

(get-path: [from to game-map map-width map-height state]
  (let ((astar-details (astar from to game-map map-width map-height state 600)))
    (if (= (astar-distance astar-details map-width map-height to) -1)
        nil
        (astar-get-path astar-details map-width map-height from to))))

(next-checkpoint: nil)
(checkpoints: (heap-create))

(get-checkpoints: [game-map state binary-tree-map]
  (progn 
    (defvar checkpoints (heap-create))
    (let* ((lambda-man (state-lambdaman state))
           (position (cadr lambda-man))
           (ghosts (state-ghosts state))
           (astar-details (astar position (coord-create -1 -1) binary-tree-map map-width map-height state))
           (row-update [row x y]
             (tif (empty? row)
                  nil
                  (let ((cell (car row))
                        (cell-dist (astar-distance astar-details map-width map-height (coord-create x y))))
                      (tcond ((and (= cell encoding-pill) (not (= cell-dist -1)))
                              (progn (defvar checkpoints (heap-insert (cons (- 0 (- points-pill cell-dist)) (coord-create x y)) checkpoints))
                                     (recur (cdr row) (+ x 1) y)))
                             ((and (not (state-fright-mode? state)) (and (= cell encoding-power-pill) (not (= cell-dist -1))))
                              (progn (defvar checkpoints (heap-insert (cons (- 0 (- points-power-pill cell-dist)) (coord-create x y)) checkpoints))
                                     (recur (cdr row) (+ x 1) y)))
                             ((and (and (> (state-fruit state) 0) (= cell encoding-fruit)) (not (= cell-dist -1)))
                              (progn (defvar checkpoints (heap-insert (cons (- 0 (- points-fruit cell-dist)) (coord-create x y)) checkpoints))
                                     (recur (cdr row) (+ x 1) y)))
                             (else (recur (cdr row) (+ x 1) y))))))
           (col-update [rows x y]
             (tif (empty? rows)
                  nil
                  (progn
                    (row-update (car rows) 0 y)
                    (recur (cdr rows) 0 (+ y 1))))))
      (progn 
        (col-update game-map 0 0)
        (foldLeft nil ghosts [acc x] (if (= (car x) 1) (defvar checkpoints (heap-insert (cons (- 0 (- points-ghosts (astar-distance astar-details map-width map-height (cadr x)))) (cadr x)) checkpoints)) nil))
))))

(get-next-checkpoint: [game-map location state binary-tree-map]
  (tif (or (empty? next-checkpoint) (coord-equal next-checkpoint location) (state-fright-mode? state))
      (progn
        (get-checkpoints game-map state binary-tree-map)
        (defvar next-checkpoint (cdr (heap-find-min checkpoints)))
        (cons next-checkpoint (get-path location next-checkpoint binary-tree-map map-width map-height state)))
      (let ((path (get-path location next-checkpoint binary-tree-map map-width map-height state)))
         (tif (empty? path)
             (progn
               (defvar next-checkpoint nil)
               (recur game-map location state binary-tree-map))
              (cons next-checkpoint path)))))

(map-to-dir: [coord]
  (cond ((coord-equal coord (coord-create 0 -1)) 0)
        ((coord-equal coord (coord-create 1 0)) 1)
        ((coord-equal coord (coord-create 0 1)) 2)
        ((coord-equal coord (coord-create -1 0)) 3)))

(seed: (foldLeft 0 (state-ghosts initial-state) [acc x] (+ acc (+ (coord-x (cadr x)) (coord-y (cadr x))))))

(main: [state]
  (tif (> map-area 576)
    (let* ((game-map (car state))
           (lambdaman (car (cdr state)))
           (location (car (cdr lambdaman)))
           (direction (car (cdr (cdr lambdaman))))
           (opposite-dir (cond ((= direction 0) (coord-create 0 1))
                               ((= direction 1) (coord-create -1 0))
                               ((= direction 2) (coord-create 0 -1))
                               ((= direction 3) (coord-create 1 0))))
           (directions (list (coord-create 0 -1) (coord-create 1 0) (coord-create 0 1) (coord-create -1 0)))
           (valid-directions (filter directions [x] (and (not (= (nnth game-map (cdr (coord-sum location x)) (car (coord-sum location x))) 0)) (not (coord-equal x opposite-dir)))))
           (valid-maps (map valid-directions [x] (map-to-dir x))))

        (if (empty? valid-maps)
            (cons 0 opposite-dir)
            (cons 0 (nth valid-maps (mod (random) (length valid-maps))))))
    (let* ((lambdaman (car (cdr state)))
           (location (car (cdr lambdaman)))
           (direction (car (cdr (cdr lambdaman))))
           (binary-tree-map (binary-tree-create (flatten1 game-map)))
           (checkpoint-and-path (get-next-checkpoint game-map location state binary-tree-map))
           (checkpoint (car checkpoint-and-path))
           (path (cdr checkpoint-and-path)))

        (progn
          (direction-to location (car path))))))

(progn
  (cons 0 main))



(include prelude.ll)
(include coord.ll)

(convert-list: [l]
  (if (atom? l)
      nil
      (cons (car l) (self (cdr l)))))

(convert-matrix: [m]
  (if (atom? m)
      nil
      (cons (convert-list (car m)) (self (cdr m)))))

(ghosts-eaten: 0)

(state-map: [state]
  (convert-matrix (car state)))
(state-fruit: [state]
  (cdddr state))
(state-lambdaman: [state]
  (cadr state))
(state-ghosts: [state]
  (convert-list (caddr state)))
(state-fright-mode?: [state]
  (exists (state-ghosts state) [x] (= (car x) 1)))

(game-map: (convert-matrix (state-map initial-state)))
(map-width: (length (car game-map)))
(map-height: (length game-map))
(map-area: (* map-width map-height))

(encoding-wall: 0)
(encoding-empty: 1)
(encoding-pill: 2)
(encoding-power-pill: 3)
(encoding-fruit: 4)
(encoding-lambda-man: 5)
(encoding-ghost: 6)

(points-pill: 10)
(points-power-pill: 50)
(points-ghosts: 200)
(points-fruit:
  (cond ((<= map-area 100) 100)
        ((and (< 100 map-area) (<= map-area 200)) 300)
        ((and (< 200 map-area) (<= map-area 300)) 500)
        ((and (< 300 map-area) (<= map-area 400)) 500)
        ((and (< 400 map-area) (<= map-area 500)) 700)
        ((and (< 500 map-area) (<= map-area 600)) 700)
        ((and (< 600 map-area) (<= map-area 700)) 1000)
        ((and (< 700 map-area) (<= map-area 800)) 1000)
        ((and (< 800 map-area) (<= map-area 900)) 2000)
        ((and (< 900 map-area) (<= map-area 1000)) 2000)
        ((and (< 1000 map-area) (<= map-area 1100)) 3000)
        ((and (< 1100 map-area) (<= map-area 1200)) 3000)
        (else 5000)))

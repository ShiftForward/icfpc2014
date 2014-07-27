(include prelude.ll)

(coord-create: [x y]
  (cons x y))

(coord-x: [coord]
  (car coord))

(coord-y: [coord]
  (cdr coord))

(coord-sum: [c1 c2]
  (coord-create (+ (coord-x c1) (coord-x c2)) (+ (coord-y c1) (coord-y c2))))

(coord-equal: [c1 c2]
  (and (= (coord-x c1) (coord-x c2)) (= (coord-y c1) (coord-y c2))))

(coord-displacement: [c1 c2]
  (coord-create (- (coord-x c2) (coord-x c1)) (- (coord-y c2) (coord-y c1))))

(coord-manhattan-distance: [c1 c2]
  (+ (abs (- (coord-x c1) (coord-x c2))) (abs (- (coord-y c1) (coord-y c2)))))

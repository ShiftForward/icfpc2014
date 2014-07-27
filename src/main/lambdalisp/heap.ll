(include prelude.ll)

; Leftist heap as per Okasaki
;
; Example: (heap-find-min (cdr (heap-pop (heap-insert 3 (heap-insert 2 (heap-insert 1 (heap-create)))))))
;
; This should return 2, since (a) we insert elements 1, 2 and 3, (b) we pop and thus get a tuple
; of the min element, followed by the rest of the heap, (c) we access the rest, and (d) we peek the
; first element.
;
; Example: (heap-find-min (heap-from-list (list 9 7 8 5 6 10)))
;
; This returns 5, since we (a) create an heap from an unordered list, and (b) return the first element.
;

; Accessors
(heap-get-rank:  [h] (car h))
(heap-get-elem:  [h] (cadr h))
(heap-get-subl:  [h] (caddr h))
(heap-get-subr:  [h] (cadddr h))
(heap-build: [rank elem subl subr] (list rank elem subl subr))

; Private inner workings
(heap-rank: [h]
	(tif (empty? h) 0
             (heap-get-rank h)))

(heap-make: [x a b]
	(tif (>= (heap-rank a) (heap-rank b))
		 (heap-build (+ (heap-rank b) 1) x a b) 
		 (heap-build (+ (heap-rank a) 1) x b a)))

(heap-merge: [h1 h2]
	(tcond
		((empty? h2) h1) 
		((empty? h1) h2)
		(else (let ((x  (heap-get-elem h1))
				    (y  (heap-get-elem h2))
				    (a1 (heap-get-subl h1))
				    (b1 (heap-get-subr h1))
				    (a2 (heap-get-subl h2))
				    (b2 (heap-get-subr h2)))
			       (tif (< (car x) (car y))
			       	    (heap-make x a1 (self b1 h2))
			       	    (heap-make y a2 (self h1 b2)))))))

; Public API
(heap-empty?: [h] (empty? h))

(heap-insert: [x h] (heap-merge (heap-build 1 x nil nil) h))

(heap-find-min: [h]
	(tif (heap-empty? h) nil
             (heap-get-elem h)))

(heap-delete-min: [h]
	(tif (heap-empty? h) nil
             (heap-merge (heap-get-subl h) (heap-get-subr h))))

(heap-pop: [h]
	(tif (heap-empty? h) nil
		 (let ((x (heap-get-elem h))
		       (r (heap-delete-min h)))
		 	  (cons x r))))

; Constructors
(heap-create: [] nil)
(heap-from-list: [l] (foldLeft (heap-create) l [acc e] (heap-insert e acc)))

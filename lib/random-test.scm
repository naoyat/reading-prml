;;
;; normal (Gaussian) distribution
;;
(use gauche.test)

(require "./random")

;;
;; Box-Muller法
;;
(test-start "Box-Muller")

(define (vector-inc! vec i . args)
  (let-optionals* args ((delta 1))
	(vector-set! vec i (+ (vector-ref vec i) delta))))
(define (vector-dec! vec i . args)
  (let-optionals* args ((delta 1))
	(vector-set! vec i (- (vector-ref vec i) delta))))

(define nd-rand (make-nd-rand 50 10))
(define cnt (make-vector 101 0))
(dotimes (i 1000000)
  (let1 x (round->exact (nd-rand))
	(when (<= 0 x 100)
	  (vector-inc! cnt x))
		))

(dotimes (i 101)
  (format #t "~3d: ~a\n" i (make-string (quotient (vector-ref cnt i) 500) #\*)))


(test-end)


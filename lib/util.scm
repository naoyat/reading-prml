(use math.const) ;; pi
(use math.mt-random) ;; Mersenne Twister

(define (interpolate x0 x1 t) ; t:[0..1], x0 when t=0, x1 when t=1
  (+ (* x0 (- 1 t)) (* x1 t)))

(define (square x) (* x x))

(define (gauss1 x mu sigma)
  (let1 sigma^2 (square sigma)
    (/ (exp (* -1 (/ (square (- x mu)) (* 2 sigma^2))))
       (/ 1 (sqrt (* 2 pi sigma^2))) )))

(define (gauss2 X Mu Sigma) ;; それぞれ vector
;  (format #t "Sigma: ~a\n" Sigma)
  (let* (;[_Sigma_ (determinant Sigma)] ;; 1.592
         [lmd (- (* (vector-ref Sigma 0) (vector-ref Sigma 3)) ;; 0.936
                 (* (vector-ref Sigma 1) (vector-ref Sigma 2)))]
         [xm0 (- (vector-ref X 0) (vector-ref Mu 0))]
         [xm1 (- (vector-ref X 1) (vector-ref Mu 1))]
         [mahalanobis^2 (/ (+ (* xm0 xm0 (vector-ref Sigma 3))
							  (* -1 xm0 xm1 (+ (vector-ref Sigma 1)
											   (vector-ref Sigma 2)))
							  (* xm1 xm1 (vector-ref Sigma 0)) )
						   lmd)])
    (/ (exp (* -1/2 mahalanobis^2))
       (* pi 2 (sqrt lmd)) )))

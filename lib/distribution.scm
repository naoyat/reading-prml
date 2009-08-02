(use c-wrapper) ; ガンマ関数をlibmから
(c-load "math.h" :import 'tgamma)
(define Γ tgamma)

(use math.const) ;; pi
(use math.mt-random) ;; Mersenne Twister

;;;;;;;;;
;;
;; ベルヌーイ分布 Bernoulli distribution
;;
(define (make-Bernoulli-distribution µ)
  (let ([E_const µ]
		[var_const (* µ (- 1 µ))])

	(define (p x) ; p(x|µ) ... (2.1)
	  (cond [(= x 1) µ]
			[(= x 0) (- 1 µ)]
			;[else #<undefined>]
			))
	(define (Bern x) ; Bern(x|µ) ... (2.2)
	  (* (expt µ x) (expt (- 1 µ) (- 1 x))))

	(define (E x) E_const) ; ... (2.3)
	(define (var x) var_const) ; ... (2.4)

	(define (likelihood D) ; p(D|µ) ... (2.5)
	  (Π (map Bern D)))
	(define (ln-likelihood D) ; ln p(D|µ) ... (2.6)
	  (Σ (map (lambda (x_n) (+ (* x_n (log µ))
								(* (- 1 x_n) (log (- 1 µ))))))))
	(define (µML D) ; ... (2.7)
	  (/ (Σ D) (length D)))
	
	(lambda (m)
	  (case m
		[(p Bern) p];Bern]
		[(E) E]
		[(var) var]
		[(likelihood) likelihood]
		[(µML) µML]
		))))

;;
;; 二項分布 binomial distribution
;;
(define (factorial n)
  (let loop ((n n) (prod 1))
	(if (= n 0) prod (loop (- n 1) (* prod n)))))
(define (C n m) (/ (factorial n) (factorial (- n m)) (factorial m)))

(define (make-binomial-distribution N µ)
  (let ([E_const (* N µ)]
		[var_const (* N µ (- 1 µ))])
	(define (Bin m) (* (C N m) (expt µ m) (expt (- 1 µ) (- N m))))
	(define (E m) E_const)
	(define (var m) var_const)

	(lambda (m)
	  (case m
		[(p Bin) Bin]
		[(E) E]
		[(var) var]
		))))

;;
;; ベータ分布 beta distribution
;;
(define (make-beta-distribution a b)
;  (define (expt* x y) (if (= x 0) 0 (expt x y)))
  (let ([g (/ (Γ (+ a b)) (Γ a) (Γ b))]
		[E_const (/ a (+ a b))]
		[var_const (/ (* a b) (+ a b) (+ a b) (+ a b 1))])
	(define (Beta µ) (* g
						 (or (inf-filter (expt µ (- a 1))) 1)
						 (or (inf-filter (expt (- 1 µ) (- b 1))) 1) ))
	(define (E µ) E_const)
	(define (var µ) var_const)
	(lambda (m)
	  (case m
		[(p Beta) Beta]
		[(E) E]
		[(var) var]
		))))

;;
;; 多項分布 multinomial distribution
;;
(define (make-multinomial-distribution µs N) ;; µs = (µ1 ... µk)T
  (define (Mult ms) ;; ... (2.34)
	(* (/ (factorial N) (Π (map factorial ms))) ;; 2.35
	   (Π (map expt µs ms))))
  (lambda (m)
	(case m
	  [(p Mult) Mult]
;	  [(E) E]
;	  [(var) var]
	  )))

;;
;; ディリクレ分布 Dirichlet distribution
;;
(define (make-Dirichlet-distribution αs) ;; αs = (α1 .... αk)T
  (let1 g (/ (Γ (Σ αs)) (Π (map Γ αs)))
	(define (Dir µs) ;; µs = (µ1 ... µk)
	  (* g (Π (map (lambda (µ α) (expt µ (- α 1))) µs αs))))
    (lambda (m)
	  (case m
		[(p Dir) Dir]
;	  [(E) E]
;	  [(var) var]
		))))


;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

;;
;; Gaussian distributions (deprecated)
;;
(define (gauss1 x mu sigma)
  (let1 sigma^2 (square sigma)
    (/ (exp (* -1 (/ (square (- x mu)) (* 2 sigma^2))))
       (/ 1 (sqrt (* 2 pi sigma^2))) )))

(define (gauss2 X Mu Sigma) ;; それぞれ vector
  (let* ([lmd (- (* (vector-ref Sigma 0) (vector-ref Sigma 3)) ;; 0.936
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

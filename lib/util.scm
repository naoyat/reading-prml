;;
;; x^2
;;
(define (square x) (* x x))

;;
;; Σ, Π
;; 
(define Σ (lambda (lis) (apply + lis)))
(define Π (lambda (lis) (apply * lis)))

;;
;; t:[0..1], x0 when t=0, x1 when t=1
;;
(define (interpolate x0 x1 t) (+ (* x0 (- 1 t)) (* x1 t)))

(define *maxnum* 999999)
(define *minnum* -999999)

;; グラフ化する際に±∞,±NaN をカットするための関数
;(define (inf-filter x) (clamp x -99999 99999))
(define (inf-filter x)
  (cond [(not (real? x)) #f]
		[(= x +inf.0) *maxnum*]
		[(= x -inf.0) *minnum*]
;		[(= x +nan.0) #f]
;		[(= x -nan.0) #f]
		[else x]))

;;
;; メモ化
;;
(define (memoize proc)
  (let1 fh (make-hash-table 'equal?)
	(lambda args
	  (or (hash-table-get fh args #f)
		  (let1 val (apply proc args)
			(hash-table-put! fh args val)
			val)))
	))

;;
;; forマクロ
;;
(define-macro (for from to step proc)
  (let ([i (gensym)] [v (gensym)])
	`(do ((,v (if #f #f))
		  (,i ,from (+ ,i ,step)))
		 ((> ,i ,to) ,v)
	   (set! ,v (,proc ,i)))))

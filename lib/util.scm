;;
;; x^2
;;
(define (square x) (* x x))

;;
;; n!
;;
(define (factorial n)
  (let loop ((n n) (prod 1))
    (if (= n 0) prod (loop (- n 1) (* prod n)))))

;;
;; nCm
;;
(define (C n m) (/ (factorial n) (factorial (- n m)) (factorial m)))

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
(define-macro (for var from to step . body)
  ;; for (var=from; var<=to; var+=step) { body ...; }
  (let ((val (gensym))
		(op (gensym)))
	`(do ((,val (if #f #f))
		  (,var ,from (+ ,var ,step))
		  (,op (if (< 0 ,step) > <)))
		 ((,op ,var ,to) ,val)
	   (set! ,val (begin ,@body)))))

;(print(%macroexpand (for i 0 10 2 (print i))))
#|
(for i 0 10 2 (print i))
  => (do ((G0 (if #f #f))
          (i 0 (+ i 2))
          (G1 >))
         ((> i 10) G0)
       (set! G0 (begin (print i))))
|#

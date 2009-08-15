(require "./lib/random")
(require "./lib/util")
(require "./lib/gd-graph")
(require "./lib/mac")
(require "./lib/matrix")

;; [0..1]の範囲で均一に分布する乱数
(define rand-0to1 (make-uniform-rand 0 1))
;; μ=0, σ=1 なガウス分布乱数
(define rand-gauss-m0-s1 (make-nd-rand 0 1))

;; サンプルデータ生成関数。
;; sin(2πx) + ガウス分布に従う小さなランダムノイズ
(define (gen)
  (let* ([x (rand-0to1)]
		 [random-noise (* 0.25 (rand-gauss-m0-s1))]
		 [t (+ (sin (* 2 pi x)) random-noise)])
	(values x t)))

(define (generate-data gen N)
  (let loop ((i N) (data '()))
	(if (zero? i) (reverse! data)
		(receive (x t) (gen)
		  (loop (- i 1) (cons (cons x t) data))))))

(define D (generate-data gen 10)) ; ((x0 . t0) (x1 . t1) ... (x_{n-1} . t_{n-1}))

(define (plot-data-points im f x0 xF y0 yF obj)
  (define (x_ x) (round->exact (interpolate x0 xF x)))
  (define (y_ y) (round->exact (interpolate y0 yF (/ (+ y 2) 4))))
  (let1 black [obj'black]
	(for-each (lambda (xt)
				(let ([x (x_ (car xt))]
					  [y (y_ (cdr xt))])
				  (gd-image-arc im x y 7 7 0 360 black) ))
			  D)))

(sys-system "rm curve*.png")

(make-graph-png "curve.png" '(0 1 1) '(-2 2 1) (lambda (x) (sin (* 2 pi x))) plot-data-points)
(open-image "curve.png")

;; 多項式曲線フィッティング
(define (curve-fitting D M)
  (let* ([N (length D)] [M+1 (+ M 1)]
		 [x_ (map car D)]
		 [t_ (map cdr D)] [T (%t t_)]
		 [B (%0 M+1 N)]
		 [A (%0 M+1 M+1)])
	;; n=[0..N), i=[0..M], B_{ni} = x_n^i
	(dotimes (n N)
	  (let1 xn (list-ref x_ n)
		(let loop ((i 0) (xn^i 1))
		  (when (<= i M)
			(array-set! B i n xn^i)
			(loop (+ i 1) (* xn^i xn)) ))))
	;; i,j=[0..M], A_{ij} = \sum_{n=0}^{N-1} x_n^{i+j}
	(dotimes (i M+1)
	  (dotimes (j M+1)
		(array-set! A i j (let loop ((n 0) (sum 0))
							(if (= n N) sum
								(loop (+ n 1) (+ sum (* (array-ref B i n) (array-ref B j n)))))))))
	;; A w_ = B T
	;;   w_ = A-1 B T
	(%*% (%-1 A) B T)))

;; パラメータ(ベクトル)wから、1引数関数yを生成
(define (make-y w) ; y(x|w) = Σ w_i * x^i
  (let1 M+1 (vector-length w)
	(lambda (x)
	  (let loop ((i 0) (x^i 1) (sum 0))
		(if (= i M+1) sum
			(loop (+ i 1) (* x^i x) (+ sum (* (vector-ref w i) x^i))))))))

;; 次数M = 0..9 で曲線フィッティング
(dotimes (M 10)
  (format #t "[M=~d] " M)
  (let1 w (array->vector (curve-fitting D M))
	(print "w:"  w)
	(make-graph-png (format #f "curve_~d.png" M) '(0 1 1) '(-2 2 1) (make-y w) ; plot-data-points)
								(lambda (im f x0 xF y0 yF obj)
								  (let ([black [obj'black]]
										[green ([obj'color] 128 224 128)])
									(plot-data-points im f x0 xF y0 yF obj)
									([obj'plot-curve] (lambda (x) (sin (* 2 pi x))) green)
									(gd-image-string im f (+ x0 15) (+ yF 7) (format #f "M = ~a" M) black))))))

;; グラフを表示する
(open-image "curve_*.png")


;; 多項式曲線フィッティング（ペナルティ付き）
(define (curve-fitting-with-penalty D M ln-lmd)
  (let* ([N (length D)] [M+1 (+ M 1)]
		 [x_ (map car D)]
		 [t_ (map cdr D)] [T (%t t_)]
		 [B (%0 M+1 N)]
		 [A (%0 M+1 M+1)]
		 [L (%diag (exp ln-lmd) (+ M 1))])
	;; n=[0..N), i=[0..M], B_{ni} = x_n^i
	(dotimes (n N)
	  (let1 xn (list-ref x_ n)
		(let loop ((i 0) (xn^i 1))
		  (when (<= i M)
			(array-set! B i n xn^i)
			(loop (+ i 1) (* xn^i xn)) ))))
	;; i,j=[0..M], A_{ij} = \sum_{n=0}^{N-1} x_n^{i+j}
	(dotimes (i M+1)
	  (dotimes (j M+1)
		(array-set! A i j (let loop ((n 0) (sum 0))
							(if (= n N) sum
								(loop (+ n 1) (+ sum (* (array-ref B i n) (array-ref B j n)))))))))
	;; (A+λI) w_ = B T
	;;         w_ = (A+λI)-1 B T
	(%*% (%-1 (%- A L)) B T)))

(let1 M 9
  (for-each (lambda (ln-lmd)
			  (format #t "[M=~d, lnλ =~a] " M ln-lmd)
			  (let1 w (array->vector (curve-fitting-with-penalty D M ln-lmd))
				(print "w:"  w)
				(make-graph-png (format #f "curve_~d_~a.png" M ln-lmd) '(0 1 1) '(-2 2 1) (make-y w)
								(lambda (im f x0 xF y0 yF obj)
								  (let ([black [obj'black]]
										[green ([obj'color] 128 224 128)])
									(plot-data-points im f x0 xF y0 yF obj)
									([obj'plot-curve] (lambda (x) (sin (* 2 pi x))) green)
									(gd-image-string+ im f (+ x0 15) (+ yF 7) (format #f "M = ~a" M) black)
									(gd-image-string+ im f (+ x0 15) (+ yF 21) (format #f "lnλ = ~a" ln-lmd) black))))))
			'(-inf.0 -24 -18 -12 -6 -3 0)))

;; グラフを表示する
(open-image "curve_*_*.png")

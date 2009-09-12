(require "./lib/neural-net-matrix")

(require "./lib/gd-graph")
(require "./lib/mac")

(define nn (make-nn-simple 0.05 (make-nd-rand 0 1) 1 8 8 1))
;(define nn (make-nn-simple 0.05 (lambda() 0.1) 1 5 1))

(define mode 'each)
(define mode 'mille)
;(define mode 'four)

(case mode
  [(each)
   (set! *debug* #t)
   ([nn'learn] '(0) '(0))
   ([nn'learn] '(1/2) '(1))
   ([nn'learn] '(1) '(0))
   ]
  [(four)
   (set! *debug* #t)
   ([nn'learn] '(0) '(1))
   ([nn'learn] '(1) '(0))
   ]
  [(mille)
   (let* ([x/ (iota 51 0 1/50)]
		  [t/ (map (lambda (x) (sin (* 2 pi x))) x/)])
	 (set! *debug* #f)
	 (dotimes (i 1000)
;	 (set! *debug* (zero? (remainder i 50)))
	   (for-each (lambda (x t)
				   ;(format #t "learning (~a)=>(~a))...\n" (*. x) t)
				   ([nn'learn] (list x) (list t)) )
				 x/ t/)

	   (when (zero? (remainder i 10))
		 (let1 imgfile (format "nntest_~6,'0d.jpg" i)
		   (make-graph-image imgfile '(0 1 0.5) '(-1 1 1) (lambda (x) (car ([nn'test] (list x))))
							 (lambda (im f x0 xF y0 yF o)
							   ([o'plot-curve] (lambda (x) (sin (* 2 pi x))) ([o'color] 0 255 0)) )
							 'jpg)
		   (format #t "[~d]" i) (flush)
		   ;(open-image imgfile)
		   ))))
   ;(open-image "nntest_*.jpg")
   (sys-system "rm -f nntest.mpeg")
   (sys-system "ffmpeg -i nntest_00%03d0.jpg nntest.mpeg")
   (open-image "nntest.mpeg")
   ])

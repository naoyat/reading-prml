(require "./lib/neural-net-matrix")

;(require "./lib/gd-graph")
;(require "./lib/mac")
;(require "./lib/gl")

(define nn (make-nn-simple 0.05 (make-nd-rand 0 1) 2 2 1))
;(define nn (make-nn-simple 0.05 (lambda() 0.1) 1 5 1))

(define mode 'each)
(define mode 'mille)
;(define mode 'four)

(case mode
  [(each)
   (set! *debug* #t)
   ([nn'learn] '(0 0) '(0))
   ([nn'learn] '(1 1) '(0))
   ([nn'learn] '(1 0) '(1))
   ([nn'learn] '(0 1) '(1))
   ]
#;  [(four)
   (set! *debug* #t)
   ([nn'learn] '(0) '(1))
   ([nn'learn] '(1) '(0))
   ]
  [(mille)
   (set! *debug* #f)
   (dotimes (i 1000)
	 ([nn'learn] '(0 0) '(0))
	 ([nn'learn] '(1 1) '(0))
	 ([nn'learn] '(1 0) '(1))
	 ([nn'learn] '(0 1) '(1))

	 (when (zero? (remainder i 10))
	   (format #t "[~d] ~d ~d ~d ~d\n" i
			   ([nn'test] '(0 0))
			   ([nn'test] '(0 1))
			   ([nn'test] '(1 0))
			   ([nn'test] '(1 1)) ))
	 )])


(use gauche.array)
(use srfi-1)

(define *undef* (if #f #f))

(define (%rep elem len) (make-list len elem))
(define (make-list-with-generator len gen)
  (let loop ((i len) (res '()))
	(if (zero? i) (reverse! res)
		(loop (- i 1) (cons (gen) res)))))

(use math.mt-random)
(define *mt* (make <mersenne-twister> :seed (sys-time)))

(define (make-random-int-list len min max)
  (define (rnd) (+ min (mt-random-integer *mt* (+ (- max min) 1))))
  (make-list-with-generator len rnd))

(define (make-random-matrix nrow ncol min max)
  (define (rnd) (+ min (mt-random-integer *mt* (+ (- max min) 1))))
  (%matrix (make-random-int-list (* nrow ncol) min max) :ncol ncol :nrow nrow))
;;;
;;; gauche.array
;;;
(define %t array-transpose)
(define %solve array-inverse) ; 逆行列
;(define %-1 array-inverse)
(define %Det determinant) ; 行列式 |A|

(define (%base0->1 m0)
  (apply array (shape 1 (+ 1 (array-length m0 0))
					  1 (+ 1 (array-length m0 1)))
		 (array->list m0) ))

(define (%-1 mx)
  (let1 mx-1 (array-inverse mx)
	(if mx-1 (%base0->1 mx-1) #f)))

;; array-rotate-90
;; array-flip
;; identity-array

;;
(define (%* . ms) ;; array-mul を３つ以上の行列に拡張
  (let loop ((rest (cdr ms)) (prod (car ms)))
	(if (null? rest) (%base0->1 prod)
		(loop (cdr rest) (array-mul prod (car rest))))))
(define %*% %*)
(define %^ array-expt)

(define %+ array-add-elements)
(define %+! array-add-elements!)
(define %- array-sub-elements)
(define %-! array-sub-elements!)
(define %* array-mul-elements)
(define %*! array-mul-elements!)
(define %/ array-div-elements)
(define %/! array-div-elements!)


;;
;; let-keyword の restvar で得られる残り引数が元の順番と異なる（２個ずつのペアで逆順）ので
;; キーワード部以前の引数を元の順番で得たいというニーズに応える関数を作った
;;
(define (omit-keywords options)
  (let loop ((orig options) (dest '()))
	(if (null? orig)
		(values (reverse! dest) '())
		(if (keyword? (car orig))
			(values (reverse! dest) orig)
			(loop (cdr orig) (cons (car orig) dest))))))

(define (matrix-print mx)
  (let ([start0 (array-start mx 0)] [end0 (array-end mx 0)] [length0 (array-length mx 0)]
		[start1 (array-start mx 1)] [end1 (array-end mx 1)] [length1 (array-length mx 1)]
		)
	(for-each (lambda (y)
				(for-each (lambda (x)
							(format #t " ~a" (array-ref mx y x))
							)
						  (iota length1 start1))
				(newline))
			  (iota length0 start0))
	))

(define pp matrix-print)

(define (make-list* len fill)
;; fillがアトムの場合、SRFI-1の(make-list)と同じ振舞い
;; fillがリストの場合、fillのパターンを繰り返して埋めた長さlenのリストを生成
  (if (pair? fill)
	  (let loop ((i len) (l '()) (items fill))
		(if (zero? i) (reverse! l)
			(loop (- i 1)
				  (cons (car items) l)
				  (if (null? (cdr items)) fill (cdr items)))))
	  (make-list len fill)))
  
(define (%matrix x . options)
  (unless (pair? x) (set! x (list x))) ; xがatomの場合リスト化
  (let1 itemcnt (length x)
	(receive (args keywords) (omit-keywords options)
	  (let-keywords keywords ((nrow #f) (nr #f)
							  (ncol #f) (nc #f)
							  (byrow #f) (b *undef*) ; デフォルトで行主導。arrayでは列主導
							  (base 1))
;;							. rest)
		(when nr (set! nrow nr))
		(when nc (set! ncol nc))
		(unless (undefined? b) (set! byrow b))
		(let-optionals* args ((nrow_ #f)
							  (ncol_ #f)
							  (byrow_ *undef*))
		  (when nrow_ (set! nrow nrow_))
		  (when ncol_ (set! ncol ncol_))
		  (unless (undefined? byrow_) (set! byrow byrow_)))
		(cond [(and nrow ncol) #t]
			  [(and nrow (not ncol))
			   (set! ncol (/ itemcnt nrow))]
			  [(and ncol (not nrow))
			   (set! nrow (/ itemcnt ncol))]
			  [else (set! nrow 0) (set! ncol 0)])
;;	  (format #t "(matrix ~a nrow:~d ncol:~d byrow:~d base:~d)\n" x nrow ncol byrow base)

		(unless (= itemcnt (* nrow ncol))
		  (set! x (make-list* (* nrow ncol) x)))
		(if byrow
			(apply array (shape base (+ base nrow) base (+ base ncol)) x)
			(array-transpose
			 (apply array (shape base (+ base ncol) base (+ base nrow)) x)) )
		))))

(define (%array x s . options)
  ;;array は必ず列主導(byrow=#f)で要素が並べられる
  (let ((nrow (car s))
		(ncol (cadr s)))
	(let-keywords options ((base 1))
	  ;;	  (format #t "(matrix ~a nrow:~d ncol:~d byrow:F base:~d)\n" x nrow ncol base)
	  (array-transpose
	   (apply array (shape base (+ base ncol) base (+ base nrow)) x)) )))


(define (%rbind . rows)
  (let1 base 1
	(let ((nrow (length rows))
		  (ncol (length (car rows))))
	  (format #t ">> rbind(nrow:~d, ncol:~d, base:~d\n" nrow ncol base)
	  (apply array (shape base (+ base nrow) base (+ base ncol))
			 (apply append rows)))))

(define (%cbind . rows)
  (let1 base 1
	(let ((nrow (length rows))
		  (ncol (length (car rows))))
	  (format #t ">> rbind(nrow:~d, ncol:~d, base:~d\n" nrow ncol base)
	  (array-transpose (apply array (shape base (+ base ncol) base (+ base nrow))
							  (apply append rows))))))

(define (%dim mx) (list (array-length mx 0) (array-length mx 1)))
(define (%nrow mx) (array-length mx 0))
(define (%ncol mx) (array-length mx 1))

(define (matrix? obj) (and (array? obj) (= 2 (array-rank obj))))

(define (%set-diag! mx elems)
  (unless (pair? elems) (set! elems (list elems)))
  (let* ([base 1]
		 [start base]
		 [end (min (array-length mx 0) (array-length mx 1))])
	(let loop ((i start) (el elems))
	  (if (> i end) mx
		  (begin (array-set! mx i i (car el))
				 (loop (+ i 1) (if (null? (cdr el)) elems (cdr el))) )))))

(define (%diag . options)
;  (when (odd? (length options))
;	(set! options (append options (list :dummy))))
  (define (make-diag nrow ncol elems)
;	(format #t "(make-diag nrow:~d ncol:~d elems:~a)\n" nrow ncol elems)
	(let* ([base 1]
		   [start base]
		   [end (min nrow ncol)])
	  (let1 mx (make-array (shape base (+ base nrow) base (+ base ncol)) 0)
		(%set-diag! mx elems))))
  (define (diag-elements mx)
	(let ([start (min (array-start mx 0) (array-start mx 1))]
		  [end (min (array-end mx 0) (array-end mx 1))])
	  (let loop ((i start) (elems '()))
		(if (= end i) (reverse! elems)
			(loop (+ i 1) (cons (array-ref mx i i) elems))))))

  (if (matrix? (car options))
	  (diag-elements (car options))
	  (receive (args keywords) (omit-keywords options)
		(let-keywords keywords ((nrow #f) (nr #f)
								(ncol #f) (nc #f)
								(base 1))
					  ;;(format #t "(diag nrow:~d ncol:~d rest:~a)\n" nrow ncol args))
		  (case (length args)
			[(0) '?]
			[(1) ; (dim)
			 (cond [(number? (car args)) ;; N次元の単位行列
					(let1 dim (car args)
					  (if (zero? base)
						  (identity-array dim)
						  (make-diag (or nrow dim) (or ncol dim) '(1))
						  ))]
				   [(pair? (car args)) ;;(length (car args))次元の対角行列
					(let1 dim (length (car args))
					  (make-diag (or nrow dim) (or ncol dim) (car args))
					  )]
				   [else #f]) ]
			[(2 3) ; (elem dim)
			 (cond [(number? (first args)) ;; dim次元の単位行列xelem
					(let ([nrow (second args)]
						  [ncol (last args)])
					  (let1 elems (make-list (min nrow ncol) (first args))
						(make-diag (or nrow dim) (or ncol dim) elems)))]
				   [(pair? (first args)) ;;dim次元の対角行列
					(let ([elems (first args)]
						  [nrow (second args)]
						  [ncol (last args)])
					  (make-diag (or nrow dim) (or ncol dim) elems) )]
				   [else #f]) ]
			[else #\?])
		  ))))

;; トレース
(define (%Tr mx) (apply + (%diag mx)))

;; 行列式
;(define (%Det mx)
  

(define (%upper-tri mx . options)
  (let-keyword options ((diag #f))
			   '()))
			   
(define (%lower-tri mx . options)
  (let-keyword options ((diag #f))
			   '()))

(define (%set! mx my) ; mx <- my
  )


;;; ムーア-ペンローズの疑似逆行列
;;; (Moore-Penrose pseudo-inverse matrix)
(define (%moore-penrose phi) ;; (3.17)Φ†
  (let1 phiT (%t phi)
	(%*% (%-1 (%*% phiT phi) phiT))))

(define %† %moore-penrose)

(require "./lib/gl")
(require "./lib/random")
(require "./lib/util")
(require "./lib/distribution")

(use srfi-1)
(use gauche.uvector)
(use gauche.array)

;(use math.const)
;(use math.mt-random) ;; Mersenne Twister

(define xscale 1.0)
(define yscale 1.0)
(define zscale 1.0)

(define xmag (* xscale 0.9))
(define ymag (* yscale 0.9))
(define zmag (* zscale 0.9))

(define NDIV 50)
(set! *color-scale* 1.414213562)
(set! *draw-mode* GL_LINE_LOOP)
;(define *draw-mode* GL_POLYGON)

(define sigma 0.2)
(define beta (square (/ 1 sigma)))
(define alpha 2.0)

(define w #(-0.3 0.5))
(define (f_ x) (+ (vector-ref w 0) (* (vector-ref w 1) x)))

(define REP 100)
(define nd (make-nd-rand 0 0.2))
(define urand (make-uniform-rand -1 1))
(define xs_stack (map (lambda _ (urand)) (iota REP)))
(define ts_stack (map (lambda (x) (+ (f_ x) (nd))) xs_stack))
(define xs '())
(define ts '())

;(define m_0 (vector 0 0))
(define m_0 #,(<array> (0 2 0 1) 0 0))
(define S_0 (array-mul-elements (identity-array 2) (/ alpha)))
(define S_0_vec (vector (array-ref S_0 0 0) (array-ref S_0 0 1)
						(array-ref S_0 1 0) (array-ref S_0 1 1) ))

(define mode 1)

;;
(define (on-display)
  (gl-clear GL_COLOR_BUFFER_BIT)

  (case mode
	[(0) ;; 尤度関数
	 (gl-color 0.7 0.7 0.7)
	 (draw-string -1 0 1.5 "Likelihood")
	 (let* ([f (lambda (x y)
				 ((lambda (xn tn) (gauss1 tn (+ x (* y xn)) (/ beta)))
				  (car xs)
				  (car ts)))
			   ]
			[f* (memoize f)])
	   (dotimes (i NDIV)
		 (let* ([i0 (/ i NDIV)]
				[i1 (/ (+ i 1) NDIV)]
				[x0 (interpolate -1.0 1.0 i0)]
				[x1 (interpolate -1.0 1.0 i1)])
		   (dotimes (j NDIV)
			 (let* ([j0 (/ j NDIV)]
					[j1 (/ (+ j 1) NDIV)]
					[y0 (interpolate -1.0 1.0 j0)]
					[y1 (interpolate -1.0 1.0 j1)])
			   (draw-quads-f f* x0 y0 x1 y0 x1 y1 x0 y1)
			   )))))
	 (gl-color 0.7 0.7 0.7)
	 (draw-char (vector-ref w 0) (vector-ref w 1) 0 #\+)
	 ]
	[(1) ;; 事前/事後
	 (gl-color 0.7 0.7 0.7)
	 (draw-string -1 0 1.5 "Posterior")
	 (let* ([f (lambda (x y) (gauss2 (vector x y) (array->vector m_0) (array->vector S_0)))]
			[f* (memoize f)])
	   (dotimes (i NDIV)
		 (let* ([i0 (/ i NDIV)]
				[i1 (/ (+ i 1) NDIV)]
				[x0 (interpolate -1.0 1.0 i0)]
				[x1 (interpolate -1.0 1.0 i1)])
		   (dotimes (j NDIV)
			 (let* ([j0 (/ j NDIV)]
					[j1 (/ (+ j 1) NDIV)]
					[y0 (interpolate -1.0 1.0 j0)]
					[y1 (interpolate -1.0 1.0 j1)])
			   (draw-quads-f f*
							 x0 y0
							 x1 y0
							 x1 y1
							 x0 y1)
			   )))))
	 #;(gl-color 0.9 0.9 0.3)
	 #;(let1 nd2 (make-nd2-rand (array->vector m_0) S_0)
	   (dotimes (i 100)
		 (receive (x y) (nd2)
		   (draw-char x y 0 #\.))))
		   ;(format #t "  [nd2] (~a, ~a)\n" x y) )))
	 (gl-color 0.7 0.7 0.7)
	 (draw-char (vector-ref w 0) (vector-ref w 1) 0 #\+)
	 ]
	[(2) ;; データ空間
	 (gl-color 0.7 0.7 0.7)
	 (draw-string -1 0 1.5 "Data Space")
	 (gl-color 0.3 0.3 0.3)
	 (draw-line -1 -1 0 1 -1 0)
	 (draw-line -1 -1 0 -1 1 0)
	 (draw-line 1 1 0 1 -1 0)
	 (draw-line 1 1 0 -1 1 0)

	 (gl-color 0.9 0.15 0.15)
	 (let1 nd2 (make-nd2-rand (array->vector m_0) S_0)
	   (dotimes (i 6)
		 (receive (w0 w1) (nd2)
		   (let* ([d 1.2]
				  [y-d (- w0 (* w1 d))]
				  [y+d (+ w0 (* w1 d))])
			 (draw-line (- d) y-d 0
						d y+d 0)
			 ))))

	 (gl-color 0.15 0.15 0.9)
	 (let1 r 0.015
	   (for-each (cut draw-circle <> <> 0  r 0 0  0 r 0)
				 xs ts))
	 ])

  ;;; x-y-z軸
  (gl-color 0.3 0.3 0.3)
  ;; x軸
  (draw-line -1.5 0 0  1.5 0 0)
  (draw-line 1.45 -0.02 0  1.5 0 0) (draw-line 1.45 0.02 0  1.5 0 0)
  (draw-line -1 -0.02 0  -1 0.02 0)
  (draw-line  1 -0.02 0  1 0.02 0)
  (draw-string -1 -0.06 0 "-1")
  (draw-string 1 -0.06 0 "1")

  ;; y軸
  (draw-line 0 -1.5 0  0 1.5 0)
  (draw-line -0.02 1.45 0  0 1.5 0) (draw-line  0.02 1.45 0  0 1.5 0)
  (draw-line -0.02 -1 0  0.02 -1 0)
  (draw-line -0.02 1 0  0.02 1 0)
  (draw-string -0.08 -1 0 "-1")
  (draw-string -0.04 1 0 "1")

  ;; z軸
  (draw-line 0 0 0  0 0 1.4)
  (draw-line -0.02 0 1.35  0 0 1.4) (draw-line 0.02 0 1.35  0 0 1.4)
  (draw-line -0.02 0 1  0.02 0 1)

  (gl-flush))
  
(define (main args)
  (gl-main args))

(define (on-init window-title)
  (glut-init-display-mode GLUT_RGBA)
  (glut-init-window-size 400 400)
  (glut-init-window-position 10 10)
  (glut-create-window window-title)

  (gl-clear-color 0.0 0.0 0.0 0.0) ;;1.0 1.0 1.0 1.0))
  (gl-matrix-mode GL_PROJECTION)
  (gl-enable-client-state GL_VERTEX_ARRAY)
  (glu-perspective 60 1 0.1 20)

  (let1 theta (* pi 0.6)
    (glu-look-at 0.9 -1.8 1.8
;    (glu-look-at 0.75 -1.5 3.0
;    (glu-look-at 0 0 5
;    (glu-look-at 0 20 0
                 0 0 0
                 (cos theta) (sin theta) 0) )

  )

(define (on-keydown key x y)
  (when (= key 27) (exit 0))
;  (let1 ch (integer->char key)
  
; (set! mode (remainder (+ mode 1) 3))
  (case mode
	[(0) ; 0 -> 1
	 (set! mode 1)]
	[(1) ; 1 -> 2
	 (set! mode 2)]
	[(2) ; 2 -> 0
	 (let* ([x (pop! xs_stack)]
			[t (pop! ts_stack)]
			[Phi (array (shape 0 1 0 2) 1 x)];(identity-array 2)]; #,(<array> (0 2 0 2) 1 0 0 1)]
			[PhiT (array (shape 0 2 0 1) 1 x)];(identity-array 2)]; #,(<array> (0 2 0 2) 1 0 0 1)]
			;[PhiT (array-transpose Phi)]
			;[betaPhiT (array-mul-elements PhiT beta)]
			[betaPhiT (array (shape 0 2 0 1) beta (* beta x))]

			[/S_0 (array-inverse S_0)]
			[/S_N (array-add-elements /S_0;(array-mul-elements (identity-array 2) alpha)
									  (array-mul-elements (array (shape 0 2 0 2) 1 x x (* x x))
														  beta)
									  ;(array-mul-elements (array-mul PhiT Phi) beta)
									  )]
			[S_N (array-inverse /S_N)]
			[T (array (shape 0 1 0 1) t)]
			;[m_N_ar (array-mul-elements (array-mul (array-mul S_N PhiT) T) beta)]
;			[m_0_ar (array (shape 0 2 0 1) (vector-ref m_0 0) (vector-ref m_0 1))]
			[m_N (array-mul S_N 
							(array-add-elements
							 (array-mul /S_0 m_0)
							 (array-mul betaPhiT T) ))]
;			[m_N_vec (vector (array-ref m_N_ar 0 0) (array-ref m_N_ar 1 0))]
			)
	   (set! S_0 S_N); (print "S_N:" S_N)
	   (set! m_0 m_N)
	   (push! xs x)
	   (push! ts t)
	   )
	 (set! mode 0)]
	)
  (glut-post-redisplay)
#|
  (format #t "m0: ~a\n" m_0)
  (format #t "S0: ~a\n" S_0)
  (let1 nd2 (make-nd2-rand m_0 S_0)
	(dotimes (i 10)
	  (receive (x y) (nd2) 
		(format #t "  [nd2] (~a, ~a)\n" x y) )))
|#
  )

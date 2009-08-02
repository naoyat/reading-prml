(require "./lib/gl")
(require "./lib/util")
(require "./lib/random")
(require "./lib/distribution")

(use srfi-1)
(use gauche.uvector)
(use gauche.array)

(define xscale 1.0)
(define yscale 1.0)
(define zscale 1.0)

(define xmag (* xscale 0.9))
(define ymag (* yscale 0.9))
(define zmag (* zscale 0.9))

(define NDIV 50)
(define DRAW_MODE GL_LINE_LOOP)
;(define DRAW-MODE GL_POLYGON)
(set! *color-scale* 0.5);1.414213562)

(define Dir (make-Dirichlet-distribution (make-list 3 0)))
(define params '(0.1 1 10))

(define (inf-filter x) (clamp x -99999 99999))
#;(define (inf-filter x)
  (cond [(not (real? x)) #f]
		[(= x +inf.0) *maxnum*]
;		[(= x +nan.0) #f]
		[(= x -inf.0) *minnum*]
;		[(= x -nan.0) #f]
		[else x]))
;(define (f_ x) (+ (vector-ref w 0) (* (vector-ref w 1) x)))

;(define REP 10)

(define (memoize proc)
  (let1 fh (make-hash-table 'equal?)
	(lambda args
	  (or (hash-table-get fh args #f)
		  (let1 val (apply proc args)
			(hash-table-put! fh args val)
			val)))
	))

;(define (color z) '(0 0 1))

(define (on-display)
  (gl-clear GL_COLOR_BUFFER_BIT)


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

  ;; 0
  (dotimes (i 21)
	(let1 r (interpolate -1 1 (/ i 20))
	  (draw-line -1 r 0 1 r 0)
	  (draw-line r -1 0 r 1 0) ))


  (gl-color 0.7 0.7 0.7)
  (let* ([f (lambda (x y) (gauss2 (vector x y) (array->vector m_0) (array->vector S_0)))]
		 [f* (memoize f)])
	(let ([ax -0.75] [ay 0]
		  [bx 0.75] [by -0.833]
		  [cx 0.75] [cy 0.833])
	(dotimes (i NDIV)
	  (let* ([i0 (/ i NDIV)]
			 [x00 (interpolate ax bx i0)] [y00 (interpolate ay by i0)]
			 [x10 (interpolate ax cx i0)] [y10 (interpolate ay cy i0)]
			 [i1 (/ (+ i 1) NDIV)]
			 [x01 (interpolate ax bx i1)] [y01 (interpolate ay by i1)]
			 [x11 (interpolate ax cx i1)] [y11 (interpolate ay cy i1)])
		(dotimes (j (+ i 1))
		  (let* ([r0 (if (= j 0) 0 (/ j i))]
				 [ux (interpolate x00 x10 r0)] [uy (interpolate y00 y10 r0)]
				 [r10 (/ j (+ i 1))]
				 [vx (interpolate x01 x11 r10)] [vy (interpolate y01 y11 r10)]
				 [r11 (/ (+ j 1) (+ i 1))]
				 [wx (interpolate x01 x11 r11)] [wy (interpolate y01 y11 r11)])
			(define (foo x) (interpolate 7e-3 (- 1 7e-3) x))
			(define (mu i j)
			  (let ([ir (/ i NDIV)]
					[jr (if (zero? j) 0 (/ j i))])
				(list (foo (- 1 ir))
					  (foo (* ir (- 1 jr)))
					  (foo (* ir jr)))))
			(define (f µ1 µ2 µ3) (/ ([Dir'p] (list µ1 µ2 µ3)) 12))
			(draw-triangles ux uy (inf-filter (apply f (mu i j)))
							vx vy (inf-filter (apply f (mu (+ i 1) j)))
							wx wy (inf-filter (apply f (mu (+ i 1) (+ j 1))) ))
			))))))


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
    (glu-look-at 0.5 -1.8 3.6
;    (glu-look-at 0.75 -1.5 3.0
;    (glu-look-at 0 0 5
;    (glu-look-at 0 20 0
                 0 0 0
;				 0 1 0))
                 (cos theta) (sin theta) 0) )

  )

(define (on-keydown key x y)
  (when (= key 27) (exit 0))

  (let1 param (pop! params)
	(set! params (append params (list param)))
	(set! Dir (make-Dirichlet-distribution (make-list 3 param))))

  (glut-post-redisplay)
  )

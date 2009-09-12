;;
;; NN test: real -> real -> complex
;;
(use srfi-1)
(require "./lib/gl")
(require "./lib/random") ; Σ,Π, interpolate
(require "./lib/util") ; Σ,Π, interpolate
(require "./lib/neural-net")

(use gauche.uvector)
(use gauche.array)


(define nn (make-neural-network 0.04 tanh identity 2 8 1))
;([nn'desc])
(dotimes (i 3000)
  ([nn'learn] '(0 0) '(0))
  ([nn'learn] '(1 0) '(1))
  ([nn'learn] '(1 1) '(1+i))
  ([nn'learn] '(0 1) '(0+i))
  ([nn'learn] '(-1 1) '(-1+i))
  ([nn'learn] '(-1 0) '(-1))
  ([nn'learn] '(-1 -1) '(-1-i))
  ([nn'learn] '(0 -1) '(0-i))
  ([nn'learn] '(1 -1) '(1-i))
  )
;(print (map (compose car [nn'test]) '((1 0) (0 1) (-1 0) (0 -1))))
;(print (map (compose car [nn'test]) '((0 0) (1 1) (-1 1) (-1 -1) (1 -1))))

(define (learned-func x y)
  (let1 im (car ([nn'test] (list x y)))
	(values (real-part im) (imag-part im))))

;;;;;;;;;;;;
;; OpenGL ;;
;;;;;;;;;;;;
(define xscale 1.0)
(define yscale 1.0)
(define zscale 1.0)

(define xmag (* xscale 0.9))
(define ymag (* yscale 0.9))
(define zmag (* zscale 0.9))

(define NDIV 50)
(set! *color-scale* 1.414213562)
(set! *draw-mode* GL_LINE_LOOP)

;;
(define (on-display)
  (gl-clear GL_COLOR_BUFFER_BIT)
  
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
  (let1 r 0.015
	(for-each (lambda (xy)
				(receive (x y) (apply learned-func xy)
				  (draw-circle x y 0
							   r 0 0  0 r 0)
				  (draw-string x y 0 (format "~a" xy))
				  ))
			  '((1 0) (0 1) (-1 0) (0 -1)
				(0 0) (1 1) (-1 1) (-1 -1) (1 -1))))

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
;	)
  (glut-post-redisplay)
  )

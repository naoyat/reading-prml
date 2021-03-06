;;
;; NN test: complex -> complex
;;
(use srfi-1)
(require "./lib/gl")
(require "./lib/random") ; Σ,Π, interpolate
(require "./lib/util") ; Σ,Π, interpolate
(require "./lib/neural-net")

(use gauche.uvector)
(use gauche.array)

(define (make-z-rand)
  (let ([N (make-nd-rand 0 1)]
		[U (make-uniform-rand (- pi) (* pi))])
	(lambda () (make-polar (N) (U)) )))


(define nn (make-neural-network 0.04 tanh identity 1 5 1))
(dotimes (i 1000)
  (for-each (lambda (th) ([nn'learn] (list (make-polar 1.0 th)) (list (make-polar 1.0 (+ th pi/4)))))
			(map (cut * <> pi/180) (iota 12 0 30)))) 

(define (learned-func z)
  (let1 im (car ([nn'test] (list z)))
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
;(define *draw-mode* GL_POLYGON)

;;
(define (re x m)
  (/. (round->exact (* x m)) m))

(define (imag-pp z)
  (let ((rl (re (real-part z) 100))
		(im (re (imag-part z) 100)))
	(cond [(zero? im)
		   (x->string rl)]
		  [(< 0 im)
		   (if (zero? rl) (format "~ai" im) (format "~a+~ai" rl im))]
		  [(> 0 im)
		   (if (zero? rl) (format "~ai" im) (format "~a~ai" rl im))])))

(define (on-display)
  (gl-clear GL_COLOR_BUFFER_BIT)
  
  (let1 r 0.015
	(for-each (lambda (z)
				(receive (x y) (learned-func z)
				  (format #t "~a -> (~a ~a)\n" z x y)
				  (gl-color 0.9 0.2 0.2)
				  (draw-circle x y 0
							   r 0 0  0 r 0)
				  (gl-color 0.6 0.6 0.6)
				  (draw-string (+ x 0.001) y 0 (imag-pp z))
				  ))
			(map (lambda (th) (make-polar 1.0 (* th pi/180))) (iota 12 0 30))))
;;			  '(0 0-i 1-i 1 1+i 0+i -1+i -1 -1-i)))
	
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
    (glu-look-at 0.9 -1.8 1.8 0 0 0 (cos theta) (sin theta) 0) ))

(define (on-keydown key x y)
  (when (= key 27) (exit 0))
  (glut-post-redisplay))


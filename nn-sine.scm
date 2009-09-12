;;
;; neural network (by graph)
;;
(use srfi-1)
(use math.const)
(require "./lib/random")
(require "./lib/util")
(require "./lib/gd-graph")
(require "./lib/mac")
(require "./lib/neural-net")

(define N01 (make-nd-rand 0 1))

(define nn (make-neural-network 0.04 tanh identity 1 5 1))
(define x/ (iota 51 0 1/50))
(define t/ (map (lambda (x) (+ (sin (* 2 pi x)) (* 0.15 (N01)))) x/))


(time
 (dotimes (i 2000)
   (when (zero? (remainder i 10))
	 (display ".") (flush)
	 (let1 imgfile (format "png/sin_~4,'0d.jpg" i)
	   (make-graph-image imgfile '(0 1 0.5) '(-1 1 1) (lambda (x) (car ([nn'test] (list x))))
						 (lambda (im f x0 xF y0 yF o)
						   (let ((black [o'black])
								 (red [o'red]))
							 (gd-image-string im f (+ x0 150) (+ yF 10) (format "T = ~d" i) black)
							 (for-each (lambda (x t)
										 (let ([x_ (round->exact (interpolate x0 xF x))]
											   [y_ (round->exact (interpolate y0 yF (/ (+ t 1) 2)))])
										   (gd-image-arc im x_ y_ 5 5 0 360 red)))
									   x/ t/)
							 ([o'plot-curve] (lambda (x) (sin (* 2 pi x))) ([o'color] 0 255 0))))
						 'jpg)))
   (for-each (lambda (x t) ([nn'learn] (list x) (list t))) x/ t/)
   ))
;(print (map (compose car [nn'test]) '((0 0) (0 1) (1 0) (1 1))))
;(sys-system "find png -name sin_\*.png -exec sips --setProperty format jpeg {} --out jpeg \;")
;(sys-system "ffmpeg -i png/sin_%03d0.png sin_png.mpeg")
;(sys-system "ffmpeg -i jpeg/sin_%03d0.jpg sin_jpeg.mpeg")
(sys-system "ffmpeg -i png/sin_%03d0.jpg sin_jpg.mpeg")


(use c-wrapper) ; ガンマ関数をlibmから
(c-load "math.h" :import 'tgamma)
(define Γ tgamma)

(require "./lib/util")
(require "./lib/gd-graph")
(require "./lib/mac")

(make-graph-png "gamma.png" '(-4 4 1) '(-10 10 5) Γ
				(lambda (im f x0 xF y0 yF)
				  (let ([black (gd-image-color-allocate im 0 0 0)]
						[x_o (round->exact (/ (+ x0 xF) 2))]
						[y_o (round->exact (/ (+ y0 yF) 2))])
					(gd-image-line im x_o yF x_o y0 black)
					(gd-image-line im x0 y_o xF y_o black)
;					(gd-image-string-ft im black "/Library/Fonts/Arial Italic.ttf" 10.0 0 (- xF 50) (+ y0 13) "µ")
					)))

(open-image "gamma.png")

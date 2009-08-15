(require "./lib/util")
(require "./lib/gd-graph")
(require "./lib/distribution")
(require "./lib/mac")

(for-each
 (lambda (µ)
   (let1 Bin (make-binomial-distribution 10 µ)
     (make-histogram-png (format #f "Bin_10_~a.png" µ) '(0 10 1) '(0 3/10 1/10) [Bin'p]
                         (lambda (im f x0 xF y0 yF obj)
						   (let1 black [obj'black]
							 (gd-image-string-ft im black "/Library/Fonts/Arial Italic.ttf" 10.0 0 (round->exact (/ (+ x0 xF) 2)) (+ y0 18) "m")
							 )))))
 '(0 0.1 0.2 0.25 0.3 0.333 0.4 0.5 0.6 0.667 0.7 0.75 0.8 0.9 1))

(open-image "Bin_*.png")

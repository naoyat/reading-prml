(require "./lib/util")
(require "./lib/gd-graph")
(require "./lib/distribution")
(require "./lib/mac")

(use util.combinations)
(for-each
 (lambda (ab)
   (let* ([a (car ab)] [b (cadr ab)]
          [Beta (make-beta-distribution a b)])
     (make-graph-png (format #f "Beta_~a_~a.png" a b) '(0 1 0.5) '(0 3 1) [Beta'p]
                     (lambda (im f x0 xF y0 yF)
                       (let1 black (gd-image-color-allocate im 0 0 0)
                         (gd-image-string im f (+ x0 15) (+ yF 7) (format #f "a = ~a" a) black)
                         (gd-image-string im f (+ x0 15) (+ yF 21) (format #f "b = ~a" b) black)
                         (gd-image-string-ft im black "/Library/Fonts/Arial Italic.ttf" 10.0 0 (- xF 50) (+ y0 13) "µ")
                         )))))
 (cartesian-product '((0.1 0.5 1 2 3 4 8) (0.1 0.5 1 2 3 4 8))) )

(open-image "Beta_*.png")

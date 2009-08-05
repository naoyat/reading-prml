(require "./lib/util")
(require "./lib/gd-graph")
(require "./lib/mac")
(require "./lib/erf")

(make-graph-png "logistic-sigmoid.png" '(-10 10 5) '(0 1 0.5) σ
                (lambda (im f x0 xF y0 yF)
                  (let ([gray (gd-image-color-allocate im 224 224 224)]
                        [x_o (round->exact (/ (+ x0 xF) 2))]
                        [y_o (round->exact (/ (+ y0 yF) 2))])
                    (gd-image-line im x_o yF x_o y0 gray)
                    (gd-image-line im x0 y_o xF y_o gray)
                    )))
(open-image "logistic-sigmoid.png")

(make-graph-png "logit.png" '(0 1 0.5) '(-10 10 5) logit
                (lambda (im f x0 xF y0 yF)
                  (let ([gray (gd-image-color-allocate im 224 224 224)]
                        [x_o (round->exact (/ (+ x0 xF) 2))]
                        [y_o (round->exact (/ (+ y0 yF) 2))])
                    (gd-image-line im x_o yF x_o y0 gray)
                    (gd-image-line im x0 y_o xF y_o gray)
                    )))
(open-image "logit.png")

(make-graph-png "probit-1.png" '(-10 10 5) '(0 1 0.5) (lambda (a) (Φ (* (sqrt (/ pi 8)) a)))
                (lambda (im f x0 xF y0 yF)
                  (let ([gray (gd-image-color-allocate im 224 224 224)]
                        [x_o (round->exact (/ (+ x0 xF) 2))]
                        [y_o (round->exact (/ (+ y0 yF) 2))])
                    (gd-image-line im x_o yF x_o y0 gray)
                    (gd-image-line im x0 y_o xF y_o gray)
                    )))
(open-image "probit-1.png")

#|
(set! *epsilon* 1e-3)
(make-graph-png "probit.png" '(0 1 0.5) '(-10 10 5) probit
                (lambda (im f x0 xF y0 yF)
                  (let ([gray (gd-image-color-allocate im 224 224 224)]
                        [x_o (round->exact (/ (+ x0 xF) 2))]
                        [y_o (round->exact (/ (+ y0 yF) 2))])
                    (gd-image-line im x_o yF x_o y0 gray)
                    (gd-image-line im x0 y_o xF y_o gray)
                    )))
(open-image "probit.png")
|#

(set! *epsilon* 1e-5)
(make-graph-png "erf.png" '(-3 3 1) '(-1 1 1) erf
                (lambda (im f x0 xF y0 yF)
                  (let ([gray (gd-image-color-allocate im 224 224 224)]
                        [x_o (round->exact (/ (+ x0 xF) 2))]
                        [y_o (round->exact (/ (+ y0 yF) 2))])
                    (gd-image-line im x_o yF x_o y0 gray)
                    (gd-image-line im x0 y_o xF y_o gray)
                    )))
(open-image "erf.png")

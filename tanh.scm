(require "./lib/util")
(require "./lib/gd-graph")
(require "./lib/mac")
(require "./lib/erf")

(make-graph-png "tanh.png" '(-5 5 1) '(-1 1 0.5) tanh
                (lambda (im f x0 xF y0 yF obj)
                  (let ([gray ([obj'color] 224 224 224)]
                        [x_o (round->exact (/ (+ x0 xF) 2))]
                        [y_o (round->exact (/ (+ y0 yF) 2))])
                    (gd-image-line im x_o yF x_o y0 gray)
                    (gd-image-line im x0 y_o xF y_o gray))))
(open-image "tanh.png")


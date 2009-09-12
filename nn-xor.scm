(require "./lib/neural-net")

(define nn (make-neural-network 0.04 tanh identity 2 8 8 1))
;([nn'desc])
(time
 (dotimes (i 1000)
   ([nn'learn] '(0 0) '(0))
   ([nn'learn] '(0 1) '(1))
   ([nn'learn] '(1 0) '(1))
   ([nn'learn] '(1 1) '(0)) )
 )
(print (map (compose car [nn'test]) '((0 0) (0 1) (1 0) (1 1))))

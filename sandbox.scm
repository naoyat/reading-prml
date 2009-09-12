(use gauche.array)
(use srfi-1)

(require "./matrix")

;;
;; 行列を二次元配列 (shape 0 h 0 w) で表現、でいいのかな
;;

(define (array-desc ar)
  (print ar)
  (let1 rank (array-rank ar); array-rank --> 次元数。次元数とランクって同じなの？
;	(format #t "rank: ~d\n" rank)
#;	(dotimes (dim rank)
	  (format #t "  #~d: start=~d end=~d length=~d\n" dim
			  (array-start ar dim)
			  (array-end ar dim)
			  (array-length ar dim) ))
;	(array-for-each-index ar (lambda args (print ">" args "<")))
;	(shape-for-each (array-shape ar) (lambda args #?=args))
;	(array-for-each-index ar (lambda ixs (format #t ">~a<\n" ixs)))
	(matrix-print ar) (newline)
	))


;  | 8 3 4 |
;  | 1 5 9 |
;  | 6 7 2 |
; のような行列を定義する方法：
(define m1 #,(<array> (0 3 0 3) 8 3 4 1 5 9 6 7 2))
(pp m1)

(define m11 #,(<array> (0 3 0 3) 1 2 3 4 5 6 7 8 9))
(pp m11)

#|
0 2
1 3
3 5
なのか
0 2 1
3 3 5
なのか
|#
(define m2 #,(<array> (0 3 0 2) 0 2 1 3 3 5))
;; ３行２列に詰めるので
;; ((c 0 2)
;;  (c 1 3)
;;  (c 3 5))
(pp m2)

(pp (array-mul m2 #,(<array> (0 2 0 2) 1 2 3 4)))

(define m20 (shape 0 2 1 3 3 5))
(pp m20)

(define m3 (array (shape 0 2 1 3) 'a 'b 'c 'd))
(pp m3)


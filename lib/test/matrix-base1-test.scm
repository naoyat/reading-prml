(use gauche.test)

(require "../matrix-base1")
(test-start "matrix-base1.scm")

(test-section "matrix(): 要素ベクトルを与えて行列を作る")
;; 1:12 = (1 2 3 ... 11 12) = (iota 12 1)
(test* "matrix(1:12, nrow=3, ncol=4)"
	   #,(<array> (1 4 1 5) 1 4 7 10 2 5 8 11 3 6 9 12)
	   (%matrix (iota 12 1) :nrow 3 :ncol 4))
(test* "matrix(1:12, nrow=3) # 自動的に ncol=4 とされる"
	   #,(<array> (1 4 1 5) 1 4 7 10 2 5 8 11 3 6 9 12)
	   (%matrix (iota 12 1) :nrow 3))
(test* "matrix(1:12, ncol=4) # 自動的に ncrow=3 とされる"
	   #,(<array> (1 4 1 5) 1 4 7 10 2 5 8 11 3 6 9 12)
	   (%matrix (iota 12 1) :ncol 4))
(test* "matrix(1:12, 3, 4)"
	   #,(<array> (1 4 1 5) 1 4 7 10 2 5 8 11 3 6 9 12)
	   (%matrix (iota 12 1) 3 4))
(test* "matrix(1:12, nc=4, nr=3, b=F)"
	   #,(<array> (1 4 1 5) 1 4 7 10 2 5 8 11 3 6 9 12)
	   (%matrix (iota 12 1) :nc 4 :nr 3 :b #f))
(test* "array(1:12, c(3,4)) # array は必ず列主導で要素が並べられる"
	   #,(<array> (1 4 1 5) 1 4 7 10 2 5 8 11 3 6 9 12)
	   (%array (iota 12 1) '(3 4)))
(test* "matrix(1:12, 3, 4, byrow=TRUE) # 要素を行主導で行列化"
	   #,(<array> (1 4 1 5) 1 2 3 4 5 6 7 8 9 10 11 12)
	   (%matrix (iota 12 1) 3 4 :byrow #t))
(test* "matrix(1:12, nc=4, b=T) # 行主導"
	   #,(<array> (1 4 1 5) 1 2 3 4 5 6 7 8 9 10 11 12)
	   (%matrix (iota 12 1) :nc 4 :b #t))
(test* "array(1:12, c(4,3)) # array を使うならこうする c(3,4) でないことを注意"
	   #,(<array> (1 4 1 5) 1 2 3 4 5 6 7 8 9 10 11 12)
	   (%t (%array (iota 12 1) '(4 3))))

(test* "matrix(1:6, , 3, byrow=TRUE) # ncol=3,自動的にnrow=2になる"
	   #,(<array> (1 3 1 4) 1 2 3 4 5 6)
	   (%matrix (iota 6 1) #f 3 :b #t))
(test* "matrix(1:6, , 3) # => matrix(1:6,,3,byrow=FALSE)"
	   #,(<array> (1 3 1 4) 1 3 5 2 4 6)
	   (%matrix (iota 6 1) #f 3))
(test* "matrix(1:3, 3, 2) # ベクトル 1:3 をリサイクル使用"
	   #,(<array> (1 4 1 3) 1 1 2 2 3 3)
	   (%matrix (iota 3 1) 3 2))

(test-section "規則的でない比較的小さな行列をプログラム中や対話的に入力する")
(test* "rbind(c(0, 0.57, 0.57, 0.57, 0.57),c(0.46, 0, 0, 0, 0), ...) # rbind 関数で行ベクトルを縦に並べる"
	   #,(<array> (1 6 1 6)
				  0    0.57 0.57 0.57 0.57
				  0.46 0    0    0    0
				  0    0.77 0    0    0
				  0    0    0.82 0    0
				  0    0    0    0.91 0.65)
	   (%rbind '(0    0.57 0.57 0.57 0.57)
			   '(0.46 0    0    0    0   )
			   '(0    0.77 0    0    0   )
			   '(0    0    0.82 0    0   )
			   '(0    0    0    0.91 0.65) ) )
(test* "cbind(c(0, 0.57, 0.57, 0.57, 0.57),c(0.46, 0, 0, 0, 0), ...) # cbind 関数を使うと、列ベクトルとして横に並べられる"
	   #,(<array> (1 6 1 6)
				  0    0.46 0    0    0
				  0.57 0    0.77 0    0
				  0.57 0    0    0.82 0
				  0.57 0    0    0    0.91
				  0.57 0    0    0    0.65)
	   (%cbind '(0    0.57 0.57 0.57 0.57)
			   '(0.46 0    0    0    0   )
			   '(0    0.77 0    0    0   )
			   '(0    0    0.82 0    0   )
			   '(0    0    0    0.91 0.65) ) )

;; 行列をベクトルとして操作することができる方がよいか？今のところそんな気もしないので略

(test-section "行列のサイズ（行数、列数）を知る")
(let1 A (%matrix (iota 12 1) 3 4)
  (test* "dim(A)" '(3 4) (%dim A))
  (test* "ncol(A)" 4 (%ncol A))
  (test* "nrow(A)" 3 (%nrow A)))

;; colSums

(test-section "転置行列")
(let1 d (%matrix (iota 6 1) :nr 2)
  (test* "d" #,(<array> (1 3 1 4) 1 3 5 2 4 6) d)
  (test* "t(d)" #,(<array> (1 4 1 3) 1 2 3 4 5 6) (%t d)))

(test-section "対角行列を作る")
(test* "diag(1, ncol=3, nrow=3)"
	   #,(<array> (1 4 1 4) 1 0 0 0 1 0 0 0 1)
	   (%diag 1 :ncol 3 :nrow 3))
(test* "diag(1, 3) # 単位行列"
	   #,(<array> (1 4 1 4) 1 0 0 0 1 0 0 0 1)
	   (%diag 1 3))
(test* "diag(3) # 単位行列"
	   #,(<array> (1 4 1 4) 1 0 0 0 1 0 0 0 1)
	   (%diag 3))
(test* "diag(5, 3) # 対角成分が5の3x3行列"
	   #,(<array> (1 4 1 4) 5 0 0 0 5 0 0 0 5)
	   (%diag 5 3))
(test* "diag(rep(1,3)) # =diag(c(1,1,1))"
	   #,(<array> (1 4 1 4) 1 0 0 0 1 0 0 0 1)
	   (%diag (%rep 1 3)))
(test* "diag(c(1,1,1))"
	   #,(<array> (1 4 1 4) 1 0 0 0 1 0 0 0 1)
	   (%diag '(1 1 1)))

(let1 x (%matrix 0 :ncol 3 :nrow 3) ;; 全成分が0の行列
  (%set-diag! x 1)
  (test* "diag(x) <- 1" #,(<array> (1 4 1 4) 1 0 0 0 1 0 0 0 1) x))

(let1 x (%matrix 0 :ncol 3 :nrow 3) ;; 全成分が0の行列
  (%set-diag! x '(1 2 3))
  (test* "diag(x) <- c(1, 2, 3)" #,(<array> (1 4 1 4) 1 0 0 0 2 0 0 0 3) x))

(test-section "対角成分を取り出す")
(let1 x (%matrix (iota 16 1) :nr 4 :nc 4)
  (test* "diag(x)" '(1 6 11 16) (%diag x)))

(test-section "行列の対角成分からなる対角行列を作る")
(let1 x (%matrix (iota 12 1) 3 4)
  (test* "x = matrix(1:12, 3, 4)" #,(<array> (1 4 1 5) 1 4 7 10 2 5 8 11 3 6 9 12) x)
  (test* "diag(x)" '(1 5 9) (%diag x))
  (test* "diag(diag(x), 3, 4)" #,(<array> (1 4 1 5) 1 0 0 0 0 5 0 0 0 0 9 0) (%diag (%diag x) 3 4)))

(test-section "単位行列")
(test* "diag(4)" #,(<array> (1 5 1 5) 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1) (%diag 4))

(test-section "ゼロ行列")
(test* "matrix(0, nr=3, nc=3)" #,(<array> (1 4 1 4) 0 0 0 0 0 0 0 0 0) (%matrix 0 :nr 3 :nc 3))
(test* "diag(0,3)" #,(<array> (1 4 1 4) 0 0 0 0 0 0 0 0 0) (%diag 0 3))
(test* "diag(0,3,4)" #,(<array> (1 4 1 5) 0 0 0 0 0 0 0 0 0 0 0 0) (%diag 0 3 4))

;;;;;;;;;;
(let* ([dim 3]
	   [x (%matrix (iota 9 1) 3)])
;	   [y (%matrix '(3 5 -2 0 1 4 2 7 -9) 3)])
  (test-section "転置 / 逆行列 / 行列式")
  (test* "x" #,(<array> (1 4 1 4) 1 4 7 2 5 8 3 6 9) x)
  (test* "t(x)" #,(<array> (1 4 1 4) 1 2 3 4 5 6 7 8 9) (%t x))
  (dotimes (i 10)
	(let* ([A (make-random-matrix dim dim 1 10)] [A-1 (%-1 A)]
		   [B (make-random-matrix dim dim 1 10)] [B-1 (%-1 B)])
	  (test* "(AB)T = BTAT" (%*% (%t B) (%t A)) (%t (%*% A B)))
	  (when (and A-1 B-1)
		(format #t "A = ~a\n" A)
		(format #t "A-1 = ~a\n" A-1)
		(test* "AA-1 = I" (%diag 3) (%*% A A-1))
		(test* "A-1A = I" (%diag 3) (%*% A-1 A))
		(test* "(AB)-1 = B-1A-1" (%*% B-1 A-1) (%-1 (%*% A B)))
		(test* "(AT)-1) = (A-1)T" (%t (%-1 A)) (%-1 (%t A)) )
		(test* "|AB| = |A||B|" (* (%Det A) (%Det B)) (%Det (%*% A B)))
		(test* "|A-1| = 1/|A|" (/ (%Det A)) (%Det A-1))
		)))

  (test-section "トレース")
  (test* "x" #,(<array> (1 4 1 4) 1 4 7 2 5 8 3 6 9) x)
  (test* "Tr(x)" 15 (%Tr x)); (+ 1 5 9)
  (dotimes (i 10)
	(let ([A (make-random-matrix dim dim 1 10)]
		  [B (make-random-matrix dim dim 1 10)]
		  [C (make-random-matrix dim dim 1 10)])
	  (test* "Tr(AB) = Tr(BA)" (%Tr (%*% A B)) (%Tr (%*% B A)))
	  (test* "Tr(ABC) = Tr(CAB) = Tr(BCA)" #t
			 (= (%Tr (%*% A B C)) (%Tr (%*% C A B)) (%Tr (%*% B C A))) )
	  )))
(test-end)

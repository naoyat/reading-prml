;;
;; Mac OS X専用ツール
;;

;;
;; ファイルを開く
;;
(define (open-image . paths)
  (for-each (lambda (path) (sys-system (string-append "open " path))) paths))

#lang racket

(provide pprint)

(define (de-index-list l)
  (define (de-index node)
    (cond
     ((list? (cadr node)) (de-index-list (cadr node)))
     (else (cadr node))))
  (if (list? l) (map de-index l) l))

(define (pprint result)
  (define fail-fmt
#<<eof

    -----------------------
    ** Match failed with **
    -----------------------
    faillist =====> ~a
    store    =====> ~v

eof
    )

  (define success-fmt
#<<eof

    --------------------------
    ** Match succeeded with **
    --------------------------
    value =====> ~v

eof
    )
  (match result
    [(list 'FAIL faillist stream store)
     (define flist (list 'FAIL (map (lambda (f) (list (pretty-format (first f)) (de-index-list (third f)))) faillist)))
     (printf fail-fmt (pretty-format flist) store)]

    [(list val stream store)
     (printf success-fmt val)]))

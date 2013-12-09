#lang racket

(provide (all-defined-out))

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
    stream   =====> ~v

eof
    )

  (define success-fmt
#<<eof

    --------------------------
    ** Match succeeded with **
    --------------------------
    value    =====> ~v
    stream   =====> ~v

eof
    )
  (define left-on (compose list->string de-index-list ))
  (match result
    [(list 'FAIL faillist stream store)
     (define flist (list 'FAIL (map (lambda (f) (list (pretty-format (first f)) (de-index-list (third f)))) faillist)))
     (printf fail-fmt (pretty-format flist) store (left-on stream))]

    [(list val stream store)
     (printf success-fmt val (left-on stream))]))

(define (fail? v) (equal? 'FAIL (car v)))
(define (success? v) (not (fail? v)))
(define (value-stream v)
  (match v
    [(list 'FAIL _ s __) s]
    [(list val s _) s]))
(define (stream-pos0 s)
  (if (empty? s) '() (caar s)))
(define m-value first)
(define m-lr? second)
(define m-lr-detected? third)

(define (ptable t)
  (printf "~n~n    Memo:~n")
  (hash-for-each t (lambda (k m)
                     (printf "    ~a ~a ==> ~v ~a~n"
                             (car k) (stream-pos0 (second k))
                             (car (m-value m)) (stream-pos0 (value-stream (m-value m)))))))

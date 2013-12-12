#lang racket

(provide (all-defined-out))

;; ======================================================== ;;
;; Stream constructors                                      ;;
;; ======================================================== ;;
(define (construct-stream input)
  (define (list->stream l [depth 0])
    (build-list (length l)
                (lambda (n)
                  `((,depth ,n) ,(cond
                                  ((list? (list-ref l n)) (list->stream (list-ref l n) n))
                                  (else (list-ref l n) ))))))
  (define (string->stream s [depth 0])
    (build-list (string-length s)
                (lambda (n) `((,depth ,n) ,(string-ref s n)))))
  (cond
   ((list? input) (list `((-1 0) ,(list->stream input))))
   ((string? input) (string->stream input))))

(define (stream? stream)
  (and (list? stream)
       (andmap (lambda (p)
                 (match p
                   [(list (list (? number?) (? number?)) _) #t]
                   [rest #f]))
               stream)))

(define (de-index-list l)
  (define (de-index node)
    (cond
     ((list? (cadr node)) (de-index-list (cadr node)))
     (else (cadr node))))
  (if (list? l) (map de-index l) l))

;; ======================================================== ;;
;; Memo                                                     ;;
;; ======================================================== ;;
(define table (make-hash))
;; table: (rule-name stream) -> (value lr? lr-detected?)

(define (fresh-table!)
  (set! table (make-hash)))

(define (memo rule-name stream)
  (hash-ref table (list rule-name stream) #f))

(define (memo-add rule-name stream value [lr? #f] [lr-detected? #f])
  (hash-set! table (list rule-name stream) (list value lr? lr-detected?)))

;; ======================================================== ;;
;; Store                                                    ;;
;; ======================================================== ;;
(define fresh-store (lambda () '()))

(define (store->env a-list)
  (define (quote-value binding)
    (match binding
      [(list id v) `(,id (quote ,v))]))
  (map quote-value  a-list))

(define (append-old-store ans old-store)
  (let* ((rev-ans (reverse ans))
         (new-store (car rev-ans)))
    (reverse (cons (append old-store new-store) (cdr rev-ans)))))

;; ======================================================== ;;
;; Pretty printing                                          ;;
;; ======================================================== ;;
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

(define (ptable t)
  (printf "~n~n    Memo:~n")
  (hash-for-each t (lambda (k m)
                     (printf "    ~a ~a ==> ~v ~a~n"
                             (car k) (stream-pos0 (second k))
                             (car (m-value m)) (stream-pos0 (value-stream (m-value m)))))))

;; ======================================================== ;;
;; Getters                                                  ;;
;; ======================================================== ;;
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

;; ======================================================== ;;
;; Debugging                                                ;;
;; ======================================================== ;;
(define debug? #t)
(define (debug-on?) debug?)
(define (debug-off!)
  (set! debug-count! 0)
  (set! debug? #f))
(define (debug-on!)
  (set! debug-count! 0)
  (set! debug? #t))
(define debug-count! 0)
(define (debug-count-inc)
  (set! debug-count! (add1 debug-count!)))
(define (debug-count-dec)
  (set! debug-count! (sub1 debug-count!)))

(define (debug-pre-apply rule-name stream store)
  (when (debug-on?)
    (unless (equal? rule-name 'anything)
      (printf "~a   |~a -stream ~v -store ~v\n"
              (list->string (build-list debug-count! (lambda (n) #\>)))
              rule-name (de-index-list stream) store)
      (set! debug-count! (add1 debug-count!)))))

(define (debug-post-apply rule-name stream store ans)
  (when (debug-on?)
    (unless (equal? rule-name 'anything)
      (set! debug-count! (sub1 debug-count!))
      (printf "~a   |~a -stream ~v -store ~v -ans ~v\n"
              (list->string (build-list debug-count! (lambda (n) #\<)))
              rule-name (de-index-list stream) (append store (last ans)) (car ans)))))

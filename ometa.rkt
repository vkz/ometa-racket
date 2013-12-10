#lang racket
(require "helpers.rkt")

(provide construct-stream
         interp
         desugar
         ometa
         omatch)

;; ======================================================== ;;
;; How to use                                               ;;
;; ======================================================== ;;
;; 1. OMeta program:
;;
;;    (define test-program
;;      (ometa
;;       (rule-name parsing-expression) ...))
;;
;; 2. Input: string or list
;;
;; 3. Run it: (omatch test-program starting-rule-name input)

;; reflective hook to reel the module namespace
;; must insert this into user-code before interpreting
;; otherwise `eval' won't have bindings from user top-level
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

;; ======================================================== ;;
;; Grammar                                                  ;;
;; ======================================================== ;;
;; e ==(empty)
;;     (atom a)
;;     (apply A)
;;     (seq e1 e2)
;;     (alt e1 e2)
;;     (many e)
;;     (~ e)
;;     (bind x e)
;;     (-> t)
;;     (list e )

;; Value == a
;;         (Value ...)
;;          none

;; Stream == ((index Value) ...)
;; index  == (num num)

;; a == number
;;      character (#\c)
;;      string    ("string")

;; store == ((symbol? Value) ...)

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
;; Interpreter (match)                                      ;;
;; ======================================================== ;;
(define (interp/fresh-memo omprog start-rule stream [store '()])
  (fresh-table!)
  (interp omprog start-rule stream store))

(define (interp omprog start stream store)
  ;; -> (Value Stream Store)
  ;; -> ('FAIL fail-list Stream Store)
  (define fresh-store (lambda () '()))
  (define rules (append omprog '()))
  (define exp-name car)

  (define (find-rule-by-name  name)
    (cond
     ((assoc name rules)
      => (lambda (rule-pair) (cadr rule-pair)))
     (else #f)))

  (define (store->env a-list)
    (define (quote-value binding)
      (match binding
        [(list id v) `(,id (quote ,v))]))
    (map quote-value  a-list))

  (define (fail e stream store faillist)
    ;; -> ('FAIL fail-list stream store) where `fail-list' is a list of (exp index top-of-stream-value)
    (cond
     ((empty? stream) (list 'FAIL (cons (list e 'END '_)      faillist) stream store))
     (else            (list 'FAIL (cons (cons e (car stream)) faillist) stream store))))

  (define (fail/empty stream store)
    (list 'FAIL '() stream store))

  (define (anything stream store)
    (define value cadr)
    (if (empty? stream)
        (fail/empty stream store)
        (list (value (car stream)) (cdr stream) store)))

  (define (rule-apply name stream store)
    (define (init-memo/fail-and-align-flags-for-planting-seed)
      ;; initialize cash entry with 'FAIL, suspect left recursion - set `lr' to #t
      (memo-add name stream (fail/empty stream (fresh-store)) #t))
    (define (align-flags-for-growing-and-fail)
      ;; set lr and lr-detected flags to #f and #t respectively
      (memo-add name stream (m-value (memo name stream)) #f #t)
      (fail/empty stream store))
    (define (left-recursion?)
      ;; is lr flag set?
      (m-lr? (memo name stream)))
    (define (grow-lr body)
      ;; invariant: latest and largest successful match is in cash
      (let* ([ans (e body stream store)]
             [ans-stream (value-stream ans)]
             [memo-entry (memo name stream)]
             [memo-stream (value-stream (m-value memo-entry))])
        (if (or (fail? ans)
                (>= (length ans-stream) (length memo-stream)))
            ;; match failed or we're no longer making progress (ans stream not shrinking)
            (begin
              (m-value memo-entry))
            ;; match succeeded and we're making progress, keep growing the match
            (begin
              (memo-add name stream ans (m-lr? memo-entry) (m-lr-detected? memo-entry))
              (grow-lr body)))))
    (let (( r (reverse
               (cond
                ((equal? name 'anything)     (anything stream (fresh-store)))
                ;; cash hit
                ((memo name stream)       => (lambda (memo-entry)
                                               (if (left-recursion?)
                                                   ;; left recursion detected
                                                   ;; plant the seed: 'FAIL #1 so that #2 in (alt #1 #2) can match
                                                   ;; drop the lr flag, so we don't come back to this branch
                                                   ;; set the lr-detected flag, so we can start growing
                                                   (align-flags-for-growing-and-fail)
                                                   ;; not in left recursion
                                                   (m-value memo-entry))))
                ;; cash miss
                ((find-rule-by-name name) => (lambda (body)
                                               (init-memo/fail-and-align-flags-for-planting-seed)
                                               ;; eval the body but this time cash is guaranteed to be hit
                                               ;; ans holds the seed to be grown if lr-detected is set
                                               (let ((ans (e body stream (fresh-store)))
                                                     (m (memo name stream)))
                                                 ;; commit the seed to cash
                                                 ;; drop lr flag, preserve lr-detected flag
                                                 (memo-add name stream ans #f (m-lr-detected? m))
                                                 (if (and (m-lr-detected? m)
                                                          (not (fail? ans)))
                                                     ;; left-recursion with seed, start growing
                                                     (grow-lr body)
                                                     ;; not in left recursion
                                                     ans))))
                (else (error "no such rule " name))))))
      (reverse (cons (append (car r) store) (cdr r)))))

  (define (e exp stream store)
    ;; -> ((index value) stream store)
    ;; -> ('FAIL fail-list stream store)
    (match
      (case (exp-name exp)
        ((apply) (rule-apply (cadr exp) stream store))

        ((empty) (list 'NONE stream store))

        ((seq) (match (e (second exp) stream store)
                 [(list val s st) (e (third exp) s st)]
                 [ fail fail]))

        ((atom) (begin
                  (define a? (lambda (b) (equal? b (cadr exp))))
                  (match (e `(apply anything) stream store)
                    [(list (? a? a) s st) (list a s st)]
                    [ _ (fail/empty stream store)])))

        ((alt) (match (e (second exp) stream store)
                 [(list 'FAIL faillist s st) (e (third exp) stream st)]
                 [result result]))

        ((many) (match (e (second exp) stream store)
                  [(list 'FAIL faillist s st) (list '() stream st)]
                  [_ (e `(many1 ,(second exp)) stream store)]))
        ((many1) (match (e (second exp) stream store)
                   [(list 'FAIL faillist s1 st1) (list '() stream st1)]
                   [(list v1 s1 st1) (match (e `(many1 ,(second exp)) s1 st1)
                                       [(list v-rest s-rest st-rest)
                                        (list (append `(,v1) v-rest) s-rest st-rest)])]))

        ((~) (match (e (second exp) stream store)
               [(list 'FAIL faillist s st) (list 'NONE stream st)]
               [(list _ s st) (fail e stream st '())]))

        ((bind) (match (e (third exp) stream store)
                  [(list val s st) (list val s (cons (list (second exp) val) st))]
                  [fail fail]))

        ((->)   (begin
                  (define env (store->env store))
                  (define code (second exp))
                  (define result (eval `(let* ,(reverse env) ,code) ns))
                  (list result stream store)))

        ((list) (begin
                  (define temprule (gensym "RULE"))
                  (define list-pattern (second exp))
                  (define subprog (cons (list temprule list-pattern) rules))
                  (match (car stream)
                    [(list pos (? list? substream))
                     (match (interp subprog temprule substream store)
                       [(list val (? empty? s) st)   (list (de-index-list substream) (cdr stream) st)]
                       [(list 'FAIL faillist s st)   (list 'FAIL (cdr faillist) s st)] ;don't report `(apply temprule)'
                       [substream-is-too-long (fail/empty stream store)])]
                    [(list pos (? (compose not list?))) (fail/empty stream store)]
                    [ oops (error "Stream cell must contain a Value" (car stream))])))
        )
      [(list 'FAIL faillist s st) (fail exp stream st faillist)]
      [result result]))
  (e `(apply ,start) stream store))

(define (desugar omprog)
  (define (desugar-e e)
    (match e
      ;; seq* into nested seq
      [`(seq* ,e1) (desugar-e e1)]
      [`(seq* ,e1 ,e2) `(seq ,(desugar-e e1) ,(desugar-e e2))]
      [`(seq* ,e1 ,e2 ...) `(seq ,(desugar-e e1) ,(desugar-e `(seq* ,@e2)))]
      ;; alt* into nested alt
      [`(alt* ,e1) (desugar-e e1)]
      [`(alt* ,e1 ,e2) `(alt ,(desugar-e e1) ,(desugar-e e2))]
      [`(alt* ,e1 ,e2 ...) `(alt ,(desugar-e e1) ,(desugar-e `(alt* ,@e2)))]
      [any any]))
  (define (desugar-rule r)
    (list (first r) (desugar-e (second r))))
  (map desugar-rule omprog))

(define-syntax-rule (omatch omprog start input)
  (interp/fresh-memo omprog (quote start) (construct-stream `input) '()))

(define-syntax-rule (ometa rule ...)
  (desugar `(rule ...)))

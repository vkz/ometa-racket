#lang racket
(require "helpers.rkt")
(provide interp
         desugar
         desugar-e
         ometa
         omatch)

;; ======================================================== ;;
;; How to use                                               ;;
;; ======================================================== ;;
;; 1. Define OMeta program:
;;
;;    (define test-program
;;      (ometa
;;       (rule-name parsing-expression) ...))
;;
;; 2. Run it:
;;
;;     (omatch test-program
;;             starting-rule-name
;;             input)
;;
;;    Where input is a string or a list.

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
;;
;; Value == a
;;         (Value ...)
;;          none
;;
;; Stream == ((index Value) ...)
;; index  == (num num)
;;
;; a == number
;;      character (#\c)
;;      string    ("string")
;;
;; store == ((symbol? Value) ...)

;; reflective hook to reel the module namespace
;; must insert this into user-code before interpreting
;; otherwise `eval' won't have bindings from user top-level
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

;; ======================================================== ;;
;; Syntax                                                   ;;
;; ======================================================== ;;
(define-syntax-rule (omatch omprog start input)
  (interp/fresh-memo omprog (quote start) (construct-stream input) '()))

(define-syntax-rule (ometa rule ...)
  (desugar `(rule ...)))

;; ======================================================== ;;
;; Interpreter (match)                                      ;;
;; ======================================================== ;;
(define (interp/fresh-memo omprog start-rule stream [store '()])
  (fresh-table!)
  (interp omprog start-rule stream store))

(define (interp omprog start stream store)
  ;; -> (Value Stream Store)
  ;; -> ('FAIL fail-list Stream Store)
  (define rules (append omprog '()))

  (define (fail e stream store faillist)
    ;; -> ('FAIL fail-list stream store)
    ;;    `fail-list' is a list of (exp index top-of-stream-value)
    (cond
     ((empty? stream) (list 'FAIL (cons (list e 'END '_)      faillist) stream store))
     (else            (list 'FAIL (cons (cons e (car stream)) faillist) stream store))))

  (define (fail/empty stream store)
    (list 'FAIL '() stream store))

  (define (anything stream store)
    (define value cadr)
    (if (empty? stream)
        (fail/empty stream store)
        (list (de-index-list (value (car stream))) (cdr stream) store)))

  (define (rule-apply name args stream store)

    ;; initialize cash entry with 'FAIL
    ;; suspect left recursion: set `lr' to #t
    (define (init-memo/fail-and-align-flags-for-planting-seed)
      (memo-add name stream (fail/empty stream (fresh-store)) #t))

    ;; set lr and lr-detected flags to #f and #t respectively
    (define (align-flags-for-growing-and-fail)
      (memo-add name stream (m-value (memo name stream)) #f #t)
      (fail/empty stream store))

    ;; is lr flag set?
    (define (left-recursion?)
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

    ;; reverse trickery is to insure proper variable shadowing
    (let ((r (reverse
              (cond
               ((equal? name 'anything) (anything stream (fresh-store)))
               ;; cash hit
               ((memo name stream)
                => (lambda (memo-entry)
                     (if (left-recursion?)
                         ;; left recursion detected! Plant the seed: 'FAIL #1 so that #2 in (alt
                         ;; #1 #2) can match, drop the lr flag so we don't come back to this
                         ;; branch, set the lr-detected flag, so we can start growing
                         (align-flags-for-growing-and-fail)
                         ;; not in left recursion
                         (m-value memo-entry))))
               ;; cash miss
               ((find-rule-by-name name rules args)
                => (lambda (body)
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
    ;; -> (value stream store)
    ;; -> ('FAIL fail-list stream store)
    (match
      (case (car exp)
        ((apply) (let ((rule-name (cadr exp))
                       (rule-args (cddr exp)))
                   (debug-pre-apply rule-name stream store)
                   (let ((ans
                          (rule-apply rule-name rule-args stream (fresh-store))))
                     (debug-post-apply rule-name stream store ans)
                     (append-old-store ans store))))

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

        ((->?)   (begin
                   (define env (store->env store))
                   (define code (second exp))
                   (define result (eval `(let* ,(reverse env) ,code) ns))
                   (if result
                       (list 'NONE stream store)
                       (fail/empty stream store))))

        ((list) (begin
                  (define temprule (gensym "RULE"))
                  (define list-pattern (second exp))
                  (define subprog (cons (list temprule list-pattern) rules))
                  (if (empty? stream) ; matching "insides" of an empty list?
                      (fail/empty stream store)
                      (match (car stream)
                        [(list pos (? stream? substream))
                         (match (interp subprog temprule substream store)
                           [(list val (? empty? s) st) (list (de-index-list substream) (cdr stream) st)]
                           [(list 'FAIL faillist s st) (list 'FAIL (cdr faillist) s st)] ;don't report `(apply temprule)'
                           [substream-is-too-long (fail/empty stream store)])]
                        [(list pos (? (compose not list?))) (fail/empty stream store)]
                        [ oops (error "Stream cell must contain a Value" (car stream))]))))
        )
      [(list 'FAIL faillist s st) (fail exp stream st faillist)]
      [result result]))
  (e `(apply ,start) stream store))

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
    [`(many ,e1) `(many ,(desugar-e e1))]
    [`(many1 ,e1) `(many1 ,(desugar-e e1))]
    [`(many+ ,e1) (let ((a (gensym '+a))
                        (rest (gensym '+rest))
                        (body (desugar-e e1)))
                    (desugar-e `(seq* (bind ,a ,body)
                                      (bind ,rest (many ,body))
                                      (-> (cons ,a ,rest)))))]
    [`(bind ,id ,e1) `(bind ,id ,(desugar-e e1))]
    [`(~ ,e1) `(~ ,(desugar-e e1))]
    [`(list ,e1) `(list ,(desugar-e e1))]
    [rest rest]))

(define (desugar omprog)
  (define (desugar-rule rule)
    (match rule
      [(list name ids ... body)
       ;; =>
       `(,name ,@ids ,(desugar-e body))]
      [_
       ;; =>
       (error "Bad syntax in rule " rule)]))
  (map desugar-rule omprog))

(define (find-rule-by-name  name rules [args '()])
  ;; find a rule (name id ... body) and return
  ;; its body extended with ids bound to args
  ;; -> (desugar-e (seq* (bind id arg) ... body))
  (cond
   ((assoc name rules)
    => (lambda (rule)
         (match rule
           [(list name ids ... body)
            ;; =>
            (let* ((bind (lambda (id arg) `(bind ,id (-> ,arg))))
                   (bindings (map bind ids args)))
              (desugar-e `(seq* ,@bindings ,body)))]
           [_
            ;; =>
            (error "Bad syntax in rule " rule)])))
   (else #f)))

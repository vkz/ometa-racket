#lang racket
(require "helpers.rkt"
         racket/trace)
(provide interp
         desugar
         desugar-e
         ometa
         define-ometa
         define-ometa-namespace
         omatch)

;; ======================================================== ;;
;; How to use                                               ;;
;; ======================================================== ;;
;; 1. Define OMeta program:
;;
;;    (define test-program
;;      (ometa
;;       (rule-name parsing-expression) ...))
;; or
;;    (define-ometa test-program
;;      (rule-name parsing-expression) ...)
;;
;; 2. Run it:
;;
;;     (omatch test-program
;;             starting-rule-name
;;             input
;;             namespace)
;;
;;    Where input is a string or a list.
;;    Namespace is optional.
;;
;; 3. If you need bindings external to OMeta or invoke rules
;;    from other OMeta programs:
;;
;;    (define-ometa-namespace ns)
;;
;;    and pass `ns' as the last argument to `omatch'

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

(debug-off!)

;; reflective hook to reel the module namespace
;; must insert this into user-code before interpreting
;; otherwise `eval' won't have bindings from user top-level
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

;; ======================================================== ;;
;; Syntax                                                   ;;
;; ======================================================== ;;
(define-syntax omatch
  (syntax-rules ()
    [(_ omprog start input ns) ;;=>
     (interp/fresh-memo omprog (quote start) (construct-stream input) '() ns)]
    [(_ omprog start input) ;;=>
     (interp/fresh-memo omprog (quote start) (construct-stream input) '())]))

;; (define-syntax-rule (ometa rule ...)
;;   (desugar `(rule ...)))

(define-syntax ometa
  (syntax-rules (<<)
    [(_ (<< parent ...) rule ...) (desugar `(rule ...) `(parent ...)) ]
    [(_ rule ...) (desugar `(rule ...))]))

;; (define-syntax-rule (define-ometa name rule ...)
;;   (define name (desugar `(rule ...))))

(define-syntax-rule (define-ometa name rule ...)
  (define name (ometa rule ...)))

(define-syntax-rule (define-ometa-namespace ns-name)
  (begin (define-namespace-anchor a)
         (define ns-name (namespace-anchor->namespace a))))

;; ======================================================== ;;
;; Interpreter (match)                                      ;;
;; ======================================================== ;;
(define (interp/fresh-memo omprog start-rule stream [store '()] [ns ns])
  (fresh-memo!)
  (interp omprog start-rule stream store ns))

(define (interp omprog start stream store ns)
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

  ;; (trace rule-apply)

  (define (e exp stream store)
    ;; -> (value stream store)
    ;; -> ('FAIL fail-list stream store)
    (match
      (case (car exp)
        ((apply) (let* ((rule-expr (cadr exp))
                        (rule-name-temp (gensym '^rule))
                        (rule-args (cddr exp))
                        (old-memo  (memo-copy))
                        (memo-restore! (lambda () (reset-memo! old-memo))))
                   (debug-pre-apply rule-expr stream store)
                   (let ((ans (match rule-expr
                                ;; inheritance or foreign invocation
                                [`(^ ,name ,from-ometa) ;;=>
                                 (interp/fresh-memo
                                  (cons `(,rule-name-temp (apply ,name ,@rule-args))
                                        ;; from-ometa is just a symbol that needs
                                        ;; to be evaled in current ns to bind it
                                        ;; to a foreign ometa definition
                                        (eval from-ometa ns))
                                  rule-name-temp stream (fresh-store) ns)]
                                ;; simple case - no calls to foreign ometa programs
                                [rule-name ;;=>
                                 (rule-apply rule-name rule-args stream (fresh-store))])))
                     ;; unless applying a left-recursive rule (growing)
                     ;; restore the memo so that reapplying the same
                     ;; parameterized rule with different arguments
                     ;; in (alt (apply r 1) (apply r 2)) works
                     (unless (and (memo rule-expr stream)
                                  (m-lr-detected? (memo rule-expr stream)))
                       (memo-restore!))
                     (debug-post-apply rule-expr stream store ans)
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
                         (match (interp subprog temprule substream store ns)
                           [(list val (? empty? s) st) (list (de-index-list substream) (cdr stream) st)]
                           [(list 'FAIL faillist s st) (list 'FAIL (cdr faillist) s st)] ;don't report `(apply temprule)'
                           [substream-is-too-long (fail/empty stream store)])]
                        [(list pos (? (compose not list?))) (fail/empty stream store)]
                        [ oops (error "Stream cell must contain a Value" (car stream))]))))
        )
      [(list 'FAIL faillist s st) (fail exp stream st faillist)]
      [result result]))
  (e `(apply ,start) stream store))

(define (desugar-e e [inheritance-chain '()])
  (define i inheritance-chain)
  (match e
    ;; seq* into nested seq
    [`(seq* ,e1) (desugar-e e1 i)]
    [`(seq* ,e1 ,e2) `(seq ,(desugar-e e1 i) ,(desugar-e e2 i))]
    [`(seq* ,e1 ,e2 ...) `(seq ,(desugar-e e1 i) ,(desugar-e `(seq* ,@e2) i))]
    ;; alt* into nested alt
    [`(alt* ,e1) (desugar-e e1 i)]
    [`(alt* ,e1 ,e2) `(alt ,(desugar-e e1 i) ,(desugar-e e2 i))]
    [`(alt* ,e1 ,e2 ...) `(alt ,(desugar-e e1 i) ,(desugar-e `(alt* ,@e2) i))]
    [`(many ,e1) `(many ,(desugar-e e1 i))]
    [`(many1 ,e1) `(many1 ,(desugar-e e1 i))]
    [`(many+ ,e1) (let ((a (gensym '+a))
                        (rest (gensym '+rest))
                        (body (desugar-e e1 i)))
                    (desugar-e `(seq* (bind ,a ,body)
                                      (bind ,rest (many ,body))
                                      (-> (cons ,a ,rest)))
                               i))]
    ;; inheritance
    [`(apply (^ ,rule-name) ,args ...) `(apply (^ ,rule-name ,@i) ,@args)]
    [`(bind ,id ,e1) `(bind ,id ,(desugar-e e1 i))]
    [`(~ ,e1) `(~ ,(desugar-e e1 i))]
    [`(list ,e1)`(list ,(desugar-e e1 i))]
    [rest rest]))

(define (desugar omprog [inheritance-chain '()])
  (define (desugar-rule rule)
    (match rule
      [(list name ids ... body)
       ;; =>
       `(,name ,@ids ,(desugar-e body inheritance-chain))]
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


;; ======================================================== ;;
;; Playground                                               ;;
;; ======================================================== ;;

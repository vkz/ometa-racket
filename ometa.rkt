#lang racket

(require "helpers.rkt")

;; TODO:
;; - proper index into stream
;; - tests for each e-case

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

;; Stream == Value ...

;; a == number
;;      character (#\c)
;;      string    ("string")

;; reflective hook to reel the module namespace
;; must insert this into user-code before interpreting
;; otherwise `eval' won't have bindings from user top-level
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

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

(define (de-index-list l)
  (define (de-index node)
    (cond
     ((list? (cadr node)) (de-index-list (cadr node)))
     (else (cadr node))))
  (map de-index l))

;; memoization
;; table-entry: (rule-name stream) -> value
(define table (make-hash))
(define (memo rule-name stream)
  (hash-ref table (list rule-name stream) #f))
(define (memo-add rule-name stream value)
  (hash-set! table (list rule-name stream) value))

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

  (define (rule-apply name stream store)
    ;;(printf "Table: ~v~n" table)
    (let (( r (reverse
               (cond
                ((equal? name 'anything)     (anything stream (fresh-store)))
                ((memo name stream)       => (lambda (memo-entry) memo-entry))
                ((find-rule-by-name name) => (lambda (body)
                                               (define ans (e body stream (fresh-store)))
                                               (memo-add name stream ans)
                                               ans))
                (else                        (error "no such rule " name))))))
      (unless (equal? name 'anything)
        (printf "~a binds: ~v~n" name (car r)))
      (reverse (cons (append (car r) store) (cdr r)))))

  (define (anything stream store)
    (define value cadr)
    (if (empty? stream)
        (fail/empty stream store)
        (list (value (car stream)) (cdr stream) store)))

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
               [(list _ s st) (fail  stream st '())]))

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

(define testprog
  `((A (seq (list (seq (bind x (atom 10))
                       (bind y (apply D))))
            (-> (list x y))))
    (B (list (seq (alt (seq (apply C) (atom 12))
                       (seq (apply C) (atom 13)))
                  (apply anything))))
    (C (atom 12))))

(define input `(10 (12 13 15)))

(printf "Input: ~v\n" input)
(printf "Stream: ~v\n" (construct-stream input))

(let ((ans (interp testprog 'A (construct-stream input) '())))
  (pprint ans)
  (printf "\n\n\n")
  ans)

;; interesting example
;; note the scoping of `x' in *-pattern
;; mulOp = ’*’                                          -> ’mul’
;;       | ’/’                                          -> ’div’,
;; fac   = num:x (mulOp:op num:y -> (x = [op, x, y]))* -> x


;; (define (construct-stream input)
;;   (apply build-list
;;          (match  (cond
;;                   [(string? input) (list string-length string-ref)]
;;                   [(list? input)   (list length list-ref)]
;;                   [(vector? input) (list vector-length vector-ref)])
;;            [(list length-of element-at)
;;             (list (length-of input) (lambda (n) (list n (element-at input n))))])))

;; (define testprog
;;   `((A (seq (bind h (atom #\h))
;;             (seq (seq (bind e (apply C))
;;                       (bind ll (apply B)))
;;                  (-> (begin
;;                        (list h e ll))))))
;;     (B (many (atom #\l)))
;;     (C (seq (bind e (alt (atom #\E) (atom #\e)))
;;             (-> (begin
;;                   (list e #\E)))))
;;     (D (empty))))

;; (define testprog
;;   `((A (seq (bind h (atom #\h))
;;             (seq (seq (bind e (apply C))
;;                       (bind ll (apply B)))
;;                  (-> (list h e ll)))))
;;     (B (many (atom #\l)))
;;     (C (alt (atom #\E) (atom #\e)))
;;     (D (empty))))

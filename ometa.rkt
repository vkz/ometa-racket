#lang racket

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
  (cond
   ((list? input) (list `((-1 0) ,(list->stream input))))
   ((string? input) (string->stream input))))

(define (list->stream l [depth 0])
  (build-list (length l)
              (lambda (n)
                `((,depth ,n) ,(cond
                                ((list? (list-ref l n)) (list->stream (list-ref l n) n))
                                (else (list-ref l n) ))))))

(define (string->stream s [depth 0])
  (build-list (string-length s)
              (lambda (n) `((,depth ,n) ,(string-ref s n)))))

(define (interp omprog start stream store)
  ;; -> Value
  (define fresh-store (lambda () '()))
  (define rules (append omprog '()))
  (define find-rule-by-name (lambda (name) (cadr (assoc name rules))))
  (define exp-name car)

  (define (rule-apply name stream store)
    (reverse
     (cons store
           (cdr (let (( r (reverse
                           (cond
                            ((equal? name 'anything)     (anything stream (fresh-store)))
                            ((find-rule-by-name name) => (lambda (body) (e body stream (fresh-store))))
                            (else                        (error "no such rule " name))))))
                  (unless (equal? name 'anything)
                    (printf "~a BINDS: ~v~n" name (car r)))
                  r)))))

  (define (anything stream store)
    (if (empty? stream)
        (fail '(apply anything) stream store '())
        (list (car stream) (cdr stream) store)))

  (define (de-pos binding)
    (match binding
      [(list id (list (list pos v) ...)) `(,id (quote ,v))]
      [(list id (list pos v)) `(,id (quote ,v))]))

  (define (store->env a-list)
    (map de-pos a-list))

  (define (fail e stream store faillist)
    ;; -> ('FAIL     fail-list         stream store)
    ;; -> ('FAIL ((e index value) ...) stream store)
    (cond
     ((empty? stream) (list 'FAIL (cons (list e 'END '_)      faillist) stream store))
     (else            (list 'FAIL (cons (cons e (car stream)) faillist) stream store))))

  (define (e exp stream store)
    ;; -> ((pos * value or FAIL) * stream * store)
    (case (exp-name exp)
      ((apply) (rule-apply (cadr exp) stream store))
      ((empty) (list `((+inf.0 +inf.0) NONE) stream store))

      ((seq) (match (e (second exp) stream store)
               [(list 'FAIL faillist s st) (fail exp stream st faillist)]
               [(list  val  s st)
                (match  (e (third exp) s st)
                  [(list 'FAIL faillist2 s2 st2) (fail exp stream st2 faillist2)]
                  [ result result])]))

      ((atom) (begin
                (define a? (lambda (b) (equal? b (cadr exp))))
                (define not-a? (lambda (b) (not (a? b))))
                (define val (e `(apply anything) stream store))
                (match val
                  [(list 'FAIL faillist s st)      (fail exp stream store '())]
                  [(list `(,pos 'NONE) s st)       (fail exp stream store '())]
                  [(list `(,pos `(,_)) s st)       (fail exp stream store '())]
                  [(list `(,pos ,(? not-a?)) s st) (fail exp stream store '())]
                  [(list `(,pos ,(? a? a)) s st)   (list `(,pos ,a) s st)])))

      ((alt) (match (e (second exp) stream store)
               [(list 'FAIL faillist s st)
                (match  (e (third exp) stream st)
                  [(list 'FAIL faillist2 s2 st2) (fail exp stream st2 faillist2)]
                  [ result result])]
               [result result]))

      ((many) (match (e (second exp) stream store)
                [(list 'FAIL faillist s st) (list '() stream st)] ;; try e -> if 'FAIL then return '()
                [_ (e `(many1 ,(second exp)) stream store)]))
      ((many1) (match (e (second exp) stream store)
                 [(list 'FAIL faillist s1 st1) (list '() stream st1)]
                 [(list v1 s1 st1) (match (e `(many1 ,(second exp)) s1 st1)
                                     [(list v-rest s-rest st-rest)
                                      (list (append `(,v1) v-rest) s-rest st-rest)])]))

      ((~) (match (e (second exp) stream store)
             [(list 'FAIL faillist s st) (list `((+inf.0 +inf.0) NONE) stream st)]
             [(list _ s st) (fail exp stream st '())]))

      ((bind) (match (e (third exp) stream store)
                [(list 'FAIL faillist s st) (fail exp stream st faillist)]
                [(list val s st) (list val s (cons (list (second exp) val) st))]))
      ((->)   (begin
                (define env (store->env store))
                (define code (second exp))
                (define result (eval `(let* ,(reverse env) ,code) ns))
                (list (list `(+inf.0 +inf.0) result) stream store)))
      ((list) (begin
                (define temprule (gensym "RULE"))
                (define list-pattern (second exp))
                (define subprog (cons (list temprule list-pattern) rules))
                (match (car stream)
                  [(list pos (? list? subinput))
                   (match (interp subprog temprule subinput store)
                     [(list 'FAIL faillist s st) (fail exp stream st faillist)]
                     [(list val s st) (if (empty? s) ;list-pattern must match entire input list
                                          (list (list `(+inf.0 +inf.0) subinput) (cdr stream) st)
                                          (fail exp stream st '()))])]
                  [(list pos (? (compose not list?))) (fail exp stream store '())]
                  [ oops (error "Stream cell must contain a Value" (car stream))])))

      ))
  (match (e `(apply ,start) stream store)
    [(list 'FAIL faillist s st) (fail `(apply ,start) stream st faillist)]
    [result result ]))

(define testprog
  `((A (list (seq (atom 10)
                  (apply B))))
    (B (seq (list (many (apply anything)))
            (apply anything)))))

(define input `(10 (11 12)))


(printf "Input: ~v\n" input)
(printf "Stream: ~v\n" (construct-stream input))

(interp testprog 'A (construct-stream input) '())


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

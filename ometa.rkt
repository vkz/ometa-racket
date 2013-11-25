#lang racket

;; e ==(empty)
;;     (atom a)
;;     (apply A)
;;     (seq e1 e2)
;;     (alt e1 e2)
;;     (many e)
;;     (~ e)
;;     (bind x e)
;;     (-> t)
;;     ( e )

;; Value == a
;;         (Value ...)
;;          none

;; Stream == Value ...

(define (interp omprog start input store)
  ;; -> Value
  (define stream (construct-stream input))
  (define fresh-store (lambda () '()))
  (define rules (append omprog '()))
  (define find-rule-by-name (lambda (name) (cadr (assoc name rules))))
  (define exp-name car)


  (define (rule-apply name stream store)
    (reverse
     (cons store
           (cdr (reverse
                 (cond
                  ((equal? name 'anything)     (anything stream (fresh-store)))
                  ((find-rule-by-name name) => (lambda (body) (e body stream (fresh-store))))
                  (else                        (error "no such rule " name))))))))

  (define (anything stream store)
    (if (empty? stream)
        (list 'FAIL stream store)
        (list (car stream) (cdr stream) store)))

  (define (e exp stream store)
    (case (exp-name exp)
      ((apply) (rule-apply (cadr exp) stream store))
      ((empty) (list 'NONE stream store))
      ((seq) (match (e (second exp) stream store)
               [(list 'FAIL s st) (list 'FAIL stream st)]
               [(list  val  s st) (e (third exp) s st)]))
      ((atom) (begin
                (define a? (lambda (b) (equal? b (cadr exp))))
                (define not-a? (lambda (b) (not (a? b))))
                (define val (e `(apply anything) stream store))
                (printf "Matching ~a~n" val)
                (match val
                  [(list 'FAIL s st)               (begin (displayln 1) (list 'FAIL stream store))]
                  [(list 'NONE s st)               (begin (displayln 2) (list 'FAIL stream store))]
                  [(list `(,pos `(,__)) s st)      (begin (displayln 3) (list 'FAIL stream store))]
                  [(list `(,pos ,(? not-a?)) s st) (begin (displayln 4) (list 'FAIL stream store))]
                  [(list `(,pos ,(? a? b )) s st)  (begin (displayln 5))(list b s st)])))

      ))

  (match (e `(apply ,start) stream store)
    [(list 'FAIL s st) (list 'FAIL stream st)]
    [result result ]))

(define (construct-stream input)
  (cond
   [(string? input)
    (build-list (string-length input)
                (lambda (n) (list n (string-ref input n))))]
   [(list? input)
    (build-list (length input)
                (lambda (n) (list n (list-ref input n))))]
   [(vector? input)
    (build-list (vector-length input)
                (lambda (n) (list n (vector-ref input n))))]))

(define testprog
  `((A (apply B))
    (B (seq (atom #\h) (apply C)))
    (C (seq (atom #\e) (apply anything)))))

(interp testprog 'A "hel" '((x 0)))

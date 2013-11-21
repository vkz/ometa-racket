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

      ))

  (e `(apply ,start) stream store))

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
    (B (seq (apply anything) (apply C)))
    (C (seq (apply anything) (apply anything)))))

(interp testprog 'A "he" '((x 0)))

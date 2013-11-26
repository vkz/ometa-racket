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

;; reflective hook to reel the module namespace
;; would need to insert this into module with
;; ometa user-code so that `eval' can see top-level bindings
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))


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
           (cdr (let (( r (reverse
                           (cond
                            ((equal? name 'anything)     (anything stream (fresh-store)))
                            ((find-rule-by-name name) => (lambda (body) (e body stream (fresh-store))))
                            (else                        (error "no such rule " name))))))
                  (unless (equal? name 'anything)
                    (printf "~a binds: ~a~n" name (car r)))
                  r)))))

  (define (anything stream store)
    (if (empty? stream)
        (list 'FAIL stream store)
        (list (car stream) (cdr stream) store)))

  (define (de-pos binding)
    (match binding
      [(list id (list (list pos v) ...)) `(,id (quote ,v))]
      [(list id (list pos v)) (list id v)]))

  (define (store->env a-list)
    (map de-pos a-list))

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
                  [(list `(,pos ,(? a? b )) s st)  (begin (displayln 5))(list `(,pos ,b) s st)])))
      ((alt) (match (e (second exp) stream store)
               [(list 'FAIL s st) (e (third exp) stream st)]
               [result result]))
      ((many) (match (e (second exp) stream store)
                [(list 'FAIL s st) (list 'FAIL stream st)] ;; try e -> if 'FAIL then e* failed
                [_ (e `(many1 ,(second exp)) stream store)]))
      ((many1) (match (e (second exp) stream store)
                 [(list 'FAIL s1 st1) (list '() stream st1)]
                 [(list v1 s1 st1) (match (e `(many1 ,(second exp)) s1 st1)
                                     [(list v-rest s-rest st-rest)
                                      (list (append `(,v1) v-rest) s-rest st-rest)])]))
      ((~) (match (e (second exp) stream store)
             [(list 'FAIL s st) (list 'NONE stream st)]
             [(list _ s st) (list 'FAIL stream st)]))
      ((bind) (match (e (third exp) stream store)
                [(list 'FAIL s st) (list 'FAIL stream st)]
                [(list val s st) (list val s (cons (list (second exp) val) st))]))
      ((->)   (begin
                ;; capturing the right name-space for eval is crazy
                ;; this here is a temporary hack and has to go
                ;; we should be capturing user ometa-code name-space
                (define env (store->env store))
                (define code (second exp))
                (define result (eval `(let* ,(reverse env)
                                        ,code)
                                     ns))
                (list (list +inf.0 result)
                      stream
                      store)))
      ))
  (match (e `(apply ,start) stream store)
    [(list 'FAIL s st) (list 'FAIL stream st)]
    [result result ]))

(define (construct-stream input)
  (apply build-list
         (match  (cond
                  [(string? input) (list string-length string-ref)]
                  [(list? input)   (list length list-ref)]
                  [(vector? input) (list vector-length vector-ref)])
           [(list len ref)
            (list (len input) (lambda (n) (list n (ref input n))))])))


(define testprog
  `((A (seq (bind h (atom #\h))
            (seq (seq (bind e (apply C))
                      (bind ll (apply B)))
                 (-> (begin
                       (printf "AAA:~v~n" e)
                       (list h e ll))))))
    (B (many (atom #\l)))
    (C (seq (bind e (alt (atom #\E) (atom #\e)))
            (-> (begin
                  (printf "Matched e:~v\n" (list e #\E))
                  (list e #\E)))))
    (D (empty))))

;; (define testprog
;;   `((A (seq (bind h (atom #\h))
;;             (seq (seq (bind e (apply C))
;;                       (bind ll (apply B)))
;;                  (-> (list h e ll)))))
;;     (B (many (atom #\l)))
;;     (C (alt (atom #\E) (atom #\e)))
;;     (D (empty))))

(interp testprog 'A "hello" '())


;; (define (construct-stream input)
;;   (cond
;;    [(string? input)
;;     (build-list (string-length input)
;;                 (lambda (n) (list n (string-ref input n))))]
;;    [(list? input)
;;     (build-list (length input)
;;                 (lambda (n) (list n (list-ref input n))))]
;;    [(vector? input)
;;     (build-list (vector-length input)
;;                 (lambda (n) (list n (vector-ref input n))))]))

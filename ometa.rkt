#lang racket

;; TODO:
;; - stream constructors
;; - tests for each e-case
;; - de-pos values

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

;; reflective hook to reel the module namespace
;; must insert this into user-code before interpreting
;; otherwise `eval' won't have bindings from user top-level
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
                    (printf "~a BINDS: ~v~n" name (car r)))
                  r)))))

  (define (anything stream store)
    (if (empty? stream)
        (list 'FAIL stream store)
        (list (car stream) (cdr stream) store)))

  (define (de-pos binding)
    (match binding
      [(list id (list (list pos v) ...)) `(,id (quote ,v))]
      [(list id (list pos v)) `(,id (quote ,v))]))

  (define (store->env a-list)
    (map de-pos a-list))

  (define (e exp stream store)
    ;; -> ((pos value) stream store)
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
                (match val
                  [(list 'FAIL s st)               (list 'FAIL stream store)]
                  [(list 'NONE s st)               (list 'FAIL stream store)]
                  [(list `(,pos `(,__)) s st)      (list 'FAIL stream store)]
                  [(list `(,pos ,(? not-a?)) s st) (list 'FAIL stream store)]
                  [(list `(,pos ,(? a? b )) s st)  (list `(,pos ,b) s st)])))
      ((alt) (match (e (second exp) stream store)
               [(list 'FAIL s st) (e (third exp) stream st)]
               [result result]))
      ((many) (match (e (second exp) stream store)
                [(list 'FAIL s st) (list '() stream st)] ;; try e -> if 'FAIL then return '()
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
                (define env (store->env store))
                (define code (second exp))
                (define result (eval `(let* ,(reverse env)
                                        ,code)
                                     ns))
                (list (list +inf.0 result)
                      stream
                      store)))

      ((list) (begin
                (define temprule (gensym "RULE"))
                (define list-pattern (second exp))
                (define subprog `((,temprule ,list-pattern)))
                (printf "Subprog: ~v\n" subprog)
                (match (car stream)
                  [(list pos (? list? subinput))
                   (begin (printf "LIST PAT\n")
                          (printf "Sub-input: ~v\n" subinput)
                          (printf "Sub-stream: ~v\n" (construct-stream subinput))
                          (match (interp subprog temprule subinput store)
                            [(list 'FAIL s st) (begin (printf "List subpat FAIL\n")
                                                      (list 'FAIL stream st))]
                            [(list val s st) (if (empty? s) ;list-pattern must match entire input list
                                                 (list (list +inf.0 subinput) (cdr stream) st)
                                                 (list 'FAIL stream st))]))]
                  [else (begin (printf "Sub-stream not a list\n")
                               (list 'FAIL stream store))])))

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
  `((A (seq (atom #\h)
            (list (seq (atom #\e)
                       (many (atom #\l))))))))

(define input (list #\h `(,@(string->list "e")) #\o))

(printf "Input: ~v\n" input)
(printf "Stream: ~v\n" (construct-stream input))

(interp testprog 'A input '())


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

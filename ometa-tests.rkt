#lang racket/base
(require rackunit
         rackunit/text-ui
         racket/match
         racket/list
         "ometa.rkt")

;; To write a test case:
;; ---------------------
;; (om-test-case
;;  "Name of the case"
;;  input               ; string or list
;;  (ometa ...)         ; ometa program with Start rule
;;  pattern             ; check-match pattern
;;  predicate)          ; check-match predicate (optional)
;; ---------------------

(define-syntax om-test-case
  (syntax-rules ()
    [(_ case-name input omprog . template)
     (test-case case-name
                (check-match (omatch omprog Start input)
                             . template))]))

;; ================================================= ;;
;; Suit                                              ;;
;; ================================================= ;;
(define left-recursion-suite
  (test-suite
   "Left recursion."

   ;; expr = expr:x - num:y -> (list x y)
   ;;      | expr:x + num:y -> (list x y)
   ;;      | num
   (om-test-case
    "Nested direct left recursion."
    "1+2-3"
    (ometa
     (Start (alt* (seq* (bind x (apply Start))
                        (atom #\-)
                        (bind y (apply N))
                        (-> (list 'sub x y)))
                  (seq* (bind x (apply Start))
                        (atom #\+)
                        (bind y (apply N))
                        (-> (list 'add x y)))
                  (apply N)))
     (N (alt* (atom #\1)
              (atom #\2)
              (atom #\3))))
    ;; Pattern
    (list '(sub (add #\1 #\2) #\3)
          stream
          (list-no-order `(x ,_) `(y ,_) _ ...))
    ;; Predicate
    (null? stream))

   ;; expr = expr:x - num:y -> (list x y)
   ;;      | num
   (om-test-case
    "Direct left recursion"
    "1-2-3"
    (ometa
     (Start (alt (seq (seq (bind x (apply Start))
                           (seq (atom #\-)
                                (bind y (apply N))))
                      (-> (list x y)))
                 (apply N)))
     (N (alt (atom #\1)
             (alt (atom #\2)
                  (atom #\3)))))
    ;; Pattern
    (list '((#\1 #\2) #\3)
          stream
          (list-no-order `(x ,_) `(y ,_) _ ...))
    ;; Predicate
    (null? stream))))

;; ================================================= ;;
;; Suit                                              ;;
;; ================================================= ;;
(define list-suite
  (test-suite
   "Matching on structured data (lists)."

   ;; A = (10:x B:y) -> (list x y)
   ;; B = (C 12 anything)
   ;;   | (C 13 anything)
   ;; C = 12
   (om-test-case
    "Nested list of numbers."
    (10 (12 13 15))
    (ometa
     (Start (seq (list (seq (bind x (atom 10))
                            (bind y (apply B))))
                 (-> (list x y))))
     (B (alt (list (seq (apply C)
                        (seq (atom 12)
                             (apply anything))))
             (list (seq (apply C)
                        (seq (atom 13)
                             (apply anything))))))
     (C (atom 12)))
    ;; Pattern
    (list `(10 (12 13 15))
          (? null?)
          (list-no-order `(x ,_) `(y ,_) _ ...)))

   ;; Not idiomatic OMeta - use example above instead
   ;; Works because of explicit grouping with (seq ...)
   ;; A = (10:x B:y) -> (list x y)
   ;; B = ((seq C 12) | (seq C 13) anything)
   ;; C = 12
   (om-test-case
    "Nested list of numbers. Alt inside expression."
    (10 (12 13 15))
    (ometa
     (Start (seq (list (seq (bind x (atom 10))
                            (bind y (apply B))))
                 (-> (list x y))))
     (B (list (seq (alt (seq (apply C) (atom 12))
                        (seq (apply C) (atom 13)))
                   (apply anything))))
     (C (atom 12)))
    ;; Pattern
    (list `(10 (12 13 15))
          (? null?)
          (list-no-order `(x ,_) `(y ,_) _ ...)))))

;; ================================================= ;;
;; Suit                                              ;;
;; ================================================= ;;
(define string-suite
  (test-suite
   "Matching strings"

   (test-case
    "Unlimited seq and alt"
    (let ((input "abc")
          (testprog
           `((Start (seq* (bind a (atom #\a))
                          (bind b (atom #\b))
                          (bind c (apply B))
                          (-> (list a b c))))
             (B (alt* (atom #\a)
                      (atom #\b)
                      (atom #\c))))))
      (check-match (interp (desugar testprog) 'Start (construct-stream input) '())
                  (list `(#\a #\b #\c)
                        (? null?)
                        (list-no-order `(a ,_) `(b ,_) `(c ,_))))))

   ;; (test-case
   ;;  "End of stream"
   ;;  (let ((input "abc")
   ;;        (testprog
   ;;         `((Start (seq* (atom #\a)
   ;;                        (~ (apply anything)))))))
   ;;    (check-match (interp (desugar testprog) 'Start (construct-stream input) '())
   ;;                 (list 'FAIL _ ...))))

   ;; (om-test-case
   ;;  "Unlimited seq and alt"
   ;;  "abc"
   ;;  (ometa
   ;;   (Start (seq* (bind a (atom #\a))
   ;;                (bind b (atom #\b))
   ;;                (bind c (apply B))
   ;;                (-> (list a b c))))
   ;;   (B (alt* (atom #\a)
   ;;            (atom #\b)
   ;;            (atom #\c))))
   ;;  ;; Pattern
   ;;  (list `(#\a #\b #\c)
   ;;        (? null?)
   ;;        (list-no-order `(a ,_) `(b ,_) `(c ,_))))

   ;; (om-test-case
   ;;  "End of stream"
   ;;  "abc"
   ;;  (ometa
   ;;   (Start (seq* (atom #\a)
   ;;                (~ (apply anything)))))
   ;;  ;; Pattern
   ;;  (list 'FAIL _ ...))

   ))


;; (define input "abc")
;; (define input "ab") ;fail
;; (define input "a")
;; (define testprog
;;   ;; look-ahead and end of stream
;;   (ometa
;;    (A (seq* (atom #\a) (apply B)))
;;    (B (alt* (seq* (atom #\b) (~ (~ (atom #\c))))
;;             (apply END)))
;;    (END (~ (apply anything)))))

(define delim (list->string (build-list 51 (lambda (n) #\-))))

(printf "Testing strings:~n~a~n" delim)
(run-tests string-suite)

(printf "Testing lists:~n~a~n" delim)
(run-tests list-suite)

(printf "Testing left recursion:~n~a~n" delim)
(run-tests left-recursion-suite)

;; interesting example
;; note the scoping of `x' in *-pattern
;; mulOp = ’*’                                          -> ’mul’
;;       | ’/’                                          -> ’div’,
;; fac   = num:x (mulOp:op num:y -> (x = [op, x, y]))* -> x

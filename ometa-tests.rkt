#lang racket/base
(require rackunit
         rackunit/text-ui
         racket/match
         racket/list
         "helpers.rkt"
         "ometa.rkt")

;; Write your test case to a suite:
;; ===============================
;; (om-test-case
;;  "Name of the case"
;;  input               ; string or list
;;  (ometa ...)         ; ometa program with Start rule
;;  pattern             ; check-match pattern
;;  predicate)          ; check-match predicate (optional)

;; Make sure your suite is run:
;; ===========================
;; Add your suits to (run-suites ...) in the `Run tests' section

(define-syntax om-test-case
  (syntax-rules ()
    [(_ case-name input omprog . template)
     (test-case case-name
                (check-match (omatch omprog Start input)
                             . template))]))

(define-syntax-rule (run-suites suite ...)
  (let ((delim (list->string (build-list 51 (lambda (n) #\-)))))
    (begin
      (printf "~n Testing ~a~n~a~n" (quote suite) delim)
      (run-tests suite)
      (newline))
    ...))


;; ================================================= ;;
;; Suite: left recursion                             ;;
;; ================================================= ;;
(define left-recursion-suite
  (test-suite
   "Left recursion."

   ;; ------------------------------------------ ;;

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

   ;; ------------------------------------------ ;;

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
;; Suite: matching lists                             ;;
;; ================================================= ;;
(define list-suite
  (test-suite
   "Matching on structured data (lists)."

   ;; ------------------------------------------ ;;

   (om-test-case
    "Empty list."
    '()
    (ometa
     (Start (list (apply End)))
     (End   (~ (apply anything))))
    ans
    (success? ans))

   (om-test-case
    "Empty list, non-empty rule."
    '()
    (ometa
     (Start (list (list (apply End))))
     (End   (~ (apply anything))))
    ans
    (fail? ans))


   ;; ------------------------------------------ ;;

   ;; A = (10:x B:y) -> (list x y)
   ;; B = (C 12 anything)
   ;;   | (C 13 anything)
   ;; C = 12
   (om-test-case
    "Nested list of numbers."
    '(10 (12 13 15))
    (ometa
     (Start (seq* (list (seq* (bind x (atom 10))
                              (bind y (apply B))))
                  (-> (list x y))))
     (B (alt* (list (seq* (apply C)
                          (atom 12)
                          (apply anything)))
              (list (seq* (apply C)
                          (atom 13)
                          (apply anything)))))
     (C (atom 12)))
    ;; Pattern
    (list `(10 (12 13 15))
          (? null?)
          (list-no-order `(x ,_) `(y ,_) _ ...)))

   ;; ------------------------------------------ ;;

   ;; Not idiomatic OMeta - use example above instead
   ;; Works because of explicit grouping with (seq ...)
   ;; A = (10:x B:y) -> (list x y)
   ;; B = ((seq C 12) | (seq C 13) anything)
   ;; C = 12
   (om-test-case
    "Nested list of numbers. Alt inside expression."
    '(10 (12 13 15))
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
;; Suite: matching strings                           ;;
;; ================================================= ;;
(define string-suite
  (test-suite
   "Matching strings"

   ;; ------------------------------------------ ;;

   (om-test-case
    "Empty stream"
    ""
    (ometa
     (Start (~ (apply anything))))
    ans
    (success? ans))


   (om-test-case
    "Empty stream"
    "a"
    (ometa
     (Start (~ (apply anything))))
    ans
    (fail? ans))

   ;; ------------------------------------------ ;;

   (om-test-case
    "Unlimited seq and alt"
    "abc"
    (ometa
     (Start (seq* (bind a (atom #\a))
                  (bind b (atom #\b))
                  (bind c (apply B))
                  (-> (list a b c))))
     (B (alt* (atom #\a)
              (atom #\b)
              (atom #\c))))
    ;; Pattern
    (list `(#\a #\b #\c)
          (? null?)
          (list-no-order `(a ,_) `(b ,_) `(c ,_))))

   ;; ------------------------------------------ ;;

   (om-test-case
    "End of stream"
    "abc"
    (ometa
     (Start (seq* (atom #\a)
                  (~ (apply anything)))))
    ;; Pattern
    (list 'FAIL _ ...))

   (om-test-case
    "Look-ahead and end of stream 1"
    "abc"
    (ometa
     (Start (seq* (atom #\a) (apply B)))
     (B     (alt* (seq* (atom #\b) (~ (~ (atom #\c))))
                  (apply END)))
     (END   (~ (apply anything))))
    ans
    (and (success? ans)
         (not (null? (value-stream ans)))))

   ;; ------------------------------------------ ;;

   (om-test-case
    "Look-ahead and end of stream 2 (fail case)"
    "ab"
    (ometa
     (Start (seq* (atom #\a) (apply B)))
     (B     (alt* (seq* (atom #\b) (~ (~ (atom #\c))))
                  (apply END)))
     (END   (~ (apply anything))))
    ans
    (fail? ans))

   ;; ------------------------------------------ ;;

   (om-test-case
    "Look-ahead and end of stream 3"
    "a"
    (ometa
     (Start (seq* (atom #\a) (apply B)))
     (B     (alt* (seq* (atom #\b) (~ (~ (atom #\c))))
                  (apply END)))
     (END   (~ (apply anything))))
    ans
    (success? ans))))

;; ================================================= ;;
;; Run tests                                         ;;
;; ================================================= ;;
(run-suites string-suite
             list-suite
             left-recursion-suite)

#lang racket/base
(require rackunit
         rackunit/text-ui
         racket/match
         racket/list
         "ometa.rkt")

(define left-recursion-suite
  (test-suite
   "Left recursion."

   (test-case
    "Nested direct left recursion."
    ;; ================================================= ;;
    ;; expr = expr:x - num:y -> (list x y)               ;;
    ;;      | expr:x + num:y -> (list x y)               ;;
    ;;      | num                                        ;;
    ;; ================================================= ;;
    (let ((input "1+2-3")
          (testprog
           `((A (alt (alt (seq (seq (bind x (apply A))
                                    (seq (atom #\-)
                                         (bind y (apply N))))
                               (-> (list 'sub x y)))
                          (seq (seq (bind x (apply A))
                                    (seq (atom #\+)
                                         (bind y (apply N))))
                               (-> (list 'add x y))))
                     (apply N)))
             (N (alt (atom #\1)
                     (alt (atom #\2)
                          (atom #\3)))))))
      (check-match
       (interp testprog 'A (construct-stream input) '())
       (list '(sub (add #\1 #\2) #\3)
             stream
             (list-no-order `(x ,_) `(y ,_) _ ...))
       (null? stream))))

   (test-case
    "Direct left recursion"
    ;; ================================================= ;;
    ;; expr = expr:x - num:y -> (list x y)               ;;
    ;;      | num                                        ;;
    ;; ================================================= ;;
    (let (( input "1-2-3")
          (testprog
           `((A (alt (seq (seq (bind x (apply A))
                               (seq (atom #\-)
                                    (bind y (apply N))))
                          (-> (list x y)))
                     (apply N)))
             (N (alt (atom #\1)
                     (alt (atom #\2)
                          (atom #\3)))))))
      (check-match
       (interp testprog 'A (construct-stream input) '())
       (list '((#\1 #\2) #\3)
             stream
             (list-no-order `(x ,_) `(y ,_) _ ...))
       (null? stream))))))

(define list-suite
  (test-suite
   "Matching on structured data (lists)."

   (test-case
    "Nested list of numbers."
    ;; ================================================= ;;
    ;; A = (10:x B:y) -> (list x y)                      ;;
    ;; B = (C 12 anything)                               ;;
    ;;   | (C 13 anything)                               ;;
    ;; C = 12                                            ;;
    ;; ================================================= ;;
    (let (( input `(10 (12 13 15)))
          (testprog
           `((A (seq (list (seq (bind x (atom 10))
                                (bind y (apply B))))
                     (-> (list x y))))
             (B (alt (list (seq (apply C)
                                (seq (atom 12)
                                     (apply anything))))
                     (list (seq (apply C)
                                (seq (atom 13)
                                     (apply anything))))))
             (C (atom 12)))))
      (check-match
       (interp testprog 'A (construct-stream input) '())
       (list `(10 (12 13 15))
             (? null?)
             (list-no-order `(x ,_) `(y ,_) _ ...)))))

   (test-case
    "Nested list of numbers. Alt inside expression."
    ;; ================================================= ;;
    ;; Not idiomatic OMeta - use example above instead   ;;
    ;; Works because of explicit grouping with (seq ...) ;;
    ;; A = (10:x B:y) -> (list x y)                      ;;
    ;; B = ((seq C 12) | (seq C 13) anything)            ;;
    ;; C = 12                                            ;;
    ;; ================================================= ;;
    (let (( input `(10 (12 13 15)))
          (testprog
           `((A (seq (list (seq (bind x (atom 10))
                                (bind y (apply B))))
                     (-> (list x y))))
             (B (list (seq (alt (seq (apply C) (atom 12))
                                (seq (apply C) (atom 13)))
                           (apply anything))))
             (C (atom 12)))))
      (check-match
       (interp testprog 'A (construct-stream input) '())
       (list `(10 (12 13 15))
             (? null?)
             (list-no-order `(x ,_) `(y ,_) _ ...)))))))


(define string-suite
  (test-suite
   "Matching on lists (stream of characters)."

   (let ((input "hello")
         (testprog
          `((A (seq (bind h (atom #\h))
                    (seq (seq (bind e (apply C))
                              (bind ll (apply B)))
                         (-> (list->string (flatten (list h e ll)))))))
            (B (many (atom #\l)))
            (C (alt (atom #\E) (atom #\e)))
            (D (empty)))))
     (check-match
      (interp testprog 'A (construct-stream input) '())
      (list "hell"
            stream
            (list-no-order `(e ,_) `(ll ,_) _ ...))
      (not (null? stream))))))

(run-tests left-recursion-suite)
(run-tests list-suite)
(run-tests string-suite)


;; interesting example
;; note the scoping of `x' in *-pattern
;; mulOp = ’*’                                          -> ’mul’
;;       | ’/’                                          -> ’div’,
;; fac   = num:x (mulOp:op num:y -> (x = [op, x, y]))* -> x

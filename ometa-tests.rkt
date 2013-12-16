#lang racket/base
(require rackunit
         rackunit/text-ui
         racket/match
         racket/list
         "helpers.rkt"
         "ometa.rkt")

(debug-off!)

;; Add your test cases to a suite:
;; ===============================
;; (om-test-case
;;  "Name of the case"
;;  input               ; string or list
;;  (ometa ...)         ; ometa program with Start rule
;;  pattern             ; check-match pattern
;;  predicate)          ; check-match predicate (optional)

;; Make them run:
;; ===========================
;; Add your suits to (run-suites ...)

(define-ometa-namespace ns)

(define-syntax om-test-case
  (syntax-rules ()
    [(_ case-name input omprog . template)
     (test-case case-name
                (check-match (omatch omprog Start input ns)
                             . template))]))

(define-syntax-rule (run-suites suite ...)
  (lambda ()
    (let ((delim (lambda (l) (list->string (build-list l (lambda (n) #\*))))))
      (begin
        (printf "~n~a~n~a~n"
                (quote suite)
                (delim (string-length (symbol->string (quote suite)))))
        (run-tests suite)
        (newline))
      ...)))

;; ================================================= ;;
;; Add your suites here                              ;;
;; ================================================= ;;
(define suites-to-run
  (run-suites string-suite
              list-suite
              left-recursion-suite
              scoping-suite
              binding-suite
              semantic-suite
              parameterized-rules-suite
              oo-suite
              ))

;; ================================================= ;;
;; Suite: bindings                                   ;;
;; ================================================= ;;
(define binding-suite
  (test-suite
   "Binding."

   ;; ------------------------------------------ ;;
   (om-test-case
    "Simple binding."
    '(1 2)
    (ometa
     (Start (seq* (list
                   (seq* (bind x (atom 1))
                         (bind y (atom 2))))
                  (-> (list x y)))))
    (list (list 1 2)
          _
          (list-no-order `(x ,_) `(y ,_) _ ...)))


   ;; ------------------------------------------ ;;
   (om-test-case
    "Binding call to apply."
    '(1 (2 3))
    (ometa
     (Start (seq*
             (list
              (seq*
               (bind x (apply anything))
               (bind y (alt* (atom 3)
                             (apply Start)))))
             (-> (list x y)))))
    (list '(1 (2 3))
          _
          (list-no-order `(x ,_) `(y ,_) _ ...)))


   )
  )

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
          _)
    ;; Predicate
    (null? stream))

   ;; ------------------------------------------ ;;

   ;; expr = expr:x - num:y -> (list x y)
   ;;      | num
   (om-test-case
    "Direct left recursion"
    "1-2-3"
    (ometa
     (Start (alt* (seq* (bind x (apply Start))
                        (atom #\-)
                        (bind y (apply N))
                        (-> (list x y)))
                  (apply N)))
     (N (alt* (atom #\1)
              (atom #\2)
              (atom #\3))))
    ;; Pattern
    (list '((#\1 #\2) #\3)
          stream
          _)
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
    "Empty list match."
    '()
    (ometa
     (Start (list (apply End)))
     (End   (~ (apply anything))))
    ans
    (success? ans))

   (om-test-case
    "Empty list don't match."
    '()
    (ometa
     (Start (list (list (apply End))))
     (End   (~ (apply anything))))
    ans
    (fail? ans))


   (om-test-case
    "Nested empty lists match."
    '(())
    (ometa
     (Start (list (list (apply End))))
     (End   (~ (apply anything))))
    ans
    (success? ans))

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
          _))

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
     (Start (seq* (list (seq* (bind x (atom 10))
                              (bind y (apply B))))
                  (-> (list x y))))
     (B (list (seq* (alt* (seq* (apply C) (atom 12))
                          (seq* (apply C) (atom 13)))
                    (apply anything))))
     (C (atom 12)))
    ;; Pattern
    (list `(10 (12 13 15))
          (? null?)
          _))))

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
          _))

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
;; Suite: scope                                      ;;
;; ================================================= ;;
(define scoping-suite
  (test-suite
   "Lexical scoping and bindings."

   ;; ------------------------------------------ ;;
   (om-test-case
    "Lexical scope"
    '(1 2)
    (ometa
     (Start (seq*
               (list (bind l (apply inside)))
               (-> l)))
     (inside (alt*
              (seq* (bind x (apply anything))
                    (bind t (apply inside))
                    (-> (cons x t)))
              (apply end)))
     (end    (seq*
              (~ (apply anything))
              (-> null))))
    (list '(1 2) _ ...))

   ;; ------------------------------------------ ;;
   (om-test-case
    "Flatten a nested list."
    '(1 (2 (3 4)) (((5)) 6))
    (ometa
     (Start (seq*
             (list (bind xs (apply inside)))
             (-> xs)))
     (inside  (alt* (seq*
                     (list (bind xs (apply inside)))
                     (bind ys (apply inside))
                     (-> (append xs ys)))
                    (seq*
                     (bind x (apply anything))
                     (bind xs (apply inside))
                     (-> (cons x xs)))
                    (seq*
                     (apply end)
                     (-> null))))
     (end  (seq*
            (~ (apply anything))
            (-> null))))
    (list '(1 2 3 4 5 6)
          (? empty?)
          _))

   ))

;; ================================================= ;;
;; Semantic actions and predicates                   ;;
;; ================================================= ;;
(define semantic-suite
  (test-suite
   "Actions and predicates."

   ;; ------------------------------------------ ;;
   (om-test-case
    "Lower-case word."
    "abc"
    (ometa
     (char (seq* (bind c (apply anything))
                 (->? (char? c))
                 (-> c)))
     (lower (seq* (bind c (apply char))
                  (->? (char<=? #\a c #\z))
                  (-> c)))
     (end  (~ (apply anything)))
     (Start (seq* (bind word (many (apply lower)))
                  (apply end)
                  (-> (list->string word)))))
    (list "abc"
          _
          _))

   ;; ------------------------------------------ ;;
   (om-test-case
    "Lower-case word fail."
    "abC"
    (ometa
     (char (seq* (bind c (apply anything))
                 (->? (char? c))
                 (-> c)))
     (lower (seq* (bind c (apply char))
                  (->? (char<=? #\a c #\z))
                  (-> c)))
     (end  (~ (apply anything)))
     (Start (seq* (bind word (many (apply lower)))
                  (apply end)
                  (-> (list->string word)))))
    ans
    (fail? ans))))

;; ================================================= ;;
;; Semantic actions and predicates                   ;;
;; ================================================= ;;
(define parameterized-rules-suite
  (test-suite
   "Parameterized rules."

   ;; ------------------------------------------ ;;
   (om-test-case
    "Lower-case letters character class."
    "f"
    (ometa
     (char-range x y
                 (seq* (bind c (apply anything))
                       (->? (and (char? c)
                                 (char<=? x c y)))
                       (-> c)))
     (Start (seq* (bind c (apply char-range #\a #\z))
                  (-> c))))
    (list #\f _ _))

   ;; ------------------------------------------ ;;
   (om-test-case
    "Lower-case letters character class."
    "F"
    (ometa
     (char-range x y
                 (seq* (bind c (apply anything))
                       (->? (and (char? c)
                                 (char<=? x c y)))
                       (-> c)))
     (Start (seq* (bind c (apply char-range #\a #\z))
                  (-> c))))
    ans
    (fail? ans))

   ;; ------------------------------------------ ;;
   (om-test-case
    "Parsing numbers (many+ digit)."
    "234 "
    (ometa
     (char-range x y
                 (seq* (bind c (apply anything))
                       (->? (and (char? c)
                                 (char<=? x c y)))
                       (-> c)))
     (digit (apply char-range #\0 #\9))
     (Start (seq* (bind num (many+ (apply digit)))
                  (-> (list->string num)))))
    (list "234" _ _))

   ;; ------------------------------------------ ;;
   (om-test-case
    "Invoking the same param-rule with different args in (alt ...)."
    "aAbb AA"
    (ometa
     (char-range x y
                 (seq* (bind c (apply anything))
                       (->? (and (char? c)
                                 (char<=? x c y)))
                       (-> c)))
     (letter (alt* (apply char-range #\a #\z)
                   (apply char-range #\A #\Z)))
     (Start (seq* (bind tok (many+ (apply letter)))
              (-> (list->string tok)))))
    (list "aAbb" _ _))

   ;; ------------------------------------------ ;;
   (om-test-case
    "Combine `scannerless' and `scannerful' parsing."
    "  myVar =   56 "
    (ometa
     (eq  (seq* (atom #\=)
                (-> (make-immutable-hash `((kind . =) (value . "="))))))
     (num (seq* (bind ds (many+ (apply digit)))
                (-> (make-immutable-hash `((kind . num) (value . ,(list->string ds)))))))
     (id  (seq* (bind ls (many+ (apply letter)))
                (-> (make-immutable-hash `((kind . id) (value . ,(list->string ls)))))))
     (char-range x y
                 (seq* (bind c (apply anything))
                       (->? (and (char? c)
                                 (char<=? x c y)))
                       (-> c)))
     (digit (apply char-range #\0 #\9))
     (letter (alt* (apply char-range #\a #\z)
                   (apply char-range #\A #\Z)))
     (spaces (many+ (atom #\space)))
     (scanner (seq* (apply spaces)
                    (alt* (apply eq)
                          (apply num)
                          (apply id))))
     (token k (seq* (bind t (apply scanner))
                    (->? (equal? (hash-ref t 'kind #f) k))
                    (-> (hash-ref t 'value))))
     (Start (seq* (bind a (apply token 'id))
                  (bind b (apply token '=))
                  (bind c (apply token 'num))
                  (-> (string-append a b c))))
     )
    (list "myVar=56" _ _))

))

;; ================================================= ;;
;; Suite: Inheritance and foreign rules              ;;
;; ================================================= ;;

;; Must have (define-ometa-namespace ns) at top level.
;; Test-cases will extend rules from these definitions.
(define-ometa std
  (char (seq* (bind c (apply anything))
              (->? (char? c))
              (-> c)))
  (char-range x y
              (seq* (bind c (apply anything))
                    (->? (and (char? c)
                              (char<=? x c y)))
                    (-> c)))
  (letter (alt* (apply char-range #\a #\z)
                (apply char-range #\A #\Z)))
  (digit (apply char-range #\0 #\9))
  (number (many+ (apply digit)))
  (spaces (many+ (atom #\space))))

(define-ometa chars
  (char-range x y
              (seq* (bind c (apply anything))
                    (->? (and (char? c)
                              (char<=? x c y)))
                    (-> c))))

(define-ometa letters
  (letter (alt* (apply (^ char-range chars) #\a #\z)
                (apply (^ char-range chars) #\A #\Z))))

(define oo-suite
  (test-suite
   "Inheritance and foreign rules."

   ;; ------------------------------------------ ;;
   (om-test-case
    "Foreign: extend letters with _."
    "hello_Id"
    (ometa
     (letter (alt* (atom #\_)
                   (apply (^ letter std))))
     (Start (many+ (apply letter))))
    (list '(#\h #\e #\l #\l #\o #\_ #\I #\d) _ _))

   ;; ------------------------------------------ ;;
   (om-test-case
    "Foreign: invoking rules with arguments."
    "F"
    (ometa
     (Start (alt* (apply (^ char-range chars) #\a #\z)
                  (apply (^ char-range chars) #\A #\Z))))
    (list #\F _ _))

   ;; ------------------------------------------ ;;
   (om-test-case
    "Foreign: nested foreign invocation."
    "uhFHa"
    (ometa
     (Start (many+ (apply (^ letter letters)))))
    (list '(#\u #\h #\F #\H #\a) _ _))


   ;; ------------------------------------------ ;;
   (om-test-case
    "Inheritance: extend letters with _."
    "hello_Id"
    (ometa (<< std)
     (letter (alt* (atom #\_)
                   (apply (^ letter))))
     (Start (many+ (apply letter))))
    (list '(#\h #\e #\l #\l #\o #\_ #\I #\d) _ _))

   ;; ------------------------------------------ ;;
   (om-test-case
    "Inheritance: invoking rules with arguments."
    "F"
    (ometa (<< chars)
     (Start (alt* (apply (^ char-range) #\a #\z)
                  (apply (^ char-range) #\A #\Z))))
    (list #\F _ _))

   ;; ------------------------------------------ ;;
   (om-test-case
    "Inherintance: nested foreign invocation."
    "uhFHa"
    (ometa (<< letters)
     (Start (many+ (apply (^ letter)))))
    (list '(#\u #\h #\F #\H #\a) _ _))

   ))

;; ================================================= ;;
;; Run tests                                         ;;
;; ================================================= ;;
(suites-to-run)

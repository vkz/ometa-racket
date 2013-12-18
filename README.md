**[Install](#install)** |
**[Play](#play)** |
**[Test](#test)** |
**[References](#references)**

_**Warning:** This implementation is experimental and incomplete. Everything is subject to change without warning._

## ometa-racket ##

------------------

is an experimental implementation of OMeta with [Racket][racket] as a host. OMeta is a beautiful instrument for experimenting with designs for programming languages - a convenient way to implement lexers, parsers, tree-transformers and extensions thereof.

From [original 2007 paper][07] by **_Alessandro Warth_** and **_Ian Piumarta_**:
>_OMeta, a new object-oriented language for pattern matching.
> OMeta is based on a variant of Parsing Expression Grammars (PEGs) -
> a recognition-based foundation for describing syntax - which we have
> extended to handle arbitrary kinds of data._

This here is a recursive interpreter that closely follows operational semantics described in Alessandro Warth's [dissertation][09]. It dispatches on tagged lists that represent core  OMeta grammar - a deliberately simpleminded approach - think *exploratory* programming to help one grok and intern ideas. Toy or not you'll find most features you'd expect having read original papers:

- [x] matching on strings
- [x] matching on structured data (lists)
- [x] semantic actions and predicates
- [x] parameterized rules
- [x] inheritance
- [x] foreign rule invocation
- [x] directly left-recursive rules (with nesting)
- [ ] higher-order apply
- [ ] pattern-matching on rule parameters
- [ ] indirectly left-recursive rules


## Install ##

-------------

given the experimental nature of this repo it is unlikely to make appearance in Racket packages or collections. Simply clone it and require *ometa.rkt* from your source files. Assuming your ometa-racket files and your source file are in the same directory:

```racket
#lang racket
(require "ometa.rkt")
```


## Play ##

----------

### Syntax ###

**[ometa](#ometa)** |
**[define-ometa](#define-ometa)** |
**[omatch](#omatch)** |
**[define-ometa-namespace](#define-ometa-namespace)**

Technically ometa-racket has enough meat to write a lexer+parser combo for OMeta proper, which uses C-like syntax. I'm working on it. Until then we'll stick with s-expressions. Some of you may even notice just how much it resembles Olin Shivers' [SRE regular-expression notation][sre]. Such is the power of truly beautiful ideas - we keep reinventing them. Current syntax is verbose because I'm only using a low-level core language.

Core grammar for OMeta parsing expressions (e):

    e ==   (empty)
           (atom a)
           (apply A)
           (seq e1 e2)
           (alt e1 e2)
           (many e)
           (~ e)
           (bind x e)
           (-> t)
           (list e)

Extended grammar:

    (apply A)   => (apply A arg ...) ; where non-terminal A == (^ RULE [PARENT])
    (seq e1 e2) => (seq* e ...)      ; sequence of arbitrary length
    (alt e1 e2) => (alt* e ...)      ; arbitrary number of alternatives
    (many e)    => (many+ e)         ; one or more


##### ometa #####
OMeta code is a first-class value, which you can bind to variables, pass around and close over in data:
```racket
(define test-program
        (ometa
          (rule-name e)
          ...))
```

##### define-ometa #####
Or use a shorthand notation:
```racket
(define-ometa test-program
              (rule-name e)
              ...)
```

##### omatch #####
Match against a stream. Stream can be a string or a list. For now these are not lazy sequences and will be fully consumed before matching. Namespace argument is optional:
```racket
(omatch test-program
        rule-name
        stream
        namespace)
```

##### define-ometa-namespace #####
Extend OMeta namespace if you want to refer to your top-level bindings. Don't forget to pass it to [omatch](#omatch). You definitely want this when inheriting from other parsers or invoking foreign rules:
```racket
(define-ometa-namespace ns)
```


### Examples ###

**[std](#std)** |
**[integers](#integers)** |
**[words and decimals](#words-and-decimals)** |
**[flatten a list](#flatten-a-list)** |
**[scanners](#scanners)** |


##### std #####
Without a standard library we'd have to tediously copy/paste the most basic rules to every OMeta project. Let's not do that! Why else have inheritance and access to foreign rules:

```racket
#lang racket
(require "ometa.rkt")

;; a reflective hook so that you have access
;; to your top-level bindings from ometa code
(define-ometa-namespace ns)

(define-ometa std
  (char (seq* (bind c (apply anything))
              (->? (char? c))
              (-> c)))
  ;; why bake character-classes into a language when
  ;; its so expressive that adding them is trivial
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
```

##### integers #####

Let's write a left-recursive rule for parsing integers:

```racket
(define char->number (compose string->number list->string list))

(define-ometa integers (<< std)                         ;inherit from std
  (int (alt* (seq* (bind n (apply int))
                   (bind d (apply (^  digit)))          ;invoke a `digit' parent rule
                   (-> (+ (* n 10) (char->number d))))
             (seq* (bind d (apply (^  digit)))
                   (-> (char->number d))))))

(car (omatch integers int "567" ns)) ;;=> 567
```

##### words and decimals #####

Let's extend rules in [std](#std). We should be able to parse decimals and identifiers with `_`:

```racket
(define-ometa token (<< std)
  (letter (alt* (atom #\_)              ; accept underscore
                (apply (^ letter))))    ; invoke parent rule
  (id (many+ (apply letter)))
  (number (alt* (seq* (bind pre (apply (^ number)))
                      (atom #\.)
                      (bind post (apply (^ number)))
                      (-> `(,@pre #\. ,@post)))
                (apply (^ number)))))

(car (omatch token id "hello_Id" ns))   ;;=> "hello_Id"
(car (omatch token number "57.877" ns)) ;;=> "57.877"
```

##### flatten a list #####
Let's pattern-match on structured data:

```racket
(define input '(1 (2 (3 4)) (((5)) 6)))

(define-ometa flat
  (flatten (seq*
            (list (bind xs (apply inside)))
            (-> xs)))
  (inside  (alt*
            (seq* (list (bind xs (apply inside)))
                  (bind ys (apply inside))
                  (-> (append xs ys)))
            (seq* (bind x (apply anything))
                  (bind xs (apply inside))
                  (-> (cons x xs)))
            (seq* (apply end)
                  (-> null))))
  (end  (seq*
         (~ (apply anything))
         (-> null))))

(car (omatch flat flatten input)) ;;=> '(1 2 3 4 5 6)
```

##### scanners #####
Let's go mental and combine *scannerless* and *scannerful* parsing to parse assignments:

```racket
(define-ometa toks (<< std)
  (eq  (seq* (atom #\=)
             (-> (make-immutable-hash `((kind . =) (value . "="))))))
  (num (seq* (bind n (apply (^ number)))
             (-> (make-immutable-hash `((kind . num) (value . ,(list->string n)))))))
  ;; not just rules inherited from `std'
  ;; let's invoke one from `token'
  (id  (seq* (bind ls (apply (^ id token)))
             (-> (make-immutable-hash `((kind . id) (value . ,(list->string ls)))))))
  (scanner (seq* (apply (^ spaces))
                 (alt* (apply eq)
                       (apply num)
                       (apply id)))))

(define-ometa assignments (<< toks)
  (token k (seq* (bind t (apply (^ scanner)))
                 (->? (equal? (hash-ref t 'kind #f) k))
                 (-> (hash-ref t 'value))))
  (assign (seq* (bind a (apply token 'id))
                (bind b (apply token '=))
                (bind c (apply token 'num))
                (-> (string-append a b c)))))

(car (omatch assignments assign " my_var    = 56" ns)) ;;=> "myvar=56"
```

## Test ##

----------

```sh
>$ racket ometa-tests.rkt
```
will run *rackunit* test-suites.

## References ##

----------------

* [OMeta][07] - original paper by Alessandro Warth and Ian Piumarta
* [Experimenting with Programming Languages][09] - a dissertation by Alessandro Warth
* [ometa-js playground][ometa-js] - try OMeta in the browser
* [Alessandro Warth's][ometa] OMeta page

[racket]:     http://racket-lang.org/
[07]:         http://www.tinlizzie.org/~awarth/papers/dls07.pdf
[09]:         http://www.vpri.org/pdf/tr2008003_experimenting.pdf
[warth]:      http://www.tinlizzie.org/~awarth/
[ometa]:      http://www.tinlizzie.org/~awarth/ometa/
[playground]: http://www.tinlizzie.org/ometa-js/#Sample_Project
[ometa-js]:   https://github.com/alexwarth/ometa-js
[sre]:        http://www.ccs.neu.edu/home/shivers/papers/sre.txt

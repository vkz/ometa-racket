## ometa-racket

is an experimental implementation of *OMeta* with [Racket][racket] as a host. An object-oriented language for pattern-matching *OMeta* is a beautiful instrument for experimenting with designs for programming languages - a convenient way to implement lexers, parsers, tree-transformers and extensions thereof.

From [original 2007 paper][07] by **_Alessandro Warth_** and **_Ian Piumarta_**:
>_OMeta, a new object-oriented language for pattern matching.
> OMeta is based on a variant of Parsing Expression Grammars (PEGs) -
> a recognition-based foundation for describing syntax-which we have
> extended to handle arbitrary kinds of data._

This is a recursive interpreter that closely follows operational semantics described in Alessandro Warth's [Dissertation'09][09] with core OMeta grammar represented as tagged list. I deliberately take a simplistic approach with this very much experimental implementation, yet it does deliver most of the features you'd expect having read original papers:

* matching on strings
* matching on structured data (lists)
* semantic actions and predicates
* parameterized rules
* inheritance
* foreign rule invocation
* directly left-recursive rules (with nesting)
*  ~~higher-order apply~~
*  ~~pattern-matching on rule parameters~~
*  ~~indirectly left-recursive rules~~

## Install

given experimental nature of this repo it's unlikely to make appearance in Racket packages or collections. Simply clone it and make and require it from your source files. Assuming your ometa-racket files and your source are in the same directory:

```racket
#lang racket
(require "ometa.rkt")
```
_**Warning:** This implementation is experimental and incomplete. Everything is subject to change without warning._

## Play

You should be able to run these code snippets without a problem.

### Syntax
Technically ometa-racket has enough meat to write a lexer+parser combo for OMeta proper, which uses C-like syntax. I'm working on it. Until then we'll stick with s-expressions. After all you're browsing Racket code, so I assume it's not a problem. Some of you may even notice just how much ometa-racket code resembles Olin Shivers' [SRE regular-expression notation][sre]. Such is the power of truly beautiful ideas - we keep reinventing them.

OMeta code is a first-class value, which you can bind to variables, pass as arguments and return as results or close over in data.

Wrap OMeta code in *ometa* form and bind it to a variable.

```racket
(define test-program
        (ometa
          (rule-name parsing-expression)
          ...))
```

Or use a shorthand notation.

```racket
(define-ometa test-program
              (rule-name parsing-expression)
              ...)
```

Match against a stream. Stream can be a string or a list. For now these are not lazy sequences and will be fully consumed before matching. Namespace argument is optional.

```racket
(omatch test-program
        rule-name
        stream
        namespace)
```

If you want to refer to your top-level bindings from OMeta code make sure you extend its namespace. Don't forget to pass it to *omatch*. You definitely want this if you want to inherit from other parsers or invoke foreign rules.

```racket
(define-ometa-namespace ns)
```

### Examples

Without a standard library we'd have to tediously copy/paste the most basic rules to every OMeta project. Let's not do that! Why else have inheritance and access to foreign rules.

```racket
#lang racket
(require "ometa.rkt"
         "helpers.rkt")

(debug-off!)

;; a reflective hook so that you have access
;; to your top-level bindings from ometa code
(define-ometa-namespace ns)

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

;; --------------------------------------------
```




## Test

```sh
>$ racket ometa-tests.rkt
```
will run *rackunit* test-suites.

## To do

[racket]:     http://racket-lang.org/
[07]:         http://www.tinlizzie.org/~awarth/papers/dls07.pdf
[09]:         http://www.vpri.org/pdf/tr2008003_experimenting.pdf
[warth]:      http://www.tinlizzie.org/~awarth/
[ometa]:      http://www.tinlizzie.org/~awarth/ometa/
[playground]: http://www.tinlizzie.org/ometa-js/#Sample_Project
[ometa-js]:   https://github.com/alexwarth/ometa-js
[sre]:        http://www.ccs.neu.edu/home/shivers/papers/sre.txt

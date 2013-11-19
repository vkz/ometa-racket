#lang racket

(require racket/match)

(define CORE-OMETA core-ometa)


(core
 (EMPTY)
 (ATOM (or/c char? number? string?))
 (NT string?)
 (AND core? core?)
 (OR core? core?)
 (MANY core?)
 (NOT core?)
 (BIND string? core? )
 (ACTION term?)
 (LIST core?))

(value
 (ATOM (or/c char? number? string?))
 (LIST (MANY value?))
 (NONE))

(term
 (ATOM )
 (LIST (MANY term?))
 (NONE)
 (ID string?))

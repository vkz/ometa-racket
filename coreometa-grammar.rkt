#lang racket
;; G
;; == Rule?*
'(G Rule? ...)

;; Rule
;; == Nterm? Pex?
'(Rule (Nterm? Pex?))

;; Nterm
;; == string?
'(Nterm string?)

;; Atom
;; == char?
;; == number?
;; == string?
'(Atom (or char?
           number?
           string?))

;; Id
;; == string?
'(Id string?)

;; Term
;; == Atom?
;; == Listof Term?
;; == none
;; == Id?
'(Term (or Atom?
           (Term? ...)
           none
           Id?))
;; V
;; == Atom?
;; == Listof V?
;; == none
'(V (or Atom?
        (V? ...)
        none))

;; Pex
'(Pex (or empty
          Atom?
          Nterm?
          (Seq  Pex? Pex?)
          (Alt  Pex? Pex? )
          (Many Pex?)
          (Not  Pex?)
          (Bind Id? Pex? )
          (Action Term?)
          (List Pex?)))

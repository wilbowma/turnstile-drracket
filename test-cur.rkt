#lang cur
(require "turnstile-ide.rkt")

; Requires turnstile-core branch of cur, and cur branch of turnstile.
(define-type Hole : (Type 0))
(begin-for-syntax
  (current-hole-type #'Hole)
  (current-hole-type?
   (syntax-parser
     [~Hole #t]
     [_ #f])))

(define-datatype Nat : Type
  (z : Nat)
  (s : (Π (x : Nat) Nat)))

z

Nat

?

(ann ? : Nat)

;(lambda (x : ?) ?)

((lambda (A : (Type 0)) (lambda (x : A) ?)) ?)
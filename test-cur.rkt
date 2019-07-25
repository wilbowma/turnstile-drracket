#lang cur
(require "turnstile-ide.rkt")

; Requires turnstile-core branch of cur, and cur branch of turnstile.
(define-type Hole : Type)
(begin-for-syntax
  (current-hole-type #'Hole)
  (current-hole-type?
   (lambda (x)
     (syntax-parse x
       [~Hole #t]
       [_ #f]))))

(define-datatype Nat : Type
  (z : Nat)
  (s : (Π (x : Nat) Nat)))

z

Nat

?

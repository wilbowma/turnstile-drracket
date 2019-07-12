#lang turnstile
; demo showing that turnstile does not preserve source locations properly.
(require (only-in turnstile/lang [⇒ =>] [⇐ <=] [≫ >>] [⊢ /-] [≻ >>>]))

(define-base-type Bool)
(define-base-type Nat)

(define-typed-syntax #%datum
  [(_ . b:boolean) >>
   ----------
   ; select at (#%datum to get the type of `#f` later in the file
   [⊢ (#%datum- . b) => Bool]]
  [(_ . n:exact-nonnegative-integer) >>
   -----------
   ; selection at (#%datum to get the type of `5` later in the file
   [⊢ (#%datum- . n) => Nat]]
  [(_ . e) >>
   ----------
   [#:error (type-error #:src #'e #:msg "Unsupported literal: ~v" #'e)]])

#f
5
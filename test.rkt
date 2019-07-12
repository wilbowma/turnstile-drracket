#lang turnstile
(require (only-in turnstile/lang [⇒ =>] [⇐ <=] [≫ >>] [⊢ /-] [≻ >>>]))

(define-base-type Bool)
(define-type-constructor -> #:arity > 0)

(define-typed-syntax (λ ([x:id : A] ...) e) >>
  [[ x >> x- : A] ... ⊢ e >> e- => B]
  ----------------
  [⊢ (λ- (x- ...) e-) => (-> A ... B)])

(define-typed-syntax (#%app e₁ e₂) >>
  [⊢ e₁ >> e₁- => (~-> A ... B)]
  [⊢ e₂ >> e₂- <= A] ...
  ---------------
  [⊢ (#%app- e₁- e₂- ...) => B])


(define-typed-syntax #%datum
  [(_ . b:boolean) >>
   ----------
   [⊢ (#%datum- . b) => Bool]]
  [(_ . e) >>
   ----------
   [#:error (type-error #:src #'e #:msg "Unsupported literal: ~v" #'e)]])

((λ ([x : Bool]) x) #t)

#f
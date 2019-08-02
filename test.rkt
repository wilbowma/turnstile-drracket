#lang turnstile
(require (only-in turnstile/lang [⇒ =>] [⇐ <=] [≫ >>] [⊢ /-] [≻ >>>]))
(require "turnstile-ide.rkt")

(try-to-init-hole)

(define-base-type Bool)
(define-base-type Nat)
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
  [(_ . n:nat) >>
    --------
               [⊢ (#%datum- . b) => Nat]]
  [(_ . e) >>
   ----------
   [#:error (type-error #:src #'e #:msg "Unsupported literal: ~v" #'e)]])


(define-typed-syntax (+ n m) >>
  [⊢ n >> n- ⇒ ~Nat]
   [⊢ m >> m- ⇒ ~Nat]
   ------
   [⊢ (+- n m) ⇒ Nat])

(define-typed-syntax (not b) >>
  [⊢ b >> b- ⇒ ~Bool]
  ----
  [⊢ (not- b) ⇒ Bool])

(define-typed-syntax (begin e1 e2) >>
  [⊢ e1 >> e1- ⇒ _]
  [⊢ e2 >> e2- ⇒ B]
  ------
  [⊢ (begin- e1 e2) ⇒ B])

((λ ([x : Bool]) x) #t)

((λ ([x : Bool]) x) ?)

#f

(? "Something should go here")

(define-typed-variable the-truth #t)

((λ ([x : Bool]) ?) the-truth)

((λ ([x : Bool]) (ann ? : Bool)) #f)

(λ ([x : Hole]) (begin (+ x 5) (not x)))

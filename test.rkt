#lang turnstile
(require (only-in turnstile/lang [⇒ =>] [⇐ <=] [≫ >>] [⊢ /-] [≻ >>>]))
(require (only-in racket/base [error error-]))

(define-base-type Hole)

(define-typed-syntax ?
  [(_ msg:str) >>
   ---------------------
   [⊢ #,(syntax-property #'(#%app- error- (#%datum- . msg))
        'todo (syntax->datum #'msg)) => Hole]]
  [_ >>
   ---------------------
   [⊢ (? "") => Hole]])

(begin-for-syntax
  (define old-relation (current-typecheck-relation))
  ;; Every type is equal to 'Hole
  (current-typecheck-relation
   (lambda (t1 t2)
     (syntax-parse (list t1 t2)
       [(~Hole _) #t]
       [(_ ~Hole) #t]
       [_ ((old-relation) t1 t2)]))))

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

((λ ([x : Bool]) x) ?)

#f

(? "meow")

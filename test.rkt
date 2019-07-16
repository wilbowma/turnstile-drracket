#lang turnstile
(require (only-in turnstile/lang [⇒ =>] [⇐ <=] [≫ >>] [⊢ /-] [≻ >>>]))
(require (only-in racket/base [error error-] define-namespace-anchor))

(define-base-type Hole)

(begin-for-syntax
  (define-namespace-anchor a)
  ;; A symbol 'x represents a typed identifier if:
  ;; - 'x is in the current namespace symbol table
  ;; - (datum->syntax stx 'x) is an id-transforer, where stx is syntax from the current context
  ;; - (local-expand (datum->syntax stx 'x)) gives back a syntax object with a ': property
  (define (identifier-typed? stx sym)
    (and
     (with-handlers ([values (lambda _ #f)])
       #;(namespace-variable-value sym)
       (or
        #;(syntax-local-value (namespace-symbol->identifier sym))
        #;(eq? sym 'x)
        #;(syntax-property (namespace-symbol->identifier sym) ':)
        #;(syntax-property (local-expand (datum->syntax stx sym) 'expression '()) ':)
        (syntax-property (local-expand (namespace-symbol->identifier sym) 'expression '()) ':)))))

  (define (namespace-mapped-typed-symbols stx [ns (current-namespace)])
    (filter (curry identifier-typed? stx)
            (namespace-mapped-symbols ns)))

  ;; namespace doesn't actually give all mapped symbols (particularly those in
  ;; an internal definition context?) so ... let's brute force it.
  #;(require srfi/14)
  #;(define (all-symbols-of-length n)
    (define alpha-set (map ~a (char-set->list char-set:full)))
    ;; at 1, the list of all symbols in the alpha-set
    ;; at 2, append to the recursion: for each element of the recursion, map
    ;; concat onto each element of the recursion to
    (map string->symbol
         (for/fold ([ls alpha-set])
                   ([i (in-range 1 n)])
           (cons
            (flatten
            (for/list ([e ls])
             (for/list ([a alpha-set])
               (string-append a e))))
            ls))))
  #;(define (current-mapped-symbols [n 10])
    (for/fold ([ls '()])
              ([i n])
      (append ls (all-symbols-of-length i))))

  #;(displayln (all-symbols-of-length 1))

  (define (env-to-string ids)
    (for/fold ([str ""])
              ([id (syntax-e ids)])
      (format "~a~a : ~a~n" str (syntax->datum id)
              (type->str (typeof (local-expand id 'expression '())) #;(syntax-property (local-expand id 'expression '()) ':))))))

(begin-for-syntax
  (struct todo-item (full summary) #:prefab))
(define-typed-syntax ?
  ;; Must explicitly list names from the environment that you wish to see as
  ;; part of the hole, for now.
  ;; Default should find all lexical variables, but need to figure out how to
  ;; reflect on those...
  [(_ msg:str (env:id ...)) >>
   ---------------------
   [⊢ #,(begin
          #;(displayln (namespace-mapped-typed-symbols this-syntax))
          (syntax-property
           #'(#%app- error- (#%datum- . msg))
           'todo (todo-item (env-to-string #'(env ...)) (syntax->datum #'msg)))) => Hole]]
  ;; TODO: This disrupts the local environment and the above local-expand trick doesn't work.
  [_:id >>
   ---------------------
   [⊢ (? "Incomplete program; cannot run." ()) => Hole]])

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


((λ ([x : Bool]) x) #t)

((λ ([x : Bool]) x) ?)

#f

(? "meow")

(define-typed-variable the-truth #t)

((λ ([x : Bool]) (? "meow" (x the-truth))) the-truth)

#f

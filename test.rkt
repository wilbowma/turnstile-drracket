#lang turnstile
(require (only-in turnstile/lang [⇒ =>] [⇐ <=] [≫ >>] [⊢ /-] [≻ >>>]))
(require (only-in racket/base [error error-]))
(require (for-syntax pretty-format))
(define-base-type Hole)

;; copy/paste from debug
(begin-for-syntax
  ;; syntax-find-local-variables : Syntax -> (Listof Id)
  (define (syntax-find-local-variables stx)
    (define debug-info (syntax-debug-info stx (syntax-local-phase-level) #t))
    (unless (hash-has-key? debug-info 'bindings)
      (pretty-eprintf
       (string-append
        "warning: debug-repl cannot find the local bindings\n"
        "  debug-info: ~v\n")
       debug-info))
    (define context (hash-ref debug-info 'context))
    (define bindings (hash-ref debug-info 'bindings '()))
    (remove-duplicates
     (for/list ([binding (in-list bindings)]
                #:when (hash-has-key? binding 'local)
                ; TODO: Turnstile does too much scope manipulation for the simple thing to work.
                #;#:when #;(context-subset? (hash-ref binding 'context) context))
       (datum->syntax stx (hash-ref binding 'name) stx))
     bound-identifier=?))

  ;; context-subset? : Context Context -> Boolean
  (define (context-subset? a b)
    ;; TODO: use an actual set-of-scopes subset function
    (list-prefix? a b))

  ;; non-macro-id? : Id -> Boolean
  (define NON-MACRO (gensym 'NON-MACRO))
  (define (non-macro-id? id)
    (eq? NON-MACRO (syntax-local-value id (λ () NON-MACRO)))))

(begin-for-syntax
  (define (env-to-string ids type)
    (format
     "~a--------------~n~a"
     (for/fold ([str ""])
               ([id ids])
       (format "~a~a : ~a~n" str (syntax->datum id)
               (type->str (typeof (local-expand id 'expression '())))))
     (type->str type))))

(begin-for-syntax
  (struct todo-item (full summary) #:prefab)
  #;(define-syntax-class env-spec
    (pattern ((~literal :all)))))

(define-for-syntax (make-todo context full msg type)
  (syntax-property
   #`(#%app- error- (#%datum- . #,msg))
   ;; NB: The syntax-find-local-variables call must happen in such a way
   ;; that the source locations and other properties on those variables
   ;; are preserved.
   ;; Don't try passing them through macro expansion.
   'todo (todo-item
          (format "~a~a" (env-to-string (syntax-find-local-variables context) type) full)
          msg)))

(define-typed-syntax ?
  ;; TODO: allow user to specify vars to hide/show
  ;; Something like require/provide specs?
  [(_ msg:str #;() #;(spec:env-spec ...)) >>
   ---------------------
   [⊢ #,(let ([msg (syntax-e (attribute msg))])
          (make-todo this-syntax msg msg #'Hole)) => Hole]]
  [(_ msg:str #;() #;(spec:env-spec ...)) <= A >>
   ---------------------
   [⊢ #,(let ([msg (syntax-e (attribute msg))])
          (make-todo this-syntax msg msg #'A))]]
  ;; TODO: This approach causes problems with source locations or debug info,
  ;; which breaks the syntax-find-local-variables
  #;[(_ msg:str) >>
   -------------------
   [>>> #,(quasisyntax/loc this-syntax
            (? msg ()))]]
  [_:id >>
   ---------------------
   [⊢ #,(make-todo this-syntax "" "" #'Hole) => Hole]]
  [_:id <= A >>
   ---------------------
   [⊢ #,(make-todo this-syntax "" "" #'A)]])

(define-typed-syntax (ann e (~datum :) τ) ≫
  [⊢ e ≫ e- ⇐ τ]
  --------
  [⊢ e- ⇒ τ])

(begin-for-syntax
  (define old-relation (current-typecheck-relation))
  ;; Every type is equal to 'Hole
  (current-typecheck-relation
   (lambda (t1 t2)
     (syntax-parse (list t1 t2)
       [(~Hole _) #t]
       [(_ ~Hole) #t]
       [_ (old-relation t1 t2)]))))

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

#;(? "meow")

(define-typed-variable the-truth #t)

((λ ([x : Bool]) ?) the-truth)

((λ ([x : Bool]) (ann ? : Bool)) #f)

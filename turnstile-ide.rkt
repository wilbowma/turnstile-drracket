#lang turnstile/base
(require
 (only-in turnstile/lang [⇒ =>] [⇐ <=] [≫ >>] [⊢ /-] [≻ >>>])
 (only-in racket/base [error error-])
 (for-syntax
  (only-in racket/list remove-duplicates)
  syntax/parse))

(provide Hole ? ann)

(define-base-type Hole)

;; copy/paste from debug
(begin-for-syntax
  ;; syntax-find-local-variables : Syntax -> (Listof Id)
  ;; NB: The syntax-find-local-variables call must happen in such a way
  ;; that the source locations and other properties on those variables
  ;; are preserved.
  ;; Don't try passing them through macro expansion from the original location.
  (define (syntax-find-local-variables stx)
    (define debug-info (syntax-debug-info stx (syntax-local-phase-level) #t))
    (define context (hash-ref debug-info 'context))
    (define bindings (hash-ref debug-info 'bindings '()))
    (remove-duplicates
     (for/list ([binding (in-list bindings)]
                #:when (hash-has-key? binding 'local)
                ; TODO: Turnstile does too much scope manipulation for the
                ; simple thing to work.
                ; Anyway David's version from demon.rkt doesn't do this.
                #;#:when #;(context-subset? (hash-ref binding 'context) context))
       (datum->syntax stx (hash-ref binding 'name) stx))
     bound-identifier=?))

  ;; context-subset? : Context Context -> Boolean
  #;(define (context-subset? a b)
    ;; TODO: use an actual set-of-scopes subset function
    (list-prefix? a b)))

(begin-for-syntax
  (struct todo-item (full summary) #:prefab)

  (define (env-to-string ids goal-type)
    (let* ([lead-space (build-string 2 (lambda _ #\space))]
           [env (for/fold ([str ""])
                          ([id ids])
                  (format "~a~a~a : ~a~n" str lead-space (syntax->datum id)
                          (type->str (typeof (local-expand id 'expression '())))))]
           [num-hyphen (max (- (string-length env) 2) 5)]
           [hyphens (build-string num-hyphen (lambda _ #\-))])
      (format "Environment:~n~a~a~a~n~a~a" env lead-space hyphens
              lead-space (type->str goal-type))))

  #;(define-syntax-class env-spec
      (pattern ((~literal :all))))

  (define (make-todo context full msg type)
    (syntax-property
     #`(#%app- error- (#%datum- . #,msg))
     'todo
     (todo-item
      (format
       "~a~a"
       ;; env then full message from of todo
       (env-to-string (syntax-find-local-variables context) type)
       ;; if full message empty, leave it; else, typeset 2 lines after env.
       (if (not (equal? "" full))
           (format "~n~n~a" full)
           ""))
      msg))))

(define-typed-syntax ?
  ;; TODO: allow user to specify vars to hide/show
  ;; Something like require/provide specs?
  [(_ msg:str #;() #;(spec:env-spec ...)) <= A >>
   ---------------------
   [⊢ #,(let ([msg (syntax-e (attribute msg))])
          (make-todo this-syntax "" msg #'A))]]
  [(_ msg:str #;() #;(spec:env-spec ...)) >>
   ---------------------
   [⊢ #,(let ([msg (syntax-e (attribute msg))])
          (make-todo this-syntax "" msg #'Hole)) => Hole]]
  ;; TODO: This approach causes problems with source locations or debug info,
  ;; which breaks the syntax-find-local-variables
  #;[(_ msg:str) >>
   -------------------
   [>>> #,(quasisyntax/loc this-syntax
            (? msg ()))]]
  [_:id <= A:type >>
   ---------------------
   [⊢ #,(make-todo this-syntax "" "" #'A)]]
  [_:id >>
   ---------------------
   [⊢ #,(make-todo this-syntax "" "" #'Hole) => Hole]])

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

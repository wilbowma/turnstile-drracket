#lang turnstile/base
(require
 (only-in turnstile/lang [⇒ =>] [⇐ <=] [≫ >>] [⊢ /-] [≻ >>>])
 (only-in racket/base [error error-])
 (for-syntax
  (only-in racket/list remove-duplicates)
  syntax/parse)
 (for-meta 2 racket/base))

(provide try-to-init-hole (for-syntax current-hole-type current-hole-type?) ? ann)

(define-for-syntax (raise-initial-hole-error name expected param x)
  (raise-syntax-error
   name
   (format
    "Expected ~a.\n Try (try-to-init-hole). If using higher-kinded or dependent types, this probably won't work and you'll have to create a new type and set the parameter `~a` at phase 1."
    expected
    param)
   x))

(define-syntax (init-Hole stx)
  (syntax-parse stx
    [_:id
     (raise-initial-hole-error
      'Hole "`current-hole-type` to be initialized, but it hasn't been" stx)]))

(define-for-syntax current-hole-type
  (make-parameter #'init-Hole
                  (lambda (x)
                    (unless (identifier? x)
                      (raise-initial-hole-error
                       'current-hole-type
                       "an identifier to act at the type of holes"
                       'current-hole-type
                       'current-hole-type x))
                    x)))

(define-for-syntax (init-~Hole stx)
  (raise-initial-hole-error
   '~Hole
   "`current-hole-type? to be initialized, but it hasn't been"
   'current-hole-type?
   stx))

(define-for-syntax current-hole-type?
  (make-parameter init-~Hole
                  (lambda (x)
                    (unless (procedure? x)
                      (raise-initial-hole-error
                       'current-hole-type?
                       "a predicated to recognize type of holes"
                       'current-hole-type? x))
                    x)))

(define-syntax (try-to-init-hole stx)
  (syntax-parse stx
    [(_)
     ;; TODO: Could try to detect if Type or Set exist, then create the typed type Hole?
     ;; Not sure how to do that without importing a particular language....
     #`(begin
         (define-base-type Hole)
         (begin-for-syntax
           (require syntax/parse)
           (current-hole-type #'Hole)
           (current-hole-type?
            (lambda (x)
              (syntax-parse x
                [~Hole #t]
                [_ #f])))))]))

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

  (require syntax/srcloc)
  (define (make-todo context full msg type)
    (syntax-property
     #`(#%app- error- (#%datum- . #,(format "~a: Running incomplete program hit a hole with msg: ~a~n" (source-location->string context) msg)))
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
   #:with Hole (current-hole-type)
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
   #:with Hole (current-hole-type)
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
     (define ~Hole (current-hole-type?))
     (or (~Hole t1) (~Hole t2) (old-relation t1 t2)))))

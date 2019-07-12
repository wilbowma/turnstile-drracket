#lang racket/gui
(require drracket/tool framework racket/runtime-path data/interval-map
         "private/goal-info.rkt")

(provide tool@)

(define-runtime-path expansion-handler.rkt "private/expansion-handler.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (struct todo-info (meta index) #:transparent)

    (define hole-text<%>
      (interface (racket:text<%>)
        [build-editing-menu (->m (is-a?/c menu-item-container<%>) exact-nonnegative-integer? (is-a?/c racket:text<%>) void?)]
        [set-todos (->m any/c void?)]
        [set-commands (->m any/c void?)]
        [set-types (->m any/c void?)]))

    (define hole-finding-text-mixin
      (mixin (racket:text<%> drracket:unit:definitions-text<%>) (hole-text<%>)
        (super-new)

        (inherit get-text get-forward-sexp get-start-position set-position
                 get-active-canvas scroll-to-position get-tab position-line)

        (define (update-pos)
          (define pos (get-start-position))
          (define tab-info (hash-ref hole-info (get-tab) #f))
          (define type-tab-info (hash-ref type-info (get-tab) #f))
          ; update type
          (send (send (get-tab) get-frame) set-current-type (get-text pos (or (get-forward-sexp pos) pos))
                (and type-info type-tab-info (interval-map-ref type-tab-info pos #f)))
          ; update todos
          (match (and hole-info
                      tab-info
                      (interval-map-ref tab-info pos #f))
            [#f (send (send (get-tab) get-frame) set-current-todo #f)]
            [(and g (todo-info _ i))
             (send (send (get-tab) get-frame) set-current-todo g)]))

        (define hole-info (make-hasheq))
        (define (set-hole-info! [info #f])
          (define tab (get-tab))
          (hash-set! hole-info tab
                     (and info (make-interval-map info)))
          (update-hole-info!))

        (define command-info (make-hasheq))
        (define (set-command-info! [commands #f])
          (define tab (get-tab))
          (hash-set! command-info tab
                     (if commands
                         commands
                         '())))

        (define type-info (make-hasheq))
        (define (set-type-info! [types #f])
          (define tab (get-tab))
          (hash-set! type-info tab (and types (make-interval-map types)))
          #;(update-type-info!))

        (define/public (update-hole-info!)
          (define tab (get-tab))
          (define frame (send tab get-frame))
          (let ([info (hash-ref hole-info tab #f)])
            (send frame set-todo-list (if info info (make-interval-map)))))

        #;(define/public (update-type-info!)
          (define tab (get-tab))
          (define frame (send tab get-frame))
          (let ([info (hash-ref type-info tab #f)])
            (send frame set-current-type (if info info (make-interval-map)))))

        (define/public (set-todos gs)
          (set-hole-info!
           (for/list ([g gs]
                      [i (in-range 10000)])
             (match-define `((,start . ,end) . ,todo) g)
             (cons (cons start end)
                   (todo-info (cdr g) i)))))

        (define/public (set-commands all-cs)
          (set-command-info! all-cs))

        (define/public (set-types all-types)
          #;(for ([t all-types])
            (match-define `((,start . ,end) . ,type) t)
            (displayln (format "At ~a--~a, ~a~n" start end (syntax->datum type))))
          (set-type-info! all-types))

        (define/augment (after-set-position)
          (update-pos))

        (define/augment (after-insert start len)
          (define holes (hash-ref hole-info (get-tab) #f))
          (when holes
            (interval-map-expand! holes start (+ start len))
            (update-hole-info!)))

        (define/augment (after-delete start len)
          (define holes (hash-ref hole-info (get-tab) #f))
          (when holes
            (interval-map-contract! holes start (+ start len))
            (update-hole-info!)))

        (define/public (build-editing-menu menu pos text)
          (define tab-info (hash-ref command-info (get-tab) #f))
          (define cmds (for/list ([cmd (or tab-info '())]
                                  #:when (let* ([region (car cmd)]
                                                [start (car region)]
                                                [end (cdr region)])
                                           (<= start pos end)))
                         cmd))
          (define editing-menu
            (make-object menu% "Commands" menu))
          (send editing-menu enable #f)
          (for ([c cmds])
            (match-define (cons (cons start end) found-cmds)
              c)
            (for ([this-c (in-list (if (or (pair? found-cmds)
                                           (null? found-cmds))
                                       found-cmds
                                       (list found-cmds)))])
              (match-define (command name module-path function arguments) this-c)
              (new menu-item%
                   [label name]
                   [parent editing-menu]
                   [callback (lambda (item ev)
                               (if (symbol? function)
                                   (let ([handler (dynamic-require
                                                   module-path function
                                                   (thunk
                                                    (error function
                                                           (format "Can't require command.\nModule: ~a\nFunction: ~a"
                                                                   module-path
                                                                   function))))])
                                     (define-values (keywords1 keywords2)
                                       (procedure-keywords handler))
                                     (define keywords (append keywords1 keywords2))
                                     (define kw/vals
                                       (sort
                                        (for/list ([kw (in-list '(#:string #:definitions #:editor
                                                                  #:file))]
                                                   [kw-val (in-list
                                                            (list (send this get-text start end)
                                                                  this
                                                                  this
                                                                  ;; TODO
                                                                  #f))]
                                                   #:when (memv kw keywords))
                                          (cons kw kw-val))
                                        keyword<?
                                        #:key car))
                                     (define str
                                       (keyword-apply handler
                                                      (map car kw/vals)
                                                      (map cdr kw/vals)
                                                      arguments))
                                     (when (string? str)
                                       (send this insert str start end)))
                                   (message-box "Bad command info"
                                                (format "Info:\n~a" c))))])))
          (when (not (null? cmds))
            (send editing-menu enable #t)))))

    (define extra-panel-mixin
      (mixin (drracket:unit:frame<%>) ()

        (inherit get-definitions-text)

        (define/augment (on-tab-change from to)
          (send (get-definitions-text) update-hole-info!))

        (define todo-list-listeners (box null))
        (define/public (set-todo-list gs)
          (for ([l (unbox todo-list-listeners)])
            (send l on-new-todo-list gs)))

        (define current-todo-listeners (box null))
        (define/public (set-current-todo g)
          (for ([l (unbox current-todo-listeners)])
            (send l on-new-current-todo g)))

        (define current-type-listeners (box null))
        (define/public (set-current-type term type)
          (for ([l (unbox current-type-listeners)])
            (send l on-new-current-type term type)))

        (define/override (get-definitions/interactions-panel-parent)
          (define super-res (super get-definitions/interactions-panel-parent))
          (define new-panel
            (new (class panel:vertical-dragable%
                   (super-new)
                   (inherit add-child delete-child get-percentages set-percentages)
                   (define todos-exist? (box #f))
                   (define init-type-message (box #t))
                   (define/public (on-new-current-type term type)
                     (displayln type)
                     (when (and type (unbox init-type-message))
                       (displayln "Initializing type box")
                       (add-child prop-panel)
                       (set-percentages '(99/100 1/100))
                       (set-box! init-type-message #f)))

                   (define/public (on-new-todo-list gs)
                     (if (dict-empty? gs)
                         ;; show -> hide
                         (when (unbox todos-exist?)
                           (set-box! panel-percents (get-percentages))
                           (set-box! todos-exist? #f)
                           (send prop-panel delete-child show-hide))
                         ;; hide -> show
                         (unless (unbox todos-exist?)
                           (on-new-current-type #t #t)
                           (send prop-panel add-child show-hide)
                           (set-percentages (unbox panel-percents))
                           (set-box! todos-exist? #t)))))
                 [parent super-res]))
          (define prop-panel
            (new panel:vertical-dragable%
                 [parent new-panel]
                 [stretchable-height #f]
                 [style '(deleted)]))
          (define show-hide
            (new vertical-panel%
                 [parent  prop-panel]
                 [stretchable-height #f]
                 [style '(deleted)]))
          (define panel-percents (box '(99/100 1/100)))
          (define (update-percents!)
            (if (unbox show-todos?)
                (send prop-panel set-percentages (unbox panel-percents))
                (begin (set-box! panel-percents (send prop-panel get-percentages))
                       (send prop-panel set-percentages '(99/100 1/100)))))
          (define (check-callback check-box evt)
            (define show? (send check-box get-value))
            (set-box! show-todos? show?)
            (update-percents!)
            (send show-hide change-children
                  (lambda (chs)
                    (cons show-hide-widget
                          (if (unbox show-todos?)
                              (list hole-holder)
                              '())))))
          (define type-widget
            (new (class message%
                   (super-new)

                   (inherit set-label)

                   (define/public (on-new-current-type term type)
                     (displayln "Updating label")
                     (if type
                         (set-label (format "Type: ~a : ~a" term type))
                         ; Can happen when a large chunk is selected, in which case we don't want to display
                         (set-label (format "Type: selected has no type")))))
                   [parent prop-panel]
                   [label "Type: "]
                   [stretchable-height #f]))

          (define show-todos? (box #t))
          (define show-hide-widget
            (new check-box%
                 [parent show-hide]
                 [label "Show TODOs"]
                 [value (unbox show-todos?)]
                 [stretchable-height #f]
                 [callback check-callback]))
          (define hole-holder
            (new panel:horizontal-dragable% [parent show-hide]))
          (define (truncate-summary str)
            (if (> (string-length str) 200)
                (string-append (substring str 0 196) "...")
                str))
          (define hole-list-box
            (new (class list-box%
                   (super-new)
                   (inherit append clear get-selection select set-selection)
                   (define/public (on-new-todo-list gs)
                     (clear)
                     (define defns (get-definitions-text))
                     (for ([(k g) (in-dict gs)])
                       (match-define (todo-item full summary) (todo-info-meta g))
                       (define line (send defns position-line (car k)))
                       (define col (- (car k) (send defns line-start-position line)))
                       (send hole-list-box append (number->string (add1 line)) (cons k g))
                       (define count (send hole-list-box get-number))
                       (send hole-list-box set-string (sub1 count) (number->string col) 1)
                       (send hole-list-box set-string
                             (sub1 count)
                             (truncate-summary (format "~a" summary)) 2)))
                   (define/public (on-new-current-todo g)
                     (if g
                         (set-selection (todo-info-index g))
                         (let ([sel (get-selection)])
                           (when sel (select sel #f))))))
                 [parent hole-holder]
                 [label #f]
                 [choices (list)]
                 [columns '("Line" "Col" "Summary")]
                 [style '(single column-headers)]
                 [callback (lambda (list-box evt)
                             (when (eqv? (send evt get-event-type) 'list-box)
                               (match (send list-box get-selections)
                                 [(list) (void)]
                                 [(list n)
                                  (match-define (cons (cons start end)
                                                      (todo-info meta _))
                                    (send list-box get-data n))
                                  (define defns (get-definitions-text))
                                  (queue-callback (thunk (send* defns
                                                           (set-position start end)
                                                           (scroll-to-position start #f end))
                                                         (define canvas (send defns get-active-canvas))
                                                         (when canvas (send canvas focus))))]
                                 [_ (void)])))]))

          (define p2
            (new group-box-panel%
                 [parent hole-holder]
                 [label "Details"]))
          (define details
            (new (class text-field%
                   (super-new)
                   (inherit set-value)
                   (define/public (on-new-current-todo g)
                     (send detail-editor lock #f)
                     (set-value (if g (format "~a" (todo-item-full (todo-info-meta g))) ""))
                     (send detail-editor lock #t)))
                 [parent p2]
                 [label #f]
                 [font (make-object font% (editor:get-current-preferred-font-size) 'modern)]
                 [style '(multiple)]))

          (define detail-editor (send details get-editor))
          (send detail-editor lock #t)

          (set-box! todo-list-listeners (list hole-list-box new-panel))
          (set-box! current-todo-listeners (list hole-list-box details))
          (set-box! current-type-listeners (list new-panel type-widget))

          new-panel)

        (super-new)))

    (define (phase1) (void))
    (define (phase2) (void))

    (drracket:get/extend:extend-definitions-text hole-finding-text-mixin)
    (drracket:get/extend:extend-unit-frame extra-panel-mixin)
    (drracket:module-language-tools:add-online-expansion-handler
     expansion-handler.rkt
     'handle-expansion
     (λ (text info)
       (match-define (list todos commands types) info)
       (send text set-todos todos)
       (send text set-commands commands)
       (send text set-types types)))
    (keymap:add-to-right-button-menu/before
     (let ([old (keymap:add-to-right-button-menu/before)])
       (λ (menu editor event)
         (old menu editor event)
         (define-values (pos text) (send editor get-pos/text event))
         (when (and pos (is-a? text hole-text<%>))
           (send editor build-editing-menu menu pos text)))))))




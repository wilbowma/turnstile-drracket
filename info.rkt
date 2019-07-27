#lang info

(define collection "turnstile-drracket")
(define version "0.1")
(define pkg-desc "A DrRacket tool for Turnstile that displays holes, types, etc. Based on David Christiansen's todo-list")

(define drracket-name "Turnstile Mode")
(define drracket-tools (list (list "tool.rkt")))
(define drracket-tool-names (list "Turnstile Mode"))

(define deps '("base"
               "data-lib"
               "drracket-plugin-lib"
               "gui-lib"
               ("turnstile-lib" #:version "0.4.10")))
(define build-deps '("scribble-lib" "racket-doc"))

(define scribblings '(("scribblings/todo-list.scrbl" ())))

(define test-omit-paths '("demo.rkt"))

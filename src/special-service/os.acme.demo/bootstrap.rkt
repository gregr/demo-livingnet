#lang racket/load

(require
  "basis.rkt"
  gregr-misc/oop
  gregr-misc/sugar
  racket/match
  )

(define (triples xs)
  (match xs
    ((list* name deps body xs) (list* (list name deps body) (triples xs)))
    ('() '())))

(define (lib->package lib)
  (forl (list name deps body) <- (triples lib)
        (cons (symbol->string name)
              (list (forl dep <- deps (list #f (symbol->string dep)))
                    `(lambda ,deps ,body)))))

(define (lib->module name provisions lib)
  (define module-body (forl (list name deps body) <- (triples lib)
                            `(define ,name ((lambda ,deps ,body) ,@deps))))
  `(module ,name racket/base
     (provide ,@provisions)
     (require
       "basis.rkt"
       gregr-misc/oop
       gregr-misc/sugar
       racket/function
       racket/match
       racket/set)
     ,@module-body))

(define lib (read/file "lib.rkts"))
(eval (lib->module 'lib '(global-context) lib))
(require 'lib)
(define fsys (global-context 'filesystem))
(displayln fsys)
(displayln (format "~v" (lib->package lib)))

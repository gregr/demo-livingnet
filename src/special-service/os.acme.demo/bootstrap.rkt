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
(define provisions
  '(global-context
    network-serve-requests
    ))
(eval (lib->module 'lib provisions lib))
(require 'lib)
(define net (global-context 'network))
(define fsys (global-context 'filesystem))

(for_ (cons name data) <- (lib->package lib)
      (o@ fsys 'put (list "lib" name) data))

(define (request->response request)
  (displayln (format "received request: ~v" request))
  (if request
    (lets
      val = (o@ fsys 'get (list "lib" request))
      _ = (displayln (format "serving: ~v" val))
      val)
    (begin
      (displayln "serving OS installation")
      "OS installation")))

(displayln "accepting connections")
(network-serve-requests net request->response)

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
        (cons name (list (forl dep <- deps (list #f dep))
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
  '(global-network
    global-storage
    network-serve-requests
    resolve-dependencies
    ))
(eval (lib->module 'lib provisions lib))
(require 'lib)
(define net global-network)
(define fsys global-storage)
(define package (make-immutable-hash (lib->package lib)))
(define (lib-name name) (list "lib" (format "~a" name)))

(for_ (values name data) <- package
      (o@ fsys 'put (lib-name name) data))

(def (get-local host request)
  _ = (when host (error "get-local applied to remote dependency: ~a, ~a"
                        host request))
  (hash-ref package request))
(define resolve (resolve-dependencies get-local))
(define (deps->bindings deps)
  `((list ,@deps) = ,(resolve (forl dep <- deps (list #f dep)))))

(define bootstrap-os
  `(let ()
     (define kernel
       `(lets ,@',(deps->bindings '(global-negotiator global-console))
              console = global-console
              ntor = global-negotiator
              _ = (o@ console 'put-line "kernel started")
              _ = (o@ console 'put "choose a destination: ")
              host = (o@ console 'get-line)
              result = (o@ ntor 'negotiate host '())
              _ = (o@ console 'put-line "kernel stopping")
              result))
     (define kernel-path '("boot" "kernel"))
     (define bootloader
       `(lets ,@',(deps->bindings '(global-console global-filesystem))
              console = global-console
              fsys = global-filesystem
              _ = (o@ console 'put-line "loading kernel at: ~v" ',kernel-path)
              kernel = (o@ fsys 'get ',kernel-path)
              _ = (o@ console 'put-line "found kernel: ~v" kernel)
              _ = (o@ console 'put-line "starting kernel")
              (eval kernel)))
     (lets _ = (displayln "in persist")
           ,@(deps->bindings '(global-filesystem master-boot-record))
           _ = (displayln "built filesystem")
           fsys = global-filesystem
           _ = (displayln (format "putting kernel in: ~v" kernel-path))
           _ = (o@ fsys 'put kernel-path kernel)
           _ = (displayln (format "putting bootloader in mbr"))
           (o@ master-boot-record 'put bootloader))))

(define (request->response request)
  (displayln (format "received request: ~v" request))
  (define response
    (if request (o@ fsys 'get (list "lib" request)) bootstrap-os))
  (displayln (format "serving: ~v" response))
  response)

(displayln "accepting connections")
(network-serve-requests net request->response)

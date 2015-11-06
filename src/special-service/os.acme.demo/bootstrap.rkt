#lang racket/load

(require
  "basis.rkt"
  gregr-misc/oop
  gregr-misc/sugar
  racket/match
  )

(define lib->package
  '(lambda (lib)
     (make-immutable-hash
       (forl (list name deps body) <- (triples lib)
             expr = `(lambda ,deps ,body)
             (cons name (if (equal? '() name) expr
                          (list (forl dep <- deps (list #f dep))
                                expr)))))))

(define datum->lib-path
  '(lambda (datum)
     (if (list? datum) (list* "lib" datum) (list "lib" (format "~s" datum)))))

(define persist-package
  `(lambda (fsys package)
     (for_ (values name data) <- package
           (o@ fsys 'put (,datum->lib-path name) data))))

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
       racket/set
       racket/string)
     ,@module-body))


(define lib (read/file "input.rkts"))
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
(define package ((eval lib->package) lib))
((eval persist-package) fsys package)

(def (get-local host request)
  _ = (when host (error "get-local applied to remote dependency: ~a, ~a"
                        host request))
  (hash-ref package request))
(define resolve (resolve-dependencies get-local))
(define (deps->bindings deps)
  `((list ,@deps) = ,(resolve (forl dep <- deps (list #f dep)))))

(define bootstrap-os
  `(let ()
     (define (server-kernel request->response)
       `(lets ,@',(deps->bindings
                    '(global-network global-filesystem global-console
                      network-serve-requests))
              console = global-console
              fsys = global-filesystem
              _ = (o@ console 'put-line "accepting connections")
              (network-serve-requests global-network ,request->response)))
     (define lib-request->response
       `(lambda (request)
          (o@ console 'put-line (format "received request: ~v" request))
          (define response (o@ fsys 'get (,',datum->lib-path request)))
          (o@ console 'put-line (format "serving: ~v" response))
          response))
     (define user-kernel
       `(lets ,@',(deps->bindings
                    '(global-negotiate global-console
                      capabilities->context capabilities-basic))
              console = global-console
              ; TODO: limit eval namespace and capabilities
              host->eval = (lambda (host)
                             (lambda (prog)
                               (o@ console 'put-line
                                   (format "evaluating: ~s" prog))
                               ((eval prog)
                                (capabilities->context
                                  (capabilities-basic host console)))))
              _ = (o@ console 'put-line "kernel started")
              _ = (o@ console 'put "choose a destination: ")
              host = (o@ console 'get-line)
              result = ((global-negotiate (host->eval host)) host '())
              _ = (o@ console 'put-line (format "produced ~s" result))
              _ = (o@ console 'put-line "kernel stopping")
              result))
     (define kernel-path '("boot" "kernel"))
     (define bootloader
       `(lets ,@',(deps->bindings '(global-console global-filesystem))
              console = global-console
              fsys = global-filesystem
              _ = (o@ console 'put-line
                      (format "loading kernel at: ~v" ',kernel-path))
              kernel = (o@ fsys 'get ',kernel-path)
              _ = (o@ console 'put-line (format "found kernel: ~v" kernel))
              _ = (o@ console 'put-line "starting kernel")
              (eval kernel)))
     (lets ,@(deps->bindings
               '(global-console global-filesystem master-boot-record))
           console = global-console
           _ = (o@ console 'put-line "built filesystem")
           fsys = global-filesystem
           _ = (o@ console 'put-line "install server kernel? [Y/n]")
           kernel =
           (if (equal? (o@ console 'get-line) "Y")
             (begin
               (o@ console 'put-line
                   "is this a library-based resource server? [Y/n]")
               (server-kernel
                 (if (equal? (o@ console 'get-line) "Y")
                   (begin (o@ console 'put-line "provide library:")
                          (o@ console 'put-line "writing library to storage")
                          (,persist-package
                            fsys (,lib->package
                                   (read/string (o@ console 'get-eof))))
                          (o@ console 'put-line "wrote library to storage")
                          lib-request->response)
                   (begin (o@ console 'put-line
                              "provide request->response definition:")
                          (read/string (o@ console 'get-eof))))))
             user-kernel)
           _ = (o@ console 'put-line
                   (format "putting kernel in: ~v" kernel-path))
           _ = (o@ fsys 'put kernel-path kernel)
           _ = (o@ console 'put-line (format "putting bootloader in mbr"))
           (o@ master-boot-record 'put bootloader))))

(define (request->response request)
  (define req->path (eval datum->lib-path))
  (displayln (format "received request: ~v" request))
  (define response
    (if request (o@ fsys 'get (req->path request)) bootstrap-os))
  (displayln (format "serving: ~v" response))
  response)

(displayln "accepting connections")
(network-serve-requests net request->response)

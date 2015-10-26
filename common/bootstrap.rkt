#lang racket/base
(provide
  filesystem
  get
  global-context
  )

(require
  "basis.rkt"
  gregr-misc/oop
  gregr-misc/sugar
  )

; TODO: IPC
; uni-directional
;   ports(strings), channels(values)
; bi-directional
;   sockets(values)

; TODO:
; is eof handling correct?
; single host connector?

(define global-network
  (let ((connection (class _ (conn) () ()
                      (close () (network-close conn))
                      (send (value) (network-send conn value))
                      (recv () (network-recv conn)))))
    (object-new '()
      (method-table _
        (listen () (connection (network-listen)))
        (connect (hostname) (connection (network-connect hostname)))))))

; TODO:
; separate mbr
; single file caps?
; read-only/write-only caps?

(define global-storage
  (let ((mbr (object-new '()
               (method-table _
                 (get () (storage-mbr-get))
                 (put (value) (storage-mbr-put value))
                 (delete () (storage-mbr-delete))))))
    (object-new '()
      (method-table _
        (get (path) (storage-get path))
        (put (path value) (storage-put path value))
        (delete (path) (storage-delete path))
        (mbr () mbr)))))

; TODO:
; current-x-port
; construct socket as mixin of two port objects
(define global-console
  (object-new '()
    (method-table _
      (get-char () (read-char))
      (get-line () (read-line))
      (put (value) (display value))
      (put-line (value) (displayln value)))))

(define filesystem
  (class _ (storage root) ((subpath (lambda (path) (build-path root path)))) ()
    (get (path) (o@ storage 'get (subpath path)))
    (put (path value) (o@ storage 'put (subpath path) value))
    (delte (path) (o@ storage 'delete (subpath path)))))

(def (download net hostname request)
  conn = (o@ net 'connect hostname)
  _ = (o@ conn 'send request)
  response = (o@ conn 'recv)
  _ = (o@ conn 'close)
  response)

; TODO: this approach to cache-duration isn't ideal, but we'll live with it.
; 1) We don't want to hand out the ability to DoS other sites.
; 2) Sites should be able to specify case-by-case durations for their content.
(define ((get net fsys) signature (cache-duration 900))
  (lets (list hostname request) = signature
        path = (build-path hostname (format "~v" request))
        cache-entry = (o@ fsys 'get path)
        (values cached? data) =
        (if (eof-object? cache-entry)
          (values #f (void))
          (lets (list time-added data) = cache-entry
                (if (<= (+ time-added cache-duration) (current-seconds))
                  (values #f (void))
                  (values #t data))))
        (if cached? data
          (lets data = (download net hostname request)
                _ = (o@ fsys 'put path (list (current-milliseconds) data))
                data))))

; TODO: define negotiate
; support multiple evaluation choices in negotiate response
;   [(documentation/explanation/metadata, eval, requested-caps, code)]

; TODO:
; capabilities that can be requested by site signature
; signature = (host, request-data)
; enables cross-site collaboration on your machine despite differing trust
; While sites can 'get' each other's data, they can only evaluate that data in
; their personal contexts.
; build on top of 'negotiate'?

(define global-capabilities
  (hash 'console global-console
        'network global-network
        'storage global-storage
        'filesystem (filesystem global-storage "application-data")
        'get (get global-network (filesystem global-storage "cache"))
        ))
; TODO: negotiate for site-specific cap keys
(define (global-context cap-key) (hash-ref global-capabilities cap-key))

; TODO: more sophisticated authority management
; capability guardedness spectrum
; logging, revocation, time-boxing, throttling, permission appeals

; UI considerations:
; ability to pause/terminate child processes at any time
; up-front requests necessary for dangerous capabilities when starting process
;   late request for a dangerous capability is auto-denied
;     process should be paused (to allow manual override) or killed outright
;   options
;     grant
;       possibly also requesting logging, subsequent permission prompting, etc.
;     do not grant
;       optionally may allow process to start anyway; because you're curious
;   options may also be conveniently set beforehand per-capability
;     globally and/or on a per-site basis
; requests for user input to a capability
;   follow the usual rules, but if granted, provide an appropriate input dialog
;   examples
;     user asked to enter an account password into a pop-up text box
;     user asked to choose a file from a dialog box

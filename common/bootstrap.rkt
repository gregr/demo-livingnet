#lang racket/base
(provide
  filesystem
  global-context
  )

(require
  "basis.rkt"
  gregr-misc/oop
  gregr-misc/sugar
  racket/function
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

(define (cache-put fsys path data time-added cache-duration)
  (o@ fsys 'put path (list time-added cache-duration data)))
(define (cache-get fsys path origin-get cache-duration)
  (lets now = (current-seconds)
        cache-entry = (o@ fsys 'get path)
        (values cached? data) =
        (if (eof-object? cache-entry)
          (values #f (void))
          (lets (list time-added duration data) = cache-entry
                cache-duration =
                (if cache-duration (if duration (min duration cache-duration)
                                     cache-duration)
                  duration)
                (if (and duration (<= (+ time-added duration) now))
                  (values #f (void)) (values #t data))))
        (if cached? data
          (lets data = (origin-get)
                _ = (unless (void? data)
                      (cache-put fsys path data now cache-duration))
                data))))

(define (((global-get net fsys) hostname) request (cache-duration #f))
  (cache-get fsys (build-path hostname (format "~v" request))
             (thunk (download net hostname request))
             cache-duration))

(define (get-self net fsys hostname) ((global-get net fsys) hostname))
(define (get-other net fsys)
  (define get (global-get net fsys))
  (lambda (hostname request) ((get hostname) request #f)))

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

(define cache/get-fsys (filesystem global-storage "cache/get"))

(define global-capabilities
  (hash 'console global-console
        'network global-network
        'storage global-storage
        'filesystem (filesystem global-storage "application-data")
        ;'get-self (get-self global-network cache/get-fsys hostname)  ; TODO
        'get-other (get-other global-network cache/get-fsys)
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

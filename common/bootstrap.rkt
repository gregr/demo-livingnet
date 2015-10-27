#lang racket/base
(provide
  global-context
  )

(require
  "basis.rkt"
  gregr-misc/oop
  gregr-misc/sugar
  racket/function
  )

; TODO: IPC
; bi-directional
;   sockets(values)

(define port/input
  (class _ (in) () ()
    (close () (close-input-port in))
    (get-char () (read-char in))
    (get-line () (read-line in))
    (port () in)))
(define port/output
  (class _ (out) () ()
    (close () (close-output-port out))
    (put (value) (display value out))
    (put-line (value) (displayln value out))
    (port () out)))
(define channel/input (class _ (in) () () (get () (read/no-eof in))))
(define channel/output (class _ (out) () () (put (value) (write value out))))

(define global-console
  (object (list (port/input (current-input-port))
                (port/output (current-output-port)))))

(define global-network
  (let ((connection (class _ (conn) () ()
                      (close () (network-close conn))
                      (delete () (network-shutdown conn))
                      (put (value) (network-send conn value))
                      (get () (network-recv conn)))))
    (object '()
      (method-table _
        (listen () (connection (network-listen)))
        (connect (hostname) (connection (network-connect hostname)))))))

(define ((network-connector network host)) (o@ network 'connect host))

(define (read-only parent)
  (object '() (method-table _ (get args (o@* parent 'get args)))))
(define (write-only parent)
  (object '() (method-table _
                (put args (o@* parent 'put args))
                (delete args (o@* parent 'delete args)))))
(define storage/sub
  (class _ (parent root) ((subroot (build-path (o@ parent 'root) root)))
         (parent)
    (root () subroot)))

(define file
  (class _ (storage path) () ()
    (get () (o@ storage 'get path))
    (put (value) (o@ storage 'put path value))
    (delete () (o@ storage 'delete path))))

(define global-storage
  (lets subpath = (lambda (self path) (build-path (o@ self 'root) path))
        (object '()
          (method-table self
            (root () ".")
            (get (path) (storage-get (subpath self path)))
            (put (path value) (storage-put (subpath self path) value))
            (delete (path) (storage-delete (subpath self path)))))))
(define global-filesystem (storage/sub global-storage "data"))
(define master-boot-record (file global-storage "master-boot-record"))

(def (download net hostname request)
  conn = (o@ net 'connect hostname)
  _ = (o@ conn 'put request)
  response = (o@ conn 'get)
  _ = (o@ conn 'delete)
  ; TODO: faithful implementation would 'get' until response is void
  _ = (o@ conn 'close)
  response)

(define (cache-put fsys path data time-added cache-duration)
  (o@ fsys 'put path (list time-added cache-duration data)))
(define (cache-get fsys path origin-get cache-duration)
  (lets now = (current-seconds)
        cache-entry = (o@ fsys 'get path)
        (values cached? data) =
        (if (void? cache-entry)
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

(def ((negotiate net fsys hostname->context) hostname request)
  code = (cache-get fsys (build-path hostname (format "~v" request))
                    (thunk (void)) #f)
  code = (if (void? code) (download net hostname request) code)
  (with-handlers ((exn:fail? (lambda (_) (void))))
    ; TODO: application-specific eval
    ((eval code) (hostname->context hostname))))

(define cache/get-fsys (storage/sub global-filesystem "cache/get"))
(define cache/negotiate-fsys (storage/sub global-filesystem "cache/negotiate"))
(define global-negotiate
  (negotiate global-network cache/negotiate-fsys (const (void))))

(define global-capabilities
  (hash 'console global-console
        'network global-network
        'storage global-storage
        'filesystem (storage/sub global-filesystem "application-data")
        ;'connect-self (network-connector global-network hostname)  ; TODO
        ;'get-self (get-self global-network cache/get-fsys hostname)  ; TODO
        'get-other (get-other global-network cache/get-fsys)
        'negotiate global-negotiate
        ))
(define ((hash->context cap-hash) cap-key)
  (hash-ref cap-hash cap-key (thunk (void))))
(define global-context (hash->context global-capabilities))

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

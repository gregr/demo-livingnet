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

(define global-network
  (let ((connection (class _ (conn) () ()
                      (close () (network-close conn))
                      (send (value) (network-send conn value))
                      (recv () (network-recv conn)))))
    (object-new '()
      (method-table _
        (listen () (connection (network-listen)))
        (connect (hostname) (connection (network-connect hostname)))))))

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

(define global-capabilities
  (hash 'console global-console
        'network global-network
        'storage global-storage
        'filesystem (filesystem global-storage "application-data")
        'get (get global-network (filesystem global-storage "cache"))
        ))
(define (global-context cap-key) (hash-ref global-capabilities cap-key))

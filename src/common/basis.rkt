#lang racket/base
(provide
  bootstrap/default
  bootstrap/local
  bootstrap/remote
  launch-and-forget
  machine-name
  network-accept
  network-connect
  network-shutdown
  network-close
  network-send
  network-recv
  storage-delete
  storage-get
  storage-put
  storage-list
  read/no-eof
  read/file
  read/string
  read-all
  read-all/string
  write/file
  write/string
  triples
  )

(require
  gregr-misc/codec
  gregr-misc/list
  gregr-misc/oop
  gregr-misc/record
  gregr-misc/string
  gregr-misc/sugar
  racket/file
  racket/function
  racket/list
  racket/match
  racket/port
  racket/runtime-path
  racket/system
  )

(define-runtime-path path-network-connect "network-connect")
(define-runtime-path path-network-listen "network-listen")
(define machine-dir (current-directory))
(define-values (machines_dir machine-base-path _) (split-path machine-dir))
(define machine-name (path->string machine-base-path))
(define dir-hardware (build-path machine-dir "hardware"))

(define (blocking-read path)
  (with-output-to-string
    (thunk (system
             (format "mkdir -p \"$(dirname '~a')\" && touch '~a' && cat < '~a'"
                     path path path)))))
(def (blocking-write path str)
  (list pout pin _ perr _) =
  (process (format "mkdir -p \"$(dirname '~a')\" && cat > '~a'" path path))
  (begin (displayln str pin)
         (close-output-port pin)
         (close-input-port pout)
         (close-input-port perr)))

(define (read/no-eof in) (match (read in)
                           ((? eof-object?) (void))
                           (result result)))
(define (read/string str) (read/no-eof (open-input-string str)))
(define (read/file path) (read/string (blocking-read path)))
(define (read-all in)
  (let loop ((exprs '()))
    (match (read in)
      ((? eof-object?) (reverse exprs))
      (expr (loop (list* expr exprs))))))
(define (read-all/string str) (read-all (open-input-string str)))
(define (write/string value)
  (if (void? value) "" (call-with-output-string (curry write value))))
(define (write/file path value) (blocking-write path (write/string value)))

(define ((launch-and-forget cmd) arg) (system (string-append cmd " " arg " &")))

(define dir-network (build-path dir-hardware "network"))
(define dir-wan (build-path dir-network "uplink" "wan"))
(define wan-path (build-path dir-wan machine-name))

(record connection wd in out)
(define (network-accept)
  (unless (file-exists? wan-path) (system (path->string path-network-listen)))
  (define conn-wd (read/file wan-path))
  (connection conn-wd (build-path conn-wd "out") (build-path conn-wd "in")))
(define (network-connect hostname)
  (define conn-wd
    (with-output-to-string
      (thunk (system* (path->string path-network-connect) hostname))))
  (connection conn-wd (build-path conn-wd "in") (build-path conn-wd "out")))
(def (network-shutdown (connection _ _ out)) (blocking-write out ""))
(def (network-close (connection wd _ _)) (delete-directory/files wd))
(def (network-send (connection _ _ out) value) (write/file out value))
(def (network-recv (connection _ in _)) (read/file in))

(define dir-storage (build-path dir-hardware "storage"))
(define (storage-path path (dir? #f))
  (lets (values dirs base) = (list-init+last (map encode-base32hex path))
        dirs = (forl dir <- dirs (string-append dir "_D"))
        base = (if dir? (string-append base "_D") base)
        (build-path (apply build-path dir-storage dirs) base)))
(define (storage-path->segments path (drop-count 0))
  (lets (values dirs base) = (list-init+last
                               (map path->string
                                    (drop (explode-path path) drop-count)))
        dirs = (forl dir <- dirs
                     len = (string-length dir)
                     (string-range-remove dir (- len 2) len)) ; remove the _D
        (map decode-base32hex (append dirs (list base)))))
(define (storage-get path) (read/file (storage-path path)))
(define (storage-put path value) (write/file (storage-path path) value))
(define (storage-delete path) (delete-directory/files (storage-path path)))
(def (storage-list path)
  path = (storage-path path #t)
  plen = (length (explode-path path))
  (forl subpath <- (if (directory-exists? path) (in-directory path) '())
        #:when (file-exists? subpath)
        (storage-path->segments subpath plen)))

(define (bootstrap/local path) (eval (storage-get path)))
(define (bootstrap/remote hostname)
  (define conn (network-connect hostname))
  (network-send conn #f)
  (define bootstrap (network-recv conn))
  (network-shutdown conn)
  (network-close conn)
  (eval bootstrap))
(define (bootstrap/default bootstrap-hostname-default)
  (match (storage-get (list "master-boot-record"))
    ((? void?) (bootstrap/remote bootstrap-hostname-default))
    (mbr (eval mbr))))

(define (triples xs)
  (match xs
    ((list* name deps body xs) (list* (list name deps body) (triples xs)))
    ('() '())))

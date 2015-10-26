#lang racket/base
(provide
  bootstrap/default
  bootstrap/local
  bootstrap/remote
  launch-and-forget
  machine-name
  network-listen
  network-connect
  network-close
  network-send
  network-recv
  storage-delete
  storage-get
  storage-put
  storage-mbr-delete
  storage-mbr-get
  storage-mbr-put
  read/file
  read/string
  write/file
  write/string
  test-serve
  test-boot
  )

(require
  gregr-misc/record
  gregr-misc/sugar
  racket/file
  racket/function
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
(define (blocking-write path str)
  (system (format "mkdir -p \"$(dirname '~a')\" && printf \"%s\n\" '~a' > ~a"
                  path str path)))

(define (read/string str) (read (open-input-string str)))
(define (read/file path) (read/string (blocking-read path)))
(define (write/string value) (call-with-output-string (curry write value)))
(define (write/file path value) (blocking-write path (write/string value)))

(define ((launch-and-forget cmd) arg) (system (string-append cmd " " arg " &")))

(define dir-network (build-path dir-hardware "network"))
(define dir-wan (build-path dir-network "uplink" "wan"))
(define wan-path (build-path dir-wan machine-name))

(record connection wd in out)
(define (network-listen)
  (unless (file-exists? wan-path) (system (path->string path-network-listen)))
  (define conn-wd (read/file wan-path))
  (connection conn-wd (build-path conn-wd "out") (build-path conn-wd "in")))
(define (network-connect hostname)
  (define conn-wd
    (with-output-to-string
      (thunk (system* (path->string path-network-connect) hostname))))
  (connection conn-wd (build-path conn-wd "in") (build-path conn-wd "out")))
(def (network-close (connection wd _ _)) (delete-directory/files wd))
(def (network-send (connection _ _ out) value) (write/file out value))
(def (network-recv (connection _ in _)) (read/file in))

(define dir-storage (build-path dir-hardware "storage"))
(define dir-data (build-path dir-storage "data"))
(define path-mbr (build-path dir-storage "master-boot-record"))

(define (storage-get path) (read/file (build-path dir-data path)))
(define (storage-put path value) (write/file (build-path dir-data path) value))
(define (storage-delete path)
  (delete-directory/files (build-path dir-data path)))
(define (storage-mbr-get) (read/file path-mbr))
(define (storage-mbr-put value) (write/file path-mbr value))
(define (storage-mbr-delete)
  (call-with-output-file path-mbr (curry display "") #:exists 'replace))

(define (bootstrap/local path) (eval (storage-get path)))
(define (bootstrap/remote hostname)
  (define conn (network-connect hostname))
  (network-send conn #f)
  (define bootstrap (network-recv conn))
  (network-close conn)
  (eval bootstrap))
(define (bootstrap/default bootstrap-hostname-default)
  (match (storage-mbr-get)
    ((? eof-object?) (bootstrap/remote bootstrap-hostname-default))
    (mbr (eval mbr))))

(define (test-serve motd bootstrap-program)
  (define conn (network-listen))
  (define request (network-recv conn))
  (if request
    (begin
      (displayln (format "received non-bootstrap request: ~a" request))
      (network-send conn motd))
    (begin
      (displayln "received bootstrap request")
      (network-send conn bootstrap-program))))

(define test-boot
  '(begin (storage-put "boot/kernel"
                       '(begin
                          (displayln "bootstrapping ...")
                          (list 'seven (+ 4 3))))
          (storage-mbr-put '(begin (displayln "you can trust us!")
                                   (bootstrap/local "boot/kernel")))
          (bootstrap/default "")))

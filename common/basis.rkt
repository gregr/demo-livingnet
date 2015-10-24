#lang racket/base
(provide
  launch-and-forget
  machine-name
  network-listen
  network-connect
  network-close
  network-send
  network-recv
  storage-get
  storage-put
  storage-mbr-get
  storage-mbr-put
  read/file
  read/string
  write/file
  write/string
  )

(require
  racket/file
  racket/function
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
    (thunk (system (string-append "cat < " (path->string path))))))
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

(define (network-listen)
  (unless (file-exists? wan-path) (system (path->string path-network-listen)))
  (read/file wan-path))
(define (network-connect hostname)
  (with-output-to-string
    (thunk (system* (path->string path-network-connect) hostname))))
(define (network-close conn) (delete-directory/files conn))
(define (network-send conn io value) (write/file (build-path conn io) value))
(define (network-recv conn io) (read/file (build-path conn io)))

(define dir-storage (build-path dir-hardware "storage"))
(define dir-data (build-path dir-storage "data"))
(define path-mbr (build-path dir-storage "master-boot-record"))

(define (storage-get path) (read/file (build-path dir-data path)))
(define (storage-put path value) (write/file (build-path dir-data path) value))
(define (storage-mbr-get) (read/file path-mbr))
(define (storage-mbr-put value) (write/file path-mbr value))

#lang racket/base
(provide
  launch-and-forget
  machine-name
  network-listen
  network-connect
  network-close
  network-send
  network-recv
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
(define-values (machines-dir machine-base-path _)
  (split-path (current-directory)))
(define machine-name (path->string machine-base-path))

(define (blocking-read path)
  (with-output-to-string
    (thunk (system (string-append "cat < " (path->string path))))))
(define (blocking-write path str)
  (system (string-append "printf \"%s\n\" \"" str "\" > " (path->string path))))

(define (read/string str) (read (open-input-string str)))
(define (read/file path) (read/string (blocking-read path)))
(define (write/string value) (call-with-output-string (curry write value)))
(define (write/file path value) (blocking-write path (write/string value)))

(define ((launch-and-forget cmd) arg) (system (string-append cmd " " arg " &")))

(define dir-network (build-path "hardware" "network"))
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

#lang racket/base
(provide
  read/file
  read/string
  write/file
  write/string
  )

(require
  racket/function
  racket/port
  )

(define (read/file path) (read (open-input-file path)))
(define (read/string str) (read (open-input-string str)))
(define (write/file value path) (write value (open-output-file path)))
(define (write/string value) (call-with-output-string (curry write value)))

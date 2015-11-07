#lang racket/load

(require
  "basis.rkt"
  gregr-misc/oop
  gregr-misc/sugar
  )

(displayln "starting default bootstrap")
(bootstrap/default "os.acme.demo")

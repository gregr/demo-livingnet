#lang racket/load

(require
  "basis.rkt"
  gregr-misc/oop
  gregr-misc/sugar
  )

(displayln "starting default bootstrap")
(bootstrap/default "os.acme.demo")
(displayln "finished default bootstrap")

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

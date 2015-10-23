#lang racket
(require
  gregr-misc/sugar
  )

(define hardware-required (set "network" "storage"))
(define hardware-found
  (list->set (map path->string (directory-list "hardware"))))
(define hardware-missing (set-subtract hardware-required hardware-found))
(define (set->sorted xs) (sort (set->list xs) string<?))

(module+ main
  (unless (set-empty? hardware-missing)
    (begin
      (displayln (format "missing required hardware: ~a"
                         (set->sorted hardware-missing)))
      (exit 1))))

(define ((launch cmd) arg) (system* cmd arg))

(define capabilities-builtin
  (hash "console" displayln
        "racket-eval" eval))
(define capabilities-extra
  (make-immutable-hash
    (forl path <- (directory-list "extras")
          (cons (path->string path)
                (launch (file->string (build-path "extras" path)))))))

(define capabilities
  (append (set->sorted hardware-found)
          (hash-keys capabilities-builtin)
          (hash-keys capabilities-extra)))

(module+ main
  (displayln (format "found capabilities: ~a" (set->sorted capabilities))))

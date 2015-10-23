#lang racket
(require
  gregr-misc/dict
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

(define (capability-network msg) (void))
(define (capability-storage msg) (void))

(define capabilities-builtin
  (hash "console" displayln
        "racket-eval" eval))
(define capabilities-hardware
  (hash "network" capability-network
        "storage" capability-storage))
(define capabilities-extra
  (make-immutable-hash
    (forl path <- (directory-list "extras")
          (cons (path->string path)
                (launch (file->string (build-path "extras" path)))))))


(define capabilities
  (dict-join (dict-join capabilities-builtin capabilities-hardware)
             capabilities-extra))

(module+ main
  (displayln (format "found capabilities: ~a" (hash-keys capabilities))))

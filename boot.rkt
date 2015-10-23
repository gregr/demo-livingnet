#lang racket

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

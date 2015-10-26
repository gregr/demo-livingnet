#lang racket/base
(provide
  )

(require
  "basis.rkt"
  gregr-misc/oop
  )

(define global-network
  (let ((connection (class _ (conn) () ()
                      (close () (network-close conn))
                      (send (value) (network-send conn value))
                      (recv () (network-recv conn)))))
    (object-new '()
      (method-table _
        (listen () (connection (network-listen)))
        (connect (hostname) (connection (network-connect hostname)))))))

(define global-storage
  (let ((mbr (object-new '()
               (method-table _
                 (get () (storage-mbr-get))
                 (put (value) (storage-mbr-put value))
                 (delete () (storage-mbr-delete))))))
    (object-new '()
      (method-table _
        (get (path) (storage-get path))
        (put (path value) (storage-put path value))
        (delete (path) (storage-delete path))
        (mbr () mbr)))))

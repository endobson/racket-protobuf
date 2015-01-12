#lang racket/base

(require
  "in-memory-structures.rkt"
  "proto-reader.rkt"
  racket/port
  racket/list
  )


(define pb (call-with-input-file* (expand-user-path "~/tmp/tmp.pb") port->bytes))

(define proto-port (open-input-bytes pb))

(parse-proto proto-port
             (message-descriptor (hash) (λ () (generic-mutable-message (make-hash)
                                                                                 (make-hash)))))

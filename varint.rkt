#lang racket/base

(provide
  varint-size
  write-varint)

(define (varint-size n)
  (cond
    [(< n 128) 1]
    [else
     (add1 (varint-size (quotient n 128)))]))

(define (write-varint number port)
  (cond
    [(< number 128)
     (write-byte (+ 128 number) port)]
    [else
     (define-values (q r) (quotient/remainder number 128))
     (write-byte r port)
     (write-varint port q)]))

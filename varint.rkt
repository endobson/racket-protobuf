#lang racket/base

(provide varint-size)

(define (varint-size n)
  (cond
    [(< n 128) 1]
    [else
     (add1 (varint-size (quotient n 128)))]))

#lang racket/base

(provide (all-defined-out))


;; number: exact-positive-integer?
(define (write-proto-varint port number)
  (cond
    [(< number 128)
     (write-byte (+ 128 number) port)]
    [else
     (define-values (q r) (quotient/remainder number 128))
     (write-byte r port)
     (write-proto-varint port q)]))
     
(define (write-proto-string port string)
  (write-proto-bytes port (string->bytes/utf-8 string)))

(define (write-proto-bytes port bytes)
  (write-proto-varint port (bytes-length bytes))
  (write-bytes bytes port))

(define (write-proto-field-tag port field-number type)
  (define encoding
    (case type
      [(varint) 0]
      [(64bit) 1]
      [(length-delimited) 2]
      [(32bit) 5]))
  (+ (arithmetic-shift field-number 3) encoding))

#lang racket/base

(require
  protogen/tests/test-data/foo-proto
  protogen/tests/test-data/foo2-proto
  racket/port)


(define (assert-equal? a b)
  (unless (equal? a b)
    (error 'unequal "~s ~s" a b)))


(assert-equal?
  (call-with-output-bytes
    (lambda (p)
      (write-foo2 (make-foo2) p)))
  #"")

(define example2
  (make-foo2
    #:bar (make-bar)))

(assert-equal?
  (call-with-output-bytes
    (lambda (p)
      (write-foo2 example2 p)))
  (bytes 10 0))

(define example3
  (make-bar
    #:string-opt "foo"
    #:string-repeated (list "1" "2")))

(assert-equal?
  (call-with-output-bytes
    (lambda (p)
      (write-bar example3 p)))
  (bytes-append
    (bytes 26 3)
    #"foo"
    (bytes 34 1)
    #"1"
    (bytes 34 1)
    #"2"))


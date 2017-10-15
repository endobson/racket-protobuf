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
  (let ([builder (foo2-builder)])
    (set-foo2-builder-bar! builder (bar-builder))
    (freeze-foo2 builder)))

(assert-equal?
  (call-with-output-bytes
    (lambda (p)
      (write-foo2 example2 p)))
  (bytes 10 0))



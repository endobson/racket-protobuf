#lang racket/base

(require
  (for-syntax
    racket/base
    protobuf/code-generation
    protobuf/convert-descriptors
    protobuf/proto-descriptors))

(provide (all-defined-out))

(define-syntax (go stx)
  (define out
    (call-with-input-file "{DESCRIPTOR}"
      (lambda (port)
        (define file-descriptor-set (parse-FileDescriptorSet port))
        (generate-code stx
          (convert-descriptors file-descriptor-set)))))
  out)
(go)


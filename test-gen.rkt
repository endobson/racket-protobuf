#lang racket/base

(require
  (for-syntax
    racket/base
    syntax/parse
    racket/list
    racket/pretty
    "../../code-generation.rkt"
    "../../convert-descriptors.rkt"
    "../../proto-descriptors.rkt"))


(define-syntax (go stx)
  (define out
    (call-with-input-file "{DESCRIPTOR}"
      (lambda (port)
        (define file-descriptor-set (parse-FileDescriptorSet port))
        (generate-code stx
          (convert-descriptors file-descriptor-set)))))
  out)
(go)


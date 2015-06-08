#lang racket/base

(require
  (for-syntax
    racket/base
    syntax/parse
    "proto-descriptors.rkt"
    "convert-descriptors.rkt"
    "parse-proto-file.rkt"
    "code-generation.rkt"))

(provide import-proto)

(define-syntax (import-proto stx)
  (syntax-parse stx
    [(_ path:str)
     (generate-code #'path
       (convert-descriptors
         (parse-proto-file (syntax-e #'path))))]))


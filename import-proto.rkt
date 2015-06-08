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
     (define dir (or (current-load-relative-directory) (current-directory)))
     (generate-code #'path
       (convert-descriptors
         (parse-proto-file (build-path dir (syntax-e #'path)))))]))


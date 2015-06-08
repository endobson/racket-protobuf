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
    [(_ #:root root-path:str path:str ...)
     (define dir (or (current-load-relative-directory) (current-directory)))
     (define root-dir (build-path dir (syntax-e #'root-path)))
     (generate-code stx
       (convert-descriptors
         (parse-proto-file
           root-dir
           (map
             (λ (p) (build-path root-dir p))
             (syntax->datum #'(path ...))))))]))


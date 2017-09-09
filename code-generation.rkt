#lang racket/base

(require
  (for-template racket/base)

  "code-generation/parser.rkt"
  "code-generation/structure.rkt"
  "code-generation/enum.rkt"
  "code-generation/message-identifiers.rkt"
  "message-descriptor.rkt")

(provide generate-code)

;; This generates the code given a list of message descriptors.
;; ctx: syntax? The lexical context for generated identifiers.
;; descirptors: (listof (or/c message-descriptor? enum-descriptor?)
;;   The messages to generate code for.
(define (generate-code ctx descriptors)
  (define mds (filter message-descriptor? descriptors))
  (define eds (filter enum-descriptor? descriptors))
  (define pids
    (for/hash ([md (in-list mds)])
      (values
        (message-descriptor-name md)
        (message-descriptor->proto-identifiers ctx md))))
  (define eids
    (for/hash ([ed (in-list eds)])
      (values
        (enum-descriptor-name ed)
        (enum-descriptor->enum-identifiers ctx ed))))


  #`(begin
      #,@(for/list ([md (in-list mds)])
           #`(begin
               #,(generate-message-structure pids eids md)
               #,(generate-builder-structure pids eids md)
               #,(generate-parser pids eids md)))
      #,@(for/list ([ed (in-list eds)])
           (generate-enum (hash-ref eids (enum-descriptor-name ed)) ed))))







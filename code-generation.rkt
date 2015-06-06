#lang racket/base

(require
  (for-template racket/base)

  "code-generation/parser.rkt"
  "code-generation/structure.rkt"
  "code-generation/message-identifiers.rkt"
  "message-descriptor.rkt")

(provide generate-code)

;; This generates the code given a list of message descriptors.
;; ctx: syntax? The lexical context for generated identifiers.
;; mds: (listof message-descriptor?) The messages to generate code for.
(define (generate-code ctx mds)
  (define pids
    (for/hash ([md (in-list mds)])
      (values
        (message-descriptor-name md)
        (message-descriptor->proto-identifiers ctx md))))

  #`(begin
      #,@(for/list ([md (in-list mds)])
           #`(begin
               #,(generate-message-structure pids md)
               #,(generate-builder-structure pids md)
               #,(generate-parser pids md)))))







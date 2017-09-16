#lang racket/base

(require
  (for-template racket/base)
  racket/contract

  "code-generation/parser.rkt"
  "code-generation/structure.rkt"
  "code-generation/enum.rkt"
  "code-generation/message-identifiers.rkt"
  "message-identifiers.rkt"
  "message-descriptor.rkt")

(provide 
  (contract-out
    [generate-code (-> (hash/c string? (or/c proto-identifiers? enum-identifiers?))
                       (listof (or/c message-descriptor? enum-descriptor?))
                       syntax?)]
    [make-type-identifier-dict
      (-> syntax?
          (listof (or/c message-descriptor? enum-descriptor?))
          (hash/c string? (or/c proto-identifiers? enum-identifiers?)))]))

;; This generates the code given a list of message descriptors.
(define (generate-code ids descriptors)
  (define mds (filter message-descriptor? descriptors))
  (define eds (filter enum-descriptor? descriptors))

  #`(begin
      #,@(for/list ([md (in-list mds)])
           #`(begin
               #,(generate-message-structure ids ids md)
               #,(generate-builder-structure ids ids md)
               #,(generate-parser ids ids md)))
      #,@(for/list ([ed (in-list eds)])
           (generate-enum (hash-ref ids (enum-descriptor-name ed)) ed))))

(define (make-type-identifier-dict ctx descriptors)
  (for/hash ([desc (in-list descriptors)])
    (cond
      [(message-descriptor? desc)
       (values
         (message-descriptor-name desc)
         (message-descriptor->proto-identifiers ctx desc))]
      [(enum-descriptor? desc)
       (values
         (enum-descriptor-name desc)
         (enum-descriptor->enum-identifiers ctx desc))])))

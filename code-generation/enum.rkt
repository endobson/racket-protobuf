#lang racket/base

(require
  (for-template
    racket/base)
  racket/match
  racket/syntax
  "../message-identifiers.rkt"
  "../message-descriptor.rkt")

(provide generate-enum)


(define (generate-enum enum-ids desc)
  (define/with-syntax (predicate enum-list enum->number number->enum)
    (match enum-ids
      [(enum-identifiers predicate enum-list enum->number number->enum)
       (list predicate enum-list enum->number number->enum)]))

  (define symbols
    (for/list ([enum-value-descriptor (enum-descriptor-values desc)])
      (string->symbol (enum-value-descriptor-name enum-value-descriptor))))

  #`(begin
      (define (predicate x)
        (case x
          [#,symbols #t]
          [else #f]))
      (define enum-list '#,symbols)
      (define (enum->number enum)
        (case enum
          #,@(for/list ([enum-value-descriptor (enum-descriptor-values desc)])
               #`[(#,(string->symbol (enum-value-descriptor-name enum-value-descriptor)))
                  '#,(enum-value-descriptor-value enum-value-descriptor)])))
      (define (number->enum number)
        (case number
          #,@(for/list ([enum-value-descriptor (enum-descriptor-values desc)])
               #`[(#,(enum-value-descriptor-value enum-value-descriptor))
                  '#,(string->symbol (enum-value-descriptor-name enum-value-descriptor))])))))

#lang racket/base

(require
  (for-syntax
    racket/base
    syntax/parse
    "code-generation.rkt"
    "message-descriptor.rkt"))
(provide define-proto)

(begin-for-syntax
  (define-syntax-class proto-type
    (pattern (~datum string))
    (pattern (~datum boolean))
    (pattern (~datum int32))
    (pattern type-ref:str))

  (define-syntax-class field-spec
    #:attributes (number descriptor)
    [pattern
      ((~and multiplicity (~or (~datum optional) (~datum repeated)))
       type:proto-type
       name:id
       (~datum =)
       number:exact-positive-integer)
      #:attr descriptor 
        (field-descriptor
          (syntax-e (attribute multiplicity))
          (syntax-e (attribute type))
          (syntax-e (attribute name)))])
  (define-syntax-class message-spec
    #:attributes (descriptor)
    [pattern ((~datum message) name:str fields:field-spec ...)
      #:attr descriptor
        (message-descriptor
          (syntax-e (attribute name))
          (make-hash
            (map cons (attribute fields.number) (attribute fields.descriptor))))]))


(define-syntax (define-proto stx)
  (syntax-parse stx
    [(_ messages:message-spec ...)
     (generate-code stx (attribute messages.descriptor))]))



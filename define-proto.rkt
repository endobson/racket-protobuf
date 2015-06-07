#lang racket/base

(require
  (for-syntax
    racket/base
    syntax/parse
    "code-generation.rkt"
    "message-descriptor.rkt"))
(provide define-proto)

(begin-for-syntax
  (define-splicing-syntax-class proto-type
    #:attributes (value)
    (pattern (~datum string) #:attr value 'string)
    (pattern (~datum boolean) #:attr value 'boolean)
    (pattern (~datum int32) #:attr value 'int32)
    (pattern (~seq (~datum message) type-ref:str)
      #:attr value (list 'message (syntax-e (attribute type-ref))))
    (pattern (~seq (~datum enum) type-ref:str)
      #:attr value (list 'enum (syntax-e (attribute type-ref)))))

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
          (attribute type.value)
          (symbol->string (syntax-e (attribute name))))])

  (define-syntax-class enum-value-spec
    #:attributes (descriptor)
    [pattern (name:id (~datum =) number:integer)
      #:attr descriptor 
        (enum-value-descriptor
          (symbol->string (syntax-e (attribute name)))
          (syntax-e (attribute number)))])

  (define-syntax-class message-spec
    #:attributes (descriptor)
    [pattern ((~datum message) name:str fields:field-spec ...)
      #:attr descriptor
        (message-descriptor
          (syntax-e (attribute name))
          (make-hash
            (map cons (attribute fields.number) (attribute fields.descriptor))))]
    [pattern ((~datum enum) name:str values:enum-value-spec ...)
      #:attr descriptor
        (enum-descriptor
          (syntax-e (attribute name))
          (attribute values.descriptor))]))


(define-syntax (define-proto stx)
  (syntax-parse stx
    [(_ messages:message-spec ...)
     (generate-code stx (attribute messages.descriptor))]))



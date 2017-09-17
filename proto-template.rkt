#lang racket/base

(require
  (for-syntax
    (for-syntax
      racket/base
      racket/port)
    racket/base
    racket/match
    racket/hash
    racket/list
    racket/syntax
    protobuf/code-generation
    protobuf/convert-descriptors
    protobuf/message-identifiers
    protobuf/proto-descriptors))

(provide (except-out
           (all-defined-out)
           gen-requires
           go))



(begin-for-syntax
  (define-syntax (gen-raw-descriptor stx)
    (call-with-input-file "{DESCRIPTOR}"
      (lambda (port)
        #`'#,(port->bytes port))))
  (define source-path "{SOURCE_PATH}")
  (define raw-descriptor (gen-raw-descriptor))
  (define file-descriptor-set (parse-FileDescriptorSet (open-input-bytes raw-descriptor)))
  (define file-descriptor
    (or
      (findf (lambda (desc) (equal? source-path (FileDescriptorProto-name desc)))
             (FileDescriptorSet-file file-descriptor-set))
      (error 'proto-template "No descriptor for ~a." source-path)))
  (define defined-descriptors (convert-file-descriptor file-descriptor))
  (define defined-type-ids (make-type-identifier-dict #'here defined-descriptors)))

(module* type-ids #f
  (provide (for-syntax type-ids))
  (define-for-syntax type-ids defined-type-ids))

(define-syntax (gen-requires stx)
  (syntax-case stx ()
    [(_ imported-ids)
     (let ()
       (define-values (ids requires)
         (for/lists (ids requires) ([dependency (in-list (FileDescriptorProto-dependency
                                                           file-descriptor))])
           (define lib-name
             (string-append
               "protogen/"
               (substring dependency 0 (- (string-length dependency) 6))
               "-proto"))
           (define id (generate-temporary 'type-id))
           (values
             id
             #`(require 
                 #,(string->symbol lib-name)
                 (only-in (submod #,(string->symbol lib-name) type-ids) [type-ids #,id])))))
       #`(begin #,@requires 
                (define-for-syntax imported-ids (hash-union defined-type-ids #,@ids))))]))
(gen-requires imported-ids)

(define-syntax (go stx)
  (define (introduce ids)
    (match ids
      [(proto-identifiers message builder) 
       (proto-identifiers (introduce message) (introduce builder))]
      [(message-identifiers constructor fields parser serializer freezer)
       (message-identifiers
         (syntax-local-introduce constructor)
         (for/hash ([(k v) (in-hash fields)])
           (values k (introduce v)))
         (syntax-local-introduce parser)
         (syntax-local-introduce serializer)
         (syntax-local-introduce freezer))]
      [(singular-field-identifiers accessor)
       (singular-field-identifiers (syntax-local-introduce accessor))]
      [(repeated-field-identifiers accessor)
       (repeated-field-identifiers (syntax-local-introduce accessor))]
      [(builder-identifiers constructor fields parser serializer copier)
       (builder-identifiers
         (syntax-local-introduce constructor)
         (for/hash ([(k v) (in-hash fields)])
           (values k (introduce v)))
         (syntax-local-introduce parser)
         (syntax-local-introduce serializer)
         #f ;(syntax-local-introduce copier)
         )]
      [(builder-singular-field-identifiers accessor mutator available-predicate clearer )
       (builder-singular-field-identifiers
         (syntax-local-introduce accessor)
         (syntax-local-introduce mutator)
         #f ;(syntax-local-introduce available-predicate)
         #f ;(syntax-local-introduce clearer)
         )]
      [(builder-repeated-field-identifiers
         count accessor setter adder list-adder remover clearer
         index-builder-accessor list-builder-accessor builder-adder)
       (builder-repeated-field-identifiers
         #f ;(syntax-local-introduce count)
         (syntax-local-introduce accessor)
         #f ;(syntax-local-introduce setter)
         (syntax-local-introduce adder)
         #f ;(syntax-local-introduce list-adder)
         #f ;(syntax-local-introduce remover)
         #f ;(syntax-local-introduce clearer)
         #f ;(syntax-local-introduce index-builder-accessor)
         #f ;(syntax-local-introduce list-builder-accessor)
         #f ;(syntax-local-introduce builder-adder)
         )]
      [(enum-identifiers predicate list enum->number number->enum)
       (enum-identifiers
         (syntax-local-introduce predicate)
         (syntax-local-introduce list)
         (syntax-local-introduce enum->number)
         (syntax-local-introduce number->enum))]))

  (define introduced-ids
    (for/hash ([(k v) (in-hash imported-ids)])
      (values k (introduce v))))
  (generate-code introduced-ids defined-descriptors))
(go)


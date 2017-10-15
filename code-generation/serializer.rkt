#lang racket/base

(require
  (for-template
    racket/base
    racket/port
    racket/list
    "template-code.rkt"
    "../varint.rkt")

  "../message-identifiers.rkt"
  "../message-descriptor.rkt"
  racket/list
  racket/match
  racket/syntax)

(provide generate-serializer)

(define (type->wire-type type)
  (match type
    [(or 'string 'bytes (list 'message _) (? string?))
     'length-delimited]
    [(or 'int32 'boolean (list 'enum _))
     'varint]))

(define (proto-field-tag field-number type)
  (define encoding
    (case type
      [(varint) 0]
      [(64bit) 1]
      [(length-delimited) 2]
      [(32bit) 5]))
  (+ (arithmetic-shift field-number 3) encoding))



(define (generate-serializer proto-ids enum-ids desc)
  (define name  (message-descriptor-name desc))
  (define message-ids (proto-identifiers-message (hash-ref proto-ids name)))
  (define builder-ids (proto-identifiers-builder (hash-ref proto-ids name)))

  (define (optional-field-clause field-number type)
    (match-define (singular-field-identifiers accessor)
      (hash-ref (message-identifiers-fields message-ids) field-number))
    (define (write-tag)
      #`(write-varint #,(proto-field-tag field-number (type->wire-type type)) port))

    #`(let ([val (#,accessor message)])
        #,(match type
            ['string
             #`(unless (equal? val "")
                 #,(write-tag)
                 (define bytes (string->bytes/utf-8 string))
                 (write-varint (bytes-length bytes) port)
                 (write-bytes bytes port))]
            ['bytes
             #`(unless (equal? val #"")
                 #,(write-tag)
                 (write-varint (bytes-length val) port)
                 (write-bytes val port))]
            ['int32 
             #`(unless (equal? val 0)
                 #,(write-tag)
                 (write-varint val port))]
            ['boolean #`(error 'nyi)]
            [(list 'enum enum-type)
             #`(let ([int-val (#,(enum-identifiers-enum->number (hash-ref enum-ids enum-type))
                              val)])
                 (unless (equal? int-val 0)
                   #,(write-tag)
                   (write-varint int-val port)))]
            [(list 'message message-type)
             (define/with-syntax sub-serializer
               (message-identifiers-serializer (proto-identifiers-message (hash-ref proto-ids message-type))))
             #`(when val
                 #,(write-tag)
                 (write-varint (protobuf-base-byte-size val) port)
                 (sub-serializer val port))])))

  (define (repeated-field-clause field-number type)
    (match-define (repeated-field-identifiers accessor)
      (hash-ref (message-identifiers-fields message-ids) field-number))
    (define (write-tag)
      #`(write-varint #,(proto-field-tag field-number (type->wire-type type)) port))
    #`(for ([val (in-list (#,accessor message))])
        #,(match type
            ['string
             #`(begin
                 #,(write-tag)
                 (define bytes (string->bytes/utf-8 string))
                 (write-varint (bytes-length bytes) port)
                 (write-bytes bytes port))]
            ['bytes
             #`(begin
                 #,(write-tag)
                 (write-varint (bytes-length bytes) port)
                 (write-bytes bytes port))]
            ['int32 
             #`(begin
                 #,(write-tag)
                 (write-varint val port))]
            ['boolean #`(error 'nyi)]
            [(list 'enum enum-type)
             #`(let ([int-val (#,(enum-identifiers-enum->number (hash-ref enum-ids enum-type))
                              val)])
                 #,(write-tag)
                 (write-varint int-val port))]
            [(list 'message message-type)
             (define/with-syntax sub-serializer
               (message-identifiers-serializer (proto-identifiers-message (hash-ref proto-ids message-type))))
             #`(begin
                 #,(write-tag)
                 (write-varint (protobuf-base-byte-size val) port)
                 (sub-serializer val port))])))

  (define/with-syntax message-serializer (message-identifiers-serializer message-ids))
  (define/with-syntax builder-serializer (builder-identifiers-serializer builder-ids))

  #`(begin
      (define (builder-serializer builder port)
        (message-serializer
          (#,(message-identifiers-freezer message-ids) builder)
          port))

      (define (message-serializer message port)
        #,@(for/list ([(field-number fd) (message-descriptor-fields desc)])
             (match-define (field-descriptor multiplicity type _) fd)
             (case multiplicity
               [(optional)
                (optional-field-clause field-number type)]
               [(repeated)
                (repeated-field-clause field-number type)]))
        (void))))

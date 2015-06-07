#lang racket/base

(require
  (for-template
    racket/base
    racket/port
    racket/list
    "../proto-reader.rkt")

  "../message-identifiers.rkt"
  "../message-descriptor.rkt"
  racket/match
  racket/syntax)

(provide generate-parser)

(define (type->expected-wire-type type)
  (match type
    [(or 'string 'bytes (? string?))
     'length-delimited]
    [(or 'int32 'boolean)
     'varint]))

(define (generate-parser proto-ids desc)
  (define name  (message-descriptor-name desc))
  (define message-ids (proto-identifiers-message (hash-ref proto-ids name)))
  (define builder-ids (proto-identifiers-builder (hash-ref proto-ids name)))
  (define field-clauses
    (for/list ([(field-number fd) (message-descriptor-fields desc)])
      (match-define (field-descriptor multiplicity type _) fd)
      #`[(#,field-number)
         (unless (equal? wire-type '#,(type->expected-wire-type type))
           (error 'parse-proto "Bad wire type"))
         #,(case multiplicity
             [(optional)
              (match-define (builder-singular-field-identifiers accessor* mutator* _ _)
                (hash-ref (builder-identifiers-fields builder-ids) field-number))
              (define/with-syntax accessor accessor*)
              (define/with-syntax mutator mutator*)
              (case type
               [(string)
                #`(let ([amount (read-proto-varint port)])
                    (mutator current-builder (read-proto-string port amount)))]
               [(bytes) #`(mutator current-builder
                           (read-proto-bytes port (read-proto-varint port)))]
               ;; TODO make this cap in 32 bit range
               [(int32) #`(mutator current-builder (read-proto-varint port))]
               ;; TODO make this work
               [(boolean) #`(error 'nyi)]
               [else
                 (define sub-ids (proto-identifiers-builder (hash-ref proto-ids type)))
                 (define/with-syntax sub-constructor
                   (builder-identifiers-constructor sub-ids))
                 (define/with-syntax sub-parser
                   (builder-identifiers-parser sub-ids))
                 #'(let ()
                     (define sub-builder
                       (let ([old-value (accessor current-builder)])
                         (or old-value
                             (let ([new-value (sub-constructor)])
                               (mutator current-builder new-value)
                               new-value))))
                     (define len (read-proto-varint port))
                     (sub-parser
                       (make-limited-input-port port len)
                       sub-builder))])]
             [(repeated)
              (match-define (builder-repeated-field-identifiers _ accessor* _ adder* _ _ _ _ _ _)
                (hash-ref (builder-identifiers-fields builder-ids) field-number))
              (define/with-syntax accessor accessor*)
              (define/with-syntax adder adder*)
              #`(adder current-builder
                  #,(case type
                     [(string) #'(read-proto-string port (read-proto-varint port))]
                     [(bytes) #'(read-proto-bytes port (read-proto-varint port))]
                     ;; TODO make this cap in 32 bit range
                     [(int32) #'(read-proto-varint port)]
                     ;; TODO make this work
                     [(boolean) #`(error 'nyi)]
                     [else
                       (define sub-ids (hash-ref proto-ids type))
                       (define/with-syntax sub-constructor
                         (builder-identifiers-constructor (proto-identifiers-builder sub-ids)))
                       (define/with-syntax sub-parser
                         (builder-identifiers-parser (proto-identifiers-builder sub-ids)))
                       #'(let ([len (read-proto-varint port)])
                           (sub-parser
                             (make-limited-input-port port len)
                             (sub-constructor)))]))])]))


  (define message-parser (message-identifiers-parser message-ids))
  (define builder-parser (builder-identifiers-parser builder-ids))

  #`(begin
      (define (#,builder-parser port current-builder)
          (let loop ()
            (define varint (read-proto-varint port #t))
            (unless (eof-object? varint)
              (define-values (field-number wire-type) (extract-wire-key varint))
              (case field-number
                #,@field-clauses
                [else
                  ;; Read through the unknown field
                  (case wire-type
                    [(varint) (read-proto-varint port)]
                    [(length-delimited)
                     (define len (read-proto-varint port))
                     (read-proto-bytes port len)]
                    [else (error 'nyi)])])
              (loop)))
          current-builder)


      (define (#,message-parser port)
        (#,(message-identifiers-freezer message-ids)
         (#,builder-parser port (#,(builder-identifiers-constructor builder-ids)))))))

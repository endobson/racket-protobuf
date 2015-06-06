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

(define (generate-parser message-ids desc)
  (define name  (message-descriptor-name desc))
  (define ids (proto-identifiers-message (hash-ref message-ids name)))
  (define parser (message-identifiers-parser ids))
  (define field-clauses
    (for/list ([(field-number fd) (message-descriptor-fields desc)])
      (match-define (field-descriptor multiplicity type _) fd)
      #`[(#,field-number)
         (unless (equal? wire-type '#,(type->expected-wire-type type))
           (error 'parse-proto "Bad wire type"))
         #,(case multiplicity
             [(optional)
              (match-define (singular-field-identifiers accessor* mutator*)
                (hash-ref (message-identifiers-fields ids) field-number))
              (define/with-syntax accessor accessor*)
              (define/with-syntax mutator mutator*)
              (case type
               [(string)
                #`(let ([amount (read-proto-varint port)])
                    (mutator current-proto (read-proto-string port amount)))]
               [(bytes) #`(mutator current-proto
                           (read-proto-bytes port (read-proto-varint port)))]
               ;; TODO make this cap in 32 bit range
               [(int32) #`(mutator current-proto (read-proto-varint port))]
               ;; TODO make this work
               [(boolean) #`(error 'nyi)]
               [else
                 (define sub-ids (proto-identifiers-message (hash-ref message-ids type)))
                 (define/with-syntax sub-constructor
                   (message-identifiers-constructor sub-ids))
                 (define/with-syntax sub-parser
                   (message-identifiers-parser sub-ids))
                 #'(let ()
                     (define sub-message
                       (let ([old-value (accessor current-proto)])
                         (or old-value
                             (let ([new-value (sub-constructor)])
                               (mutator current-proto new-value)
                               new-value))))
                     (define len (read-proto-varint port))
                     (sub-parser
                       (make-limited-input-port port len)
                       sub-message))])]
             [(repeated)
              (match-define (repeated-field-identifiers accessor* adder*)
                (hash-ref (message-identifiers-fields ids) field-number))
              (define/with-syntax accessor accessor*)
              (define/with-syntax adder adder*)
              #`(adder current-proto
                  #,(case type
                     [(string) #'(read-proto-string port (read-proto-varint port))]
                     [(bytes) #'(read-proto-bytes port (read-proto-varint port))]
                     ;; TODO make this cap in 32 bit range
                     [(int32) #'(read-proto-varint port)]
                     ;; TODO make this work
                     [(boolean) #`(error 'nyi)]
                     [else
                       (define sub-ids (proto-identifiers-message (hash-ref message-ids type)))
                       (define/with-syntax sub-constructor
                         (message-identifiers-constructor sub-ids))
                       (define/with-syntax sub-parser
                         (message-identifiers-parser sub-ids))
                       #'(let ([len (read-proto-varint port)])
                           (sub-parser
                             (make-limited-input-port port len)
                             (sub-constructor)))]))])]))


  #`(define (#,parser port current-proto)
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
      current-proto))
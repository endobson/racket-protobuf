#lang racket/base

(require
  (for-template
    racket/base
    racket/list
    "proto-reader.rkt")

  racket/match
  racket/syntax)

(provide (all-defined-out))

;; A protocol message has the following things at runtime
;; Field accessors

;; Constructor 

(struct message-identifiers (constructor fields parser serializer))
(struct singular-field-identifiers (accessor mutator))
(struct repeated-field-identifiers (accessor adder))

;; fields: (hash/c exact-positive-integer? field-descriptor)
(struct message-descriptor (name fields) #:transparent)
;; type: string?
(struct field-descriptor (multiplicity type) #:transparent)


(define (type->expected-wire-type type)
  (match type
    [(or 'string 'bytes (? string?))
     'length-delimited]
    ['int32
     'varint]))

(define (generate-structure message-ids desc)
  (match-define (message-descriptor name fields) desc)
  (define ids (hash-ref message-ids name))
  (define/with-syntax constructor (message-identifiers-constructor ids))
  (define/with-syntax (type-descriptor predicate accessor mutator)
    (generate-temporaries '(type-descriptor predicate accessor mutator)))
  #`(begin
      (define-values (type-descriptor constructor predicate accessor mutator)
        (make-struct-type '#,(string->symbol name) #f #,(hash-count fields) 0 #f empty #f))))


(define (generate-parser message-ids desc)
  (define ids (hash-ref message-ids (message-descriptor-name desc)))
  (define parser (message-identifiers-parser ids))
  (define field-clauses
    (for/list ([(field-number fd) (message-descriptor-fields desc)])
      (match-define (field-descriptor multiplicity type) fd)
                    
      #`[(#,field-number)
         (unless (equal? wire-type '#,(type->expected-wire-type type))
           (error 'parse-proto "Bad wire type"))
         #,(case multiplicity
             [(optional)
              (match-define (singular-field-identifiers accessor* mutator*)
                (message-identifiers-fields ids))
              (define/with-syntax accessor accessor*)
              (define/with-syntax mutator mutator*)
              (case type
               [(string) #`(mutator current-proto
                            (read-proto-string port (read-proto-varint port)))]
               [(bytes) #`(mutator current-proto
                           (read-proto-bytes port (read-proto-varint port)))]
               ;; TODO make this cap in 32 bit range
               [(int32) #`(mutator current-proto (read-proto-varint port))]
               [else
                 (define sub-ids (hash-ref message-ids type))
                 (define/with-syntax sub-constructor
                   (message-identifiers-constructor sub-ids))
                 (define/with-syntax sub-parser
                   (message-identifiers-parser sub-ids))
                 ;; read length off of wire
                 #'(let ()
                     (define sub-message
                       (let ([old-value (accessor current-proto)])
                         (or old-value
                             (let ([new-value (sub-constructor)])
                               (mutator current-proto new-value)))))
                     (sub-parser port sub-message))])]
             [(repeated)
              (match-define (repeated-field-identifiers accessor* adder*)
                (message-identifiers-fields ids))
              (define/with-syntax accessor accessor*)
              (define/with-syntax adder adder*)
              #`(adder current-proto
                  #,(case type
                     [(string) #'(read-proto-string port (read-proto-varint port))]
                     [(bytes) #'(read-proto-bytes port (read-proto-varint port))]
                     ;; TODO make this cap in 32 bit range
                     [(int32) #'(read-proto-varint port)]
                     [else
                       (define sub-ids (hash-ref message-ids type))
                       (define/with-syntax sub-constructor
                         (message-identifiers-constructor sub-ids))
                       (define/with-syntax sub-parser
                         (message-identifiers-parser sub-ids))
                       ;; read length off of wire
                       #'(sub-parser port (sub-constructor))]))])]))


  #`(define (#,parser port current-proto)
      (define varint (read-proto-varint port #t))
      (define-values (field-number wire-type) (extract-wire-key varint))
      (unless (eof-object? varint)
        (case field-number
          #,@field-clauses))
      current-proto))





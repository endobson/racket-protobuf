#lang racket/base
(require
  "proto-reader.rkt"
  racket/port
  racket/match)

(provide (all-defined-out))

;; Structures needed for runtime code generation of a parser

(struct generic-mutable-message (singular-fields repeated-fields) #:transparent)
;; messages: (hash/c string? message-descriptor?)
(struct descriptor-set (messages) #:transparent)
;; fields: (hash/c exact-positive-integer? field-descriptor)
(struct message-descriptor ([fields #:mutable] constructor) #:transparent)
;; type: string?
(struct field-descriptor (multiplicity type) #:transparent)

(define (primitive-type? x)
  (symbol? x))

(define (parse-proto port md)
  (define v ((message-descriptor-constructor md)))
  (fill-in-proto md v port)
  v)

(define (fill-in-proto md v port)
  (define varint (read-proto-varint port #t))
  (unless (eof-object? varint)
    (define-values (field-number wire-type) (extract-wire-key varint))
    (match (hash-ref (message-descriptor-fields md) field-number #f)
      [#f (drop-proto port wire-type)]
      [(field-descriptor multiplicity type)
       (cond
        [(not (equal? (type->expected-wire-type type) wire-type))
         (error 'parse-proto "Bad wire type")]
        [(equal? 'optional multiplicity)
         (cond
           [(primitive-type? type)
            (generic-mutable-message-set-singular-field! v field-number (read-proto-primitive port type))]
           [else
             (define inner
               (generic-mutable-message-get-singular-field! v field-number
                 (message-descriptor-constructor type)))
             (fill-in-proto type inner (make-limited-input-port port (read-proto-varint port)))])]
        [(equal? 'repeated multiplicity)
         (generic-mutable-message-add-repeated-field! v field-number
           (cond
             [(primitive-type? type)
              (read-proto-primitive port type)]
             [else
               (define inner ((message-descriptor-constructor type)))
               (fill-in-proto type inner (make-limited-input-port port (read-proto-varint port)))]))]
        [else (error 'parse-proto "Fell through")])])
    (fill-in-proto md v port))
  v)


      


(define (generic-mutable-message-get-singular-field! gm number default-cons)
  (hash-ref!
    (generic-mutable-message-singular-fields gm)
    number default-cons))

(define (generic-mutable-message-set-singular-field! gm number value)
  (hash-set!
    (generic-mutable-message-singular-fields gm)
    number value))

(define (generic-mutable-message-add-repeated-field! gm number value)
  (hash-update!
    (generic-mutable-message-repeated-fields gm)
    number
    (λ (vs) (cons value vs))
    null))

(define (type->expected-wire-type type)
  (match type
    [(or 'string 'bytes (? message-descriptor?))
     'length-delimited]
    ['int32
     'varint]))



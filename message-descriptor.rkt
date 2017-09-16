#lang racket/base

(require
  racket/contract)

(provide
  (contract-out
    (struct message-descriptor
            ([name immutable-string/c]
             [fields (hash/c exact-positive-integer? field-descriptor? #:immutable #t)]))
    (struct field-descriptor
            ([multiplicity (or/c 'optional 'repeated)]
             [type (or/c 'int32 'string 'bytes 'boolean (list/c (or/c 'enum 'message) immutable-string/c))]
             [name immutable-string/c]))
    (struct enum-descriptor
            ([name immutable-string/c]
             [values (listof enum-value-descriptor?)]))
    (struct enum-value-descriptor
            ([name immutable-string/c]
             [value exact-integer?]))))

(define immutable-string/c (and/c immutable? string?))

(struct message-descriptor (name fields) #:transparent)
(struct field-descriptor (multiplicity type name) #:transparent)
(struct enum-descriptor (name values) #:transparent)
(struct enum-value-descriptor (name value) #:transparent)
(struct type-name (package local-name))

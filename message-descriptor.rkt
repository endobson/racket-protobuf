#lang racket/base


(provide
  (struct-out message-descriptor)
  (struct-out field-descriptor)
  (struct-out enum-descriptor)
  (struct-out enum-value-descriptor))

;; name: string?
;; fields: (hash/c exact-positive-integer? field-descriptor?)
(struct message-descriptor (name fields) #:transparent)
;; multiplicity: (or/c 'optional 'repeated)
;; type: (or/c 'int32 'string 'bytes 'boolean string?)
;; name: string?
(struct field-descriptor (multiplicity type name) #:transparent)

;; name: string?
;; values: (listof enum-value-descriptor?)
(struct enum-descriptor (name values) #:transparent)
;; name: string?
;; value: integer?
(struct enum-value-descriptor (name value) #:transparent)

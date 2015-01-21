#lang racket/base


(provide
  (struct-out message-descriptor)
  (struct-out field-descriptor))

;; fields: (hash/c exact-positive-integer? field-descriptor)
(struct message-descriptor (name fields) #:transparent)
;; type: (or/c 'int32 'string 'bytes 'boolean string?)
;; name: string?
(struct field-descriptor (multiplicity type name) #:transparent)

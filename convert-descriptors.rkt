#lang racket/base

(require
  racket/list
  racket/match
  "message-descriptor.rkt"
  "proto-descriptors.rkt")

(provide convert-file-descriptor)

(define (convert-file-descriptor file-descriptor)
  (define package-name (string-append "." (file-descriptor-proto-package file-descriptor)))
  (flatten
    (list
      (map
        (convert-message-type package-name)
        (file-descriptor-proto-message_type file-descriptor))
      (map
        (convert-enum-type package-name)
        (file-descriptor-proto-enum_type file-descriptor)))))


(define ((convert-enum-type container-name) enum-type)
  (enum-descriptor
    (string->immutable-string
      (string-append container-name "." (enum-descriptor-proto-name enum-type)))
    (for/list ([value (enum-descriptor-proto-value enum-type)])
      (enum-value-descriptor
        (string->immutable-string (enum-value-descriptor-proto-name value))
        (enum-value-descriptor-proto-number value)))))


(define ((convert-message-type container-name) message-type)
  (define message-name
    (string-append container-name "." (descriptor-proto-name message-type)))
  (define self
    (message-descriptor
      (string->immutable-string message-name)

      ;; TODO once string fields of protos are immutable, remove the string->immutable-string conversions
      (for/hash ([field (descriptor-proto-field message-type)])
        (values
          (field-descriptor-proto-number field)
          (field-descriptor
            (match (field-descriptor-proto-label field)
              ['LABEL_OPTIONAL 'optional]
              ['LABEL_REQUIRED 'optional] ;; Required fields are not supported
              ['LABEL_REPEATED 'repeated])
            (match (field-descriptor-proto-type field)
              ['TYPE_STRING 'string]
              ['TYPE_BYTES 'bytes]
              ['TYPE_BOOL 'boolean]
              ['TYPE_INT32 'int32]
              ;; TODO handle the following types correctly
              ['TYPE_UINT32 'bytes]
              ['TYPE_INT64 'bytes]
              ['TYPE_UINT64 'bytes]
              ['TYPE_DOUBLE 'bytes]
              ['TYPE_FLOAT 'bytes]
              ['TYPE_MESSAGE
               (list 'message (string->immutable-string (field-descriptor-proto-type_name field)))]
              ['TYPE_ENUM
               (list 'enum (string->immutable-string (field-descriptor-proto-type_name field)))])
            (string->immutable-string (field-descriptor-proto-name field)))))))
  (list
    self
    (map (convert-enum-type message-name) (descriptor-proto-enum_type message-type))
    (map (convert-message-type message-name) (descriptor-proto-nested_type message-type))))


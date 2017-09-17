#lang racket/base

(require
  racket/list
  racket/match
  "message-descriptor.rkt"
  "proto-descriptors.rkt")

(provide convert-file-descriptor)

(define (convert-file-descriptor file-descriptor)
  (flatten
    (map
      (convert-message-type (string-append "." (FileDescriptorProto-package file-descriptor)))
      (FileDescriptorProto-message_type file-descriptor))))


(define ((convert-enum-type container-name) enum-type)
  (enum-descriptor
    (string-append container-name "." (EnumDescriptorProto-name enum-type))
    (for/list ([value (EnumDescriptorProto-value enum-type)])
      (enum-value-descriptor
        (EnumValueDescriptorProto-name value)
        (EnumValueDescriptorProto-number value)))))


(define ((convert-message-type container-name) message-type)
  (define message-name
    (string-append container-name "." (DescriptorProto-name message-type)))
  (define self
    (message-descriptor
      (string->immutable-string message-name)

      ;; TODO once string fields of protos are immutable, remove the string->immutable-string conversions
      (for/hash ([field (DescriptorProto-field message-type)])
        (values
          (FieldDescriptorProto-number field)
          (field-descriptor
            (match (FieldDescriptorProto-label field)
              ['LABEL_OPTIONAL 'optional]
              ['LABEL_REQUIRED 'optional] ;; Required fields are not supported
              ['LABEL_REPEATED 'repeated])
            (match (FieldDescriptorProto-type field)
              ['TYPE_STRING 'string]
              ['TYPE_BYTES 'bytes]
              ['TYPE_BOOL 'boolean]
              ['TYPE_INT32 'int32]
              ;; TODO handle the following types correctly
              ['TYPE_INT64 'bytes]
              ['TYPE_UINT64 'bytes]
              ['TYPE_DOUBLE 'bytes]
              ['TYPE_MESSAGE
               (list 'message (string->immutable-string (FieldDescriptorProto-type_name field)))]
              ['TYPE_ENUM
               (list 'enum (string->immutable-string (FieldDescriptorProto-type_name field)))])
            (string->immutable-string (FieldDescriptorProto-name field)))))))
  (list
    self
    (map (convert-enum-type message-name) (DescriptorProto-enum_type message-type))
    (map (convert-message-type message-name) (DescriptorProto-nested_type message-type))))


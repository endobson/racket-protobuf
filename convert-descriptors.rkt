#lang racket/base

(require
  racket/list
  racket/match
  "message-descriptor.rkt"
  "proto-descriptors.rkt")

(provide convert-descriptors)

(define (convert-descriptors file-descriptor-set)
  (for/fold ([acc empty]) ([file-descriptor (FileDescriptorSet-file file-descriptor-set)])
    (convert-descriptor file-descriptor acc)))

(define (convert-descriptor file-descriptor acc)
  (convert-message-types
    (string-append "." (FileDescriptorProto-package file-descriptor))
    (FileDescriptorProto-message_type file-descriptor)
    acc))

(define (convert-message-types container-name message-types acc)
  (for/fold ([acc acc]) ([message-type message-types])
    (convert-message-type container-name message-type acc)))

(define (convert-enum-types container-name enum-types acc)
  (for/fold ([acc acc]) ([enum-type enum-types])
    (convert-enum-type container-name enum-type acc)))

(define (convert-enum-type container-name enum-type acc)
  (define enum-name
    (string-append container-name "." (EnumDescriptorProto-name enum-type)))
  (cons
    (enum-descriptor
      enum-name
      (for/list ([value (EnumDescriptorProto-value enum-type)])
        (enum-value-descriptor
          (EnumValueDescriptorProto-name value)
          (EnumValueDescriptorProto-number value))))
    acc))


(define (convert-message-type container-name message-type acc)
  (define message-name
    (string-append container-name "." (DescriptorProto-name message-type)))
  (define self
    (message-descriptor
      message-name
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
               (list 'message (FieldDescriptorProto-type_name field))]
              ['TYPE_ENUM
               (list 'enum (FieldDescriptorProto-type_name field))])
            (FieldDescriptorProto-name field))))))

  (cons self
    (convert-enum-types message-name (DescriptorProto-enum_type message-type)
      (convert-message-types message-name (DescriptorProto-nested_type message-type) acc))))


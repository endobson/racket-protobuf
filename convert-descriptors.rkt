#lang racket/base

(require
  racket/list
  racket/match
  "message-descriptor.rkt"
  "proto-descriptors.rkt")

(provide convert-descriptors)

(define (convert-descriptors file-descriptor-set)
  (for/fold ([descriptors empty]) ([file-descriptor (FileDescriptorSet-file file-descriptor-set)])
    (append (convert-descriptor file-descriptor) descriptors)))

(define (convert-descriptor file-descriptor)
  (for/list ([message-type (FileDescriptorProto-message_type file-descriptor)])
    (convert-message-type message-type)))

(define (convert-message-type message-type)
  (message-descriptor
    (DescriptorProto-name message-type)
    (for/hash ([field (DescriptorProto-field message-type)])
      (values
        (FieldDescriptorProto-number field)
        (field-descriptor
          (match (FieldDescriptorProto-label field)
            ['LABEL_OPTIONAL 'optional]
            ['LABEL_REPEATED 'repeated])
          (match (FieldDescriptorProto-type field)
            ['TYPE_STRING 'string]
            ['TYPE_BYTES 'bytes]
            ['TYPE_BOOL 'boolean]
            ['TYPE_INT32 'int32]
            ;; TODO handle the following types
            ['TYPE_INT64 'int64]
            ['TYPE_UINT64 'uint64]
            ['TYPE_DOUBLE 'double]
            ['TYPE_MESSAGE
             (list 'message (FieldDescriptorProto-type_name field))]
            ['TYPE_ENUM
             (list 'enum (FieldDescriptorProto-type_name field))])
          (FieldDescriptorProto-name field))))))


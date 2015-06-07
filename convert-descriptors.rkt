#lang racket/base

(require
  racket/list
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
          'optional ;;TODO
          'string ;;TODO
          (FieldDescriptorProto-name field))))))


#lang racket/base

(require
  (for-syntax
    racket/base
    racket/list
    protobuf/code-generation
    protobuf/convert-descriptors
    protobuf/proto-descriptors))

(provide (all-defined-out))

(define-syntax (gen-requires stx)
  (call-with-input-file "{DESCRIPTOR}"
    (lambda (port)
      (define file-descriptor-set (parse-FileDescriptorSet port))
      (define requires
        (append*
          (for/list ([file (in-list (FileDescriptorSet-file file-descriptor-set))])
            (for/list ([dependency (in-list (FileDescriptorProto-dependency file))])
              (define lib-name
                (string-append
                  "protogen/"
                  (substring dependency 0 (- (string-length dependency) 6))
                  "-proto"))
              #`(require #,(string->symbol lib-name))))))
      #`(begin #,@requires))))
(gen-requires)

(define-syntax (go stx)
  (call-with-input-file "{DESCRIPTOR}"
    (lambda (port)
      (define file-descriptor-set (parse-FileDescriptorSet port))
      (generate-code stx
        (convert-descriptors file-descriptor-set)))))
(go)


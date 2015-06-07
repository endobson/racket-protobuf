#lang racket/base

(require
  "proto-reader.rkt"
  "define-proto.rkt"
  "proto-descriptors.rkt"
  "convert-descriptors.rkt"
  "code-generation.rkt"
  "import-proto.rkt"
  rackunit
  racket/runtime-path)

(provide tests)

(define-namespace-anchor anchor)

(define tests
  (test-suite "Protobuf Tests"
    (test-suite "Can import FileDescriptors"
      (test-begin
        (parameterize ([current-namespace (namespace-anchor->namespace anchor)])
          (check-not-exn (lambda ()
            (eval #'(let () (import-proto "tests/test-data/tmp.pb") (void)))))))
      (test-begin
        (parameterize ([current-namespace (namespace-anchor->namespace anchor)])
          (check-not-exn (lambda ()
            (eval #'(let () (import-proto "tests/test-data/descriptor.pb") (void))))))))

    (test-suite "Enums"
      (check-true (Label? 'LABEL_OPTIONAL))
      (check-false (Label? 'LABEL_))
      (check-equal? (Label->number 'LABEL_OPTIONAL) 1)
      (check-equal? (number->Label 2) 'LABEL_REQUIRED)
      (check-equal? Label-list '(LABEL_UNKNOWN LABEL_OPTIONAL LABEL_REQUIRED LABEL_REPEATED))

      (test-begin
        (define p (FieldDescriptorProto))
        (check-equal? (FieldDescriptorProto-label p) 'LABEL_UNKNOWN))

      (test-begin
        (define b (FieldDescriptorProto-builder))
        (set-FieldDescriptorProto-builder-label! b 'LABEL_OPTIONAL)
        (check-equal? (FieldDescriptorProto-builder-label b) 'LABEL_OPTIONAL)))


    (test-suite "Builders"
      (check-equal?
        (FileDescriptorSet-builder)
        (FileDescriptorSet-builder))

      (test-begin
        (define b (FileDescriptorSet-builder))
        (check-equal? (FileDescriptorSet-builder-file b) '()))

      (test-begin
        (define b (FileDescriptorProto-builder))
        (check-equal? (FileDescriptorProto-builder-name b) ""))

      (test-begin
        (define b (FieldDescriptorProto-builder))
        (check-equal? (FieldDescriptorProto-builder-number b) 0))

      (test-begin
        (define b (FileDescriptorProto-builder))
        (define p (FileDescriptorProto))
        (check-equal? (freeze-FileDescriptorProto b) p))


      (test-begin
        (define b (FileDescriptorProto-builder))
        (check-equal? (FileDescriptorProto-builder-dependency b) '())
        (FileDescriptorProto-builder-add-dependency! b "dep1")
        (check-equal? (FileDescriptorProto-builder-dependency b) '("dep1"))
        (FileDescriptorProto-builder-add-dependency! b "dep2")
        (check-equal? (FileDescriptorProto-builder-dependency b) '("dep1" "dep2"))))))

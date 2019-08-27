#lang racket/base

(require protobuf/code-generation)
(require protogen/tests/test-data/foo-proto)
(require protogen/tests/test-data/foo2-proto)
(require protogen/google/protobuf/any-proto)

(make-bar)
(make-foo2)
(make-any)

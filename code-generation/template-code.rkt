#lang racket/base

(provide
  protobuf-base
  struct:protobuf-base
  protobuf-base-byte-size)

(struct protobuf-base (byte-size) #:transparent)

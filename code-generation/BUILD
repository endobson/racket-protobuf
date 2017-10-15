load("@minimal_racket//:racket.bzl", "racket_binary", "racket_library", "racket_test")

package(
  default_visibility = ["//visibility:public"],
)

racket_library(
  name = "enum",
  srcs = ["enum.rkt"],
  deps = [
    "//:message-descriptor",
    "//:message-identifiers",
  ],
)

racket_library(
  name = "message-identifiers",
  srcs = ["message-identifiers.rkt"],
  deps = [
    "//:message-descriptor",
    "//:message-identifiers",
  ],
)

racket_library(
  name = "parser",
  srcs = ["parser.rkt"],
  deps = [
    "//:message-descriptor",
    "//:message-identifiers",
    "//:proto-reader",
  ],
)

racket_library(
    name = "serialization-test",
    srcs = ["serialization-test.rkt"],
    deps = [
      "//tests/test-data:foo2_proto_rkt"
    ],
)

racket_test(
    name = "serialization_test",
    main_module = "serialization-test.rkt",
    deps = [":serialization-test"],
)

racket_library(
  name = "serializer",
  srcs = ["serializer.rkt"],
  deps = [
    ":template-code",
    "//:message-descriptor",
    "//:message-identifiers",
    "//:varint",
  ],
)

racket_library(
  name = "structure",
  srcs = ["structure.rkt"],
  deps = [
    ":template-code",
    "//:message-descriptor",
    "//:message-identifiers",
    "//:varint",
  ],
)

racket_library(
    name = "template-code",
    srcs = ["template-code.rkt"],
)

load("@minimal_racket//:racket.bzl", "racket_binary", "racket_library")

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


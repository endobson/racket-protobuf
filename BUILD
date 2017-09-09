load("@minimal_racket//:racket.bzl", "racket_binary", "racket_library")

package(
  default_visibility = ["//visibility:public"],
)

racket_library(
  name = "code-generation",
  srcs = ["code-generation.rkt"],
  deps = [
    "//code-generation:enum",
    "//code-generation:message-identifiers",
    "//code-generation:parser",
    "//code-generation:structure",
  ]
)

racket_library(
  name = "convert-descriptors",
  srcs = ["convert-descriptors.rkt"],
  deps = [
    ":message-descriptor",
    ":proto-descriptors",
  ],
)

racket_library(
  name = "define-proto",
  srcs = ["define-proto.rkt"],
  deps = [
    ":code-generation",
    ":message-descriptor",
  ],
)

racket_library(
  name = "import-proto",
  srcs = ["import-proto.rkt"],
  deps = [
    ":proto-descriptors",
    ":convert-descriptors",
    ":parse-proto-file",
    ":code-generation",
  ],
)

racket_library(
  name = "info",
  srcs = ["info.rkt"],
)

racket_library(
  name = "message-descriptor",
  srcs = ["message-descriptor.rkt"],
)

racket_library(
  name = "message-identifiers",
  srcs = ["message-identifiers.rkt"],
)

racket_library(
  name = "parse-proto-file",
  srcs = ["parse-proto-file.rkt"],
  deps = [
    ":proto-descriptors",
  ]
)

racket_library(
  name = "proto-descriptors",
  srcs = ["proto-descriptors.rkt"],
  deps = [
    ":define-proto"
  ],
)

racket_library(
  name = "proto-reader",
  srcs = ["proto-reader.rkt"],
)

racket_library(
  name = "proto-serializer",
  srcs = ["proto-serializer.rkt"],
)

# racket_library(
#   name = "tests",
#   srcs = ["tests.rkt"],
#   deps = [
#     ":proto-reader",
#   ],
# )


racket_library(
  name = "test-gen",
  srcs = ["test-gen.rkt"],
  deps = [
    ":proto-descriptors",
    ":convert-descriptors",
    ":code-generation",
  ],
  compile_data = [
    "//tests/test-data:foo_proto"
  ]
)

racket_binary(
  name = "test-gen-bin",
  main_module = "test-gen.rkt",
  deps = [
    ":test-gen",
  ],
)

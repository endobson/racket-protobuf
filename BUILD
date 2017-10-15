load(
    "@minimal_racket//:racket.bzl",
    "racket_binary",
    "racket_library",
    "racket_collection",
)
load("//:racket_proto_library.bzl", "racket_proto_library")

package(
    default_visibility = ["//visibility:public"],
)

exports_files(["proto-template.rkt"])

racket_collection(
    name = "protobuf",
    deps = [
        ":code-generation",
        ":convert-descriptors",
        ":proto-descriptors",
    ],
)

racket_library(
  name = "test-collection",
  srcs = ["test-collection.rkt"],
  deps = [
    ":protobuf",
    "//tests/test-data:foo_proto_rkt",
    "//tests/test-data:foo2_proto_rkt",
    "//tests/test-data:multi_file_proto_rkt",
    "//tests/test-data:multi_file_b_proto_rkt",
    ":well_known_types_racket",
    ":build_event_service_racket",
  ],
)

racket_binary(
  name = "test-collection-bin",
  main_module = "test-collection.rkt",
  deps = [":test-collection"],
)

racket_library(
    name = "code-generation",
    srcs = ["code-generation.rkt"],
    deps = [
        "//code-generation:enum",
        "//code-generation:message-identifiers",
        "//code-generation:parser",
        "//code-generation:serializer",
        "//code-generation:structure",
    ],
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
    name = "proto-descriptors",
    srcs = ["proto-descriptors.rkt"],
    deps = [
        ":define-proto",
    ],
)

racket_library(
    name = "proto-reader",
    srcs = ["proto-reader.rkt"],
)

racket_library(
    name = "proto-racket-compiler",
    srcs = ["proto-racket-compiler.rkt"],
    deps = [
        ":protobuf"
    ],
)

racket_binary(
    name = "proto-racket-compiler_bin",
    main_module = "proto-racket-compiler.rkt",
    deps = ["proto-racket-compiler"],
)

racket_library(
    name = "varint",
    srcs = ["varint.rkt"],
)

# racket_library(
#   name = "tests",
#   srcs = ["tests.rkt"],
#   deps = [
#     ":proto-reader",
#   ],
# )

racket_proto_library(
  name = "build_event_service_racket",
  deps = ["@googleapis//:build_event_service"],
)

racket_proto_library(
  name = "well_known_types_racket",
  deps = ["@com_google_protobuf//:well_known_types_protos"]
)


load("//:racket_proto_library.bzl", "racket_proto_library")

package(
  default_visibility = ["//visibility:public"],
)

proto_library(
  name = "foo_proto",
  srcs = ["foo.proto"],
)

proto_library(
  name = "foo2_proto",
  srcs = ["foo2.proto"],
  deps = [":foo_proto"],
)

racket_proto_library(
  name = "foo_proto_rkt",
  deps = [":foo_proto"],
)

racket_proto_library(
  name = "foo2_proto_rkt",
  deps = [":foo2_proto"],
)


proto_library(
  name = "multi_file_proto",
  srcs = [
      "multi_file1.proto",
      "multi_file2.proto",
  ],
)

proto_library(
  name = "multi_file_b_proto",
  srcs = [
      "multi_file_b1.proto",
      "multi_file_b2.proto",
  ],
  deps = [
      ":multi_file_proto",
  ]
)

racket_proto_library(
  name = "multi_file_proto_rkt",
  deps = [":multi_file_proto"]
)

racket_proto_library(
  name = "multi_file_b_proto_rkt",
  deps = [":multi_file_b_proto"]
)

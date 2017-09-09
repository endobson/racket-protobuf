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

workspace(name = "racket_protobuf")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
  name = "minimal_racket",
  sha256 = "bab69cf3a6266edf7378ecc3776800e74ce043dc960c23093efc4ffb244d9b7a",
  strip_prefix = "minimal-racket-ba6f0b15396476746f4d98075d1a43e9ace1108b",
  urls = ["https://github.com/endobson/minimal-racket/archive/ba6f0b15396476746f4d98075d1a43e9ace1108b.tar.gz"]
)
load("@minimal_racket//:releases.bzl", "racket_releases")
racket_releases()

register_toolchains(
    '@minimal_racket//:osx_osx_racket_toolchain',
    '@minimal_racket//:linux_linux_racket_toolchain',
    '@minimal_racket//:osx_linux_racket_toolchain',
    '@minimal_racket//:linux_osx_racket_toolchain',
)

http_archive(
    name = "com_google_protobuf",
    sha256 = "416212e14481cff8fd4849b1c1c1200a7f34808a54377e22d7447efdf54ad758",
    strip_prefix = "protobuf-09745575a923640154bcf307fba8aedff47f240a",
    url = "https://github.com/google/protobuf/archive/09745575a923640154bcf307fba8aedff47f240a.tar.gz",
)
load("@com_google_protobuf//:protobuf_deps.bzl", "protobuf_deps")
protobuf_deps()

http_archive(
    name = "bazel_skylib",
    sha256 = "bbccf674aa441c266df9894182d80de104cabd19be98be002f6d478aaa31574d",
    strip_prefix = "bazel-skylib-2169ae1c374aab4a09aa90e65efe1a3aad4e279b",
    urls = ["https://github.com/bazelbuild/bazel-skylib/archive/2169ae1c374aab4a09aa90e65efe1a3aad4e279b.tar.gz"],
)

workspace(name = "racket_protobuf")

http_archive(
  name = "minimal_racket",
  sha256 = "4d431edf7784e4c869cb03f1dc371c9cc4b714dcfc23734b23dee0d6c21b7a51",
  strip_prefix = "minimal-racket-b05cd33865d17977ac75f3cc635f6a0b075ff054",
  urls = ["https://github.com/endobson/minimal-racket/archive/b05cd33865d17977ac75f3cc635f6a0b075ff054.tar.gz"]
)

load("@minimal_racket//:releases.bzl", "racket_releases")
racket_releases()

http_archive(
  name = "com_google_protobuf",
  sha256 = "8b3a82704fbf5202c3bcfbbe6b2eb4d07d85bcb507876aaf60edff751c821854",
  strip_prefix = "protobuf-hack-wkt",
  urls = ["https://github.com/endobson/protobuf/archive/hack-wkt.tar.gz"]
)

new_http_archive(
  name = "googleapis",
  sha256 = "0421e89b76a6fa6f820c39ad365a5e490873ae4c7509c8a53f42671f1e53e1e8",
  urls = ["https://github.com/googleapis/googleapis/archive/220c359ac969c6bbab7a11077b32de2533cc7bad.tar.gz"],
  strip_prefix = "googleapis-220c359ac969c6bbab7a11077b32de2533cc7bad",
  build_file = "BUILD.googleapis",
)

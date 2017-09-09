workspace(name = "racket_protobuf")

http_archive(
  name = "minimal_racket",
  sha256 = "9cdf52e420e7a3e1f14033c9db427b6f28f95529f88bb48f1f9422bdfaea3013",
  strip_prefix = "minimal-racket-08cd37aa20aec7065a579130f612fa93af764d81",
  urls = ["https://github.com/endobson/minimal-racket/archive/08cd37aa20aec7065a579130f612fa93af764d81.tar.gz"]
)

load("@minimal_racket//:releases.bzl", "racket_releases")
racket_releases()

http_archive(
  name = "com_google_protobuf",
  sha256 = "8b3a82704fbf5202c3bcfbbe6b2eb4d07d85bcb507876aaf60edff751c821854",
  strip_prefix = "protobuf-hack-wkt",
  urls = ["https://github.com/endobson/protobuf/archive/hack-wkt.tar.gz"]
)

workspace(name = "racket_protobuf")

http_archive(
  name = "minimal_racket",
  sha256 = "1250e8a6b3209663070dc1679ef445dfe0489037537faa1420b3a520899ae295",
  strip_prefix = "minimal-racket-8c96c49702a170a104748b4a48702000ba141eb1",
  urls = ["https://github.com/endobson/minimal-racket/archive/8c96c49702a170a104748b4a48702000ba141eb1.tar.gz"]
)

load("@minimal_racket//:releases.bzl", "racket_releases")
racket_releases()

http_archive(
  name = "com_google_protobuf",
  sha256 = "8b3a82704fbf5202c3bcfbbe6b2eb4d07d85bcb507876aaf60edff751c821854",
  strip_prefix = "protobuf-hack-wkt",
  urls = ["https://github.com/endobson/protobuf/archive/hack-wkt.tar.gz"]
)

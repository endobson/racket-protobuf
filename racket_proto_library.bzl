load("@minimal_racket//:racket.bzl", "racket_compile", "RacketInfo")

def _racket_proto_library_aspect_impl(target, ctx):
  dep_zos = (ctx.attr._proto_collection[RacketInfo].transitive_zos)
  dep_links = (ctx.attr._proto_collection[RacketInfo].transitive_links)
  for dep in ctx.rule.attr.deps:
    dep_zos += dep[RacketInfo].transitive_zos
    dep_links += dep[RacketInfo].transitive_links

  zo_files = []
  link_files = []
  for proto_file in target.proto.direct_sources:
    basename = proto_file.basename[:-len(".proto")]
    rkt_file = ctx.actions.declare_file(
      basename + "-proto.rkt",
      sibling = proto_file
    )
    zo_file = ctx.actions.declare_file(
      "racket_protogen/" + proto_file.dirname + "/compiled/" + basename + "-proto_rkt.zo",
      sibling = proto_file
    )
    ctx.actions.expand_template(
      template = ctx.file._template,
      output = rkt_file,
      substitutions = {
        "{DESCRIPTOR}": target.proto.direct_descriptor_set.path
      }
    )
    link_file = ctx.actions.declare_file(
      basename + "-proto_links.rktd",
      sibling = proto_file
    )
    ctx.actions.write(
      output = link_file,
      content = "((\"protogen\" \"racket_protogen\"))",
    )


    inputs = (depset([rkt_file, target.proto.direct_descriptor_set]) + ctx.attr._lib_deps.files +
              dep_zos + dep_links)
    racket_compile(
      ctx,
      src_file = rkt_file,
      output_file = zo_file,
      link_files = dep_links,
      inputs = inputs)

    zo_files.append(zo_file)
    link_files.append(link_file)


  return [
    RacketInfo(
      transitive_zos = dep_zos + zo_files,
      transitive_links = dep_links + link_files,
    )
  ]

racket_proto_library_aspect = aspect(
  implementation = _racket_proto_library_aspect_impl,
  attrs = {
    "_template": attr.label(
      default="//:proto-template.rkt",
      allow_files=True,
      single_file=True,
    ),
    "_racket_bin": attr.label(
      default="@minimal_racket//osx/v6.10:bin/racket",
      allow_files=True,
      executable=True,
      cfg="host",
    ),
    "_lib_deps": attr.label(
      default="@minimal_racket//osx/v6.10:racket-src-osx",
    ),
    "_proto_collection": attr.label(
      default="//:protobuf",
      providers = [RacketInfo],
    ),
  },
  attr_aspects = ["deps"]
)

def _racket_proto_library_impl(ctx):
  zos = depset()
  links = depset()
  for dep in ctx.attr.deps:
    zos += dep[RacketInfo].transitive_zos
    links += dep[RacketInfo].transitive_links

  return [
    RacketInfo(
      transitive_zos = zos,
      transitive_links = links,
    )
  ]

racket_proto_library = rule(
  implementation = _racket_proto_library_impl,
  attrs = {
    "deps": attr.label_list(
      providers = [RacketInfo],
      aspects = [racket_proto_library_aspect]
    ),
  }
)


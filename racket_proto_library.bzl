load("@minimal_racket//:racket.bzl", "racket_compile", "RacketInfo")

def _racket_proto_library_aspect_impl(target, ctx):
  rkt_files = []
  zo_files = []
  for proto_file in target.proto.direct_sources:
    rkt_file = ctx.actions.declare_file(
      proto_file.basename + ".rkt",
      sibling = proto_file
    )
    zo_file = ctx.actions.declare_file(
      "compiled/" + proto_file.basename + "_rkt.zo",
      sibling = proto_file
    )
    ctx.actions.expand_template(
      template = ctx.file._template,
      output = rkt_file,
      substitutions = {
        "{DESCRIPTOR}": target.proto.direct_descriptor_set.path
      }
    )

    dep_zos = (ctx.attr._code_generation[RacketInfo].transitive_zos +
       ctx.attr._convert_descriptors[RacketInfo].transitive_zos +
       ctx.attr._proto_descriptors[RacketInfo].transitive_zos)
    link_files = (ctx.attr._code_generation[RacketInfo].transitive_links +
       ctx.attr._convert_descriptors[RacketInfo].transitive_links +
       ctx.attr._proto_descriptors[RacketInfo].transitive_links)

    inputs = depset([rkt_file, target.proto.direct_descriptor_set]) + ctx.attr._lib_deps.files + dep_zos
    racket_compile(
      ctx,
      src_file = rkt_file,
      output_file = zo_file,
      link_files = link_files,
      inputs = inputs)
    rkt_files.append(rkt_file)
    zo_files.append(zo_file)

  return [
    OutputGroupInfo(
      racket_proto = rkt_files,
      racket_proto_zo = zo_files
    )
  ]

racket_proto_library_aspect = aspect(
  implementation = _racket_proto_library_aspect_impl,
  attrs = {
    "_template": attr.label(
      default="//:test-gen.rkt",
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
    "_code_generation": attr.label(
      default="//:code-generation",
      providers = [RacketInfo],
    ),
    "_convert_descriptors": attr.label(
      default="//:convert-descriptors",
      providers = [RacketInfo],
    ),
    "_proto_descriptors": attr.label(
      default="//:proto-descriptors",
      providers = [RacketInfo],
    ),
  }
)

def _racket_proto_library_impl(ctx):
  ctx.actions.run_shell(
     outputs = [ctx.outputs.rkt],
     command = "touch " + ctx.outputs.rkt.path
  )

  rkt_files = depset()
  zo_files = depset()
  for dep in ctx.attr.deps:
    rkt_files += dep[OutputGroupInfo].racket_proto
    zo_files += dep[OutputGroupInfo].racket_proto_zo
  

  return [
    OutputGroupInfo(
      racket_proto = rkt_files,
      racket_proto_zo = zo_files,
    )
  ]

racket_proto_library = rule(
  implementation = _racket_proto_library_impl,
  outputs = {
    "rkt": "%{name}.rkt"
  },
  attrs = {
    "deps": attr.label_list(
      providers = ["proto"],
      aspects = [racket_proto_library_aspect]
    ),
  }
)


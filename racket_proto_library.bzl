load("@minimal_racket//:racket.bzl", "racket_compile", "RacketInfo")

def _racket_proto_library_aspect_impl(target, ctx):
  dep_zos = ctx.attr._proto_collection[RacketInfo].transitive_zos
  dep_links = ctx.attr._proto_collection[RacketInfo].transitive_links
  for dep in ctx.rule.attr.deps:
    dep_zos += dep[RacketInfo].transitive_zos
    dep_links += dep[RacketInfo].transitive_links

  rkt_files = []
  zo_files = []

  link_file = ctx.actions.declare_file(
    ctx.label.name + "_links.rktd",
  )
  ctx.actions.write(
    output = link_file,
    content = "((\"protogen\" \"racket_protogen\"))",
  )


  for proto_file in target.proto.direct_sources:
    basename = proto_file.basename[:-len(".proto")]
    rkt_file = ctx.actions.declare_file(
      basename + "-proto.rkt",
      sibling = proto_file
    )
    if (proto_file.short_path.startswith("../")):
      proto_file_local_path = proto_file.short_path[proto_file.short_path.index("/",3)+1:]
    else:
      proto_file_local_path = proto_file.short_path
    proto_file_fixed_dirname = proto_file_local_path[0:proto_file_local_path.rindex("/")]

    zo_file = ctx.actions.declare_file(
      "racket_protogen/" + proto_file_fixed_dirname + "/compiled/" + basename + "-proto_rkt.zo",
    )

    ctx.actions.expand_template(
      template = ctx.file._template,
      output = rkt_file,
      substitutions = {
        "{DESCRIPTOR}": target.proto.direct_descriptor_set.path,
        "{SOURCE_PATH}": proto_file_local_path
      }
    )

    rkt_files.append(rkt_file)
    zo_files.append(zo_file)
 
  transitive_links = dep_links + [link_file]

  arguments = []
  arguments += ["--links", 
                "(" + " ".join(['"%s"' % link_file.path for link_file
                                in transitive_links.to_list()]) + ")"]
  arguments += ["--file_descriptor", target.proto.direct_descriptor_set.path]
  arguments += ["--bin_dir", ctx.bin_dir.path + "/" + ctx.label.workspace_root]
  arguments += ["--output_dir",
                ctx.bin_dir.path + "/" +
                ctx.build_file_path[:ctx.build_file_path.rindex("/")] + "/racket_protogen"]


  
  ctx.actions.run(
    executable = ctx.executable._proto_racket_compiler,
    arguments = arguments,
    inputs = depset([target.proto.direct_descriptor_set] + rkt_files) + transitive_links +
             dep_zos,
    outputs = zo_files,
  )

  return [
    RacketInfo(
      transitive_zos = dep_zos + zo_files,
      transitive_links = transitive_links,
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
    "_core_racket": attr.label(
      default="@minimal_racket//osx/v6.10:racket-src-osx",
    ),
    "_proto_collection": attr.label(
      default="//:protobuf",
      providers = [RacketInfo],
    ),
    "_bazel_tools": attr.label(
      default=Label("@minimal_racket//build_rules:bazel-tools"),
      cfg="host",
    ),
    "_proto_racket_compiler": attr.label(
      default=Label("//:proto-racket-compiler_bin"),
      executable=True,
      cfg="host",
    )

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


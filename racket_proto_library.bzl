def _racket_proto_library_aspect_impl(target, ctx):
  rkt_files = []
  for proto_file in target.proto.direct_sources:
    rkt_file = ctx.actions.declare_file(
      proto_file.basename + ".rkt",
      sibling = proto_file
    )
    ctx.actions.write(
      output = rkt_file,
      content = "#lang racket/base\n"
    )
    rkt_files.append(rkt_file)

  return []

racket_proto_library_aspect = aspect(
  implementation = _racket_proto_library_aspect_impl,
)

def _racket_proto_library_impl(ctx):
  ctx.actions.run_shell(
     outputs = [ctx.outputs.rkt],
     command = "touch " + ctx.outputs.rkt.path
  )

racket_proto_library = rule(
  implementation = _racket_proto_library_impl,
  outputs = {
    "rkt": "%{name}.rkt"
  },
  attrs = {
    "deps": attr.label_list(
      providers = ["proto"],
      aspects = [racket_proto_library_aspect]
    )
  }
)


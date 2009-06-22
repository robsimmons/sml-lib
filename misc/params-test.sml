structure ParamsTest =
struct

  val () = print "\n\n"

  val fu = Params.use_param "f"

  val f = Params.param "default" (SOME ("-p", "desc")) "f"

  val _ = print (Params.usage ())

  val () = print "Arguments: "
  val () = app (fn p => print (p ^ " ")) (CommandLine.arguments())

  val _ = (ignore (Params.docommandline ()))
    handle Params.Params s => print (s ^ "\n")

end

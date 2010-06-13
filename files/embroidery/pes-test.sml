
structure PesTest =
struct

  val r = Reader.fromfile "test.pes"
  val f : PES.pesfile = PES.readpes r
      handle (e as PES.PES s) => (print ("PES failed: " ^ s ^ "\n");
                                  raise e)

  val () = print "PES ok\n"

end

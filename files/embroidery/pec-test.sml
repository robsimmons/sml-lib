
structure PecTest =
struct

  val r = Reader.fromfile "test.pec"
  val p : PEC.pecfile = PEC.readpec r
      handle (e as PEC.PEC s) => (print ("PEC failed: " ^ 
                                         s ^ "\n"); raise e)

  val () = print "PEC ok\n"

end

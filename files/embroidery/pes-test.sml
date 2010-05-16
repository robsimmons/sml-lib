
(* nothing yet *)
structure PesTest =
struct

  val r = Reader.fromfile "test.pes"
  val p : int = PES.readpes r
      handle (e as PES.PES s) => (print (s ^ "\n"); raise e)

end

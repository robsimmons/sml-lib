(* XXX: Lots more tests! *)
structure GrowArrayTest =
struct

  structure GA = GrowArray
  exception Nope

  val () =
  let
      val _ = GA.length (GA.empty()) = 0 orelse raise Nope
      val _ = (GA.sub (GA.empty()) 0; raise Nope) handle Subscript => ()
          
      val a : int GA.growarray = GA.empty ()
      val _ = 0 = GA.update_next a 5 orelse raise Nope
      val _ = 1 = GA.length a orelse raise Nope
      val _ = GA.update a 10 3
      val _ = 11 = GA.length a orelse raise Nope
      val _ = GA.exists (fn 5 => true | _ => false) a orelse raise Nope
      val _ = GA.erase a 10
      val _ = 1 = GA.length a orelse raise Nope
      val _ = GA.erase a 0
      val _ = 0 = GA.length a orelse raise Nope

  in
      ()
  end handle e => (print (exnName e ^ ": " ^ exnMessage e ^ "\n"); raise e)




  val () = print "GrowArray tests passed!\n"
end

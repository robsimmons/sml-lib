structure TestMT =
struct

    structure MT = MersenneTwister
    val init = Vector.fromList [ 0wx123 : Word32.word, 0wx234, 0wx345, 0wx456 ]

    fun show (mt, ref i) =
        let in
            print ("@@ " ^ Int.toString i ^ " @@\n");
            Util.for 0 (MT.N - 1)
            (fn i =>
             print (Word32.toString (Array.sub(mt, i)) ^ "\n"));
            print "\n"
        end
(*
    val mt = MT.init32 0w0
    val () = show mt

    val () = print "----\n"
    val () = print (Word32.toString (MT.rand32 mt) ^ "\n")
    val () = print ".....\n"
    val () = show mt
*)
    (* same test from mt.c *)
    val mt = MT.init init
    val () = show mt
    val () = Util.for 0 999 
        (fn i =>
         let in
             print (Word32.toString (MT.rand32 mt));
             (if i mod 5 = 4 then print "\n" else print " ")
         end)
end

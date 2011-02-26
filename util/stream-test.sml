
structure StreamTest =
struct

    structure S = Stream
    exception Nope

    fun headn 0 _ = nil
      | headn n s =
      (case Stream.force s of
         Stream.Nil => nil
       | Stream.Cons (a, t) => a :: headn (n - 1) t)

    val () =
    let
	val s1 = S.countup 1 NONE
	val s2 = S.map (fn x => x * 2) (S.countup 1 NONE)
	val s3 = S.singleton ~1

	val smerge = S.merge_sorted Int.compare [s1, s2, s3]

	val hn = headn 11 smerge 
	val _ = hn = [~1, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7]
	    orelse (app (fn x => print (Int.toString x ^ " ")) hn;
		    print "\n";
		    raise Nope);
    in
        ()
    end handle e => (print (exnName e ^ ": " ^ exnMessage e ^ "\n"); raise e)

    val () = print "Stream tests passed!\n"

end


structure RETest =
struct

    open RE
    exception Nope

    val () =
    let
        val _ = hasmatch "hello" "hello" orelse raise Nope
        val _ = hasmatch "hello" "oh hello there" orelse raise Nope
        val _ = hasmatch "no" "yes" andalso raise Nope

        (* Check that find is staged, by seeing that it rejects an
           ill-formed regular expression eagerly. *)
        val () = (find "("; raise Nope) handle RE _ => ()

        val _ = case find " ([0-9]+) " "now there are 60 wugs" of
            SOME f => (f 1 = "60" orelse raise Nope;
                       f 0 = " 60 " orelse raise Nope)
          | NONE => raise Nope


        val _ = case findall " ([0-9]+) " "and 6 times nine is 42 ?" of
            [first, second] =>
                (first 1 = "6" orelse raise Nope;
                 first 0 = " 6 " orelse raise Nope;
                 second 1 = "42" orelse raise Nope;
                 second 0 = " 42 " orelse raise Nope)
          | _ => raise Nope
    in
        ()
    end handle e => (print (exnName e ^ ": " ^ exnMessage e ^ "\n"); raise e)

    val () = print "RE tests passed!\n"

end

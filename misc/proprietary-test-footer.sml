
val s1 = "Hello, world."
val s2 = "Hfllo, world."
val s3 = "Hello, wprld."

fun test s =
    let
        val (a, b, c, d) = proprietary s
    in
        print (Word32.toString a ^ " " ^
               Word32.toString b ^ " " ^
               Word32.toString c ^ " " ^
               Word32.toString d ^ " <= " ^ s ^ "\n")
    end

val () = app test [s1, s2, s3]


val s1 = "Hello, world."
val s2 = "Hfllo, world."
val s3 = "Hello, wprld."

fun word32_tostring w =
    let
        val s = Word32.toString w
        fun pad n s = if size s < n
                      then pad n ("0" ^ s)
                      else s
    in
        pad 8 s
    end

fun test s =
    let
        val (a, b, c, d) = proprietary s
    in
        print (word32_tostring a ^ " " ^
               word32_tostring b ^ " " ^
               word32_tostring c ^ " " ^
               word32_tostring d ^ " <= " ^ s ^ "\n")
    end

val () = app test [s1, s2, s3]

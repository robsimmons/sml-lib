
structure SVGTest =
struct

  exception Nope

  open SVG

  val () =
  let 
      val () = case parsepathstring "" of
          NONE => raise Nope
        | SOME nil => ()
        | SOME _ => raise Nope

      val () = case parsepointsstring "  123.4,56.7 8,9 \n 10 11\r" of
          SOME [a, b, c] => ()
        | _ => raise Nope

  in
      ()
  end handle e => (print (exnName e ^ ": " ^ exnMessage e ^ "\n"); raise e)

  val () = print "SVG tests passed!\n"

end

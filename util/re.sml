
structure RE : RE =
struct

  exception RE of string

  structure R = RegExpFn(structure P = AwkSyntax
                         structure E = BackTrackEngine);

  type substring = Substring.substring

  fun compile x =
      R.compileString x
          handle RegExpSyntax.CannotParse => raise RE "ill-formed regular expression"
               | RegExpSyntax.CannotCompile => raise RE "couldn't compile RE"
               | _ => raise RE "(unknown other error compiling regexp)"

  fun find x =
    let
      val re = compile x
      val re_find = R.find re Substring.getc
      fun f s =
        case re_find (Substring.full s) of
          NONE => NONE
        | SOME (tree, _) =>
            let
              fun g i =
                  let val { pos, len } = MatchTree.nth (tree, i)
                  in Substring.string (Substring.slice (pos, 0, SOME len))
                  end handle Subscript => raise RE ("match " ^ Int.toString i ^ 
                                                    "  out of bounds")
            in
              SOME g
            end
    in
      f
    end

  fun findall x =
    let
      val re = compile x
      val re_find : substring ->
          ({ len: int, 
             pos: substring } MatchTree.match_tree * 
           substring) option = R.find re Substring.getc

      fun fall (s : substring) =
        case re_find s of
          NONE => nil
        | SOME (tree, rest : substring) =>
            let
              fun g i =
                  let val { pos : substring, len } = MatchTree.nth (tree, i)
                  in Substring.string (Substring.slice (pos, 0, SOME len))
                  end handle Subscript => raise RE ("match " ^ Int.toString i ^ 
                                                    "  out of bounds")
            in
                g :: fall rest
            end
    in
        fall o Substring.full
    end

  (* PERF: Use FSM matcher for hasmatch and ismatch; they don't collect
     submatch information and so they are much faster. *)
  fun hasmatch re = Option.isSome o find re

  (* TODO: ismatch *)

end
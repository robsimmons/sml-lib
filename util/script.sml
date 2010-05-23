structure Script :> SCRIPT =
struct

  exception Script of string

  fun linesfromfile f =
      let val s = StringUtil.readfile f
          val l = String.tokens (fn #"\n" => true
                                  | #"\r" => true
                                  | _ => false) s
      in
          List.mapPartial (fn s =>
                           case StringUtil.losespecsides StringUtil.whitespec s of
                               "" => NONE
                             | w => SOME w) l
      end

  structure SM = SplayMapFn(type ord_key = string val compare = String.compare)

  fun alistfromfile f =
    let
      val lines = linesfromfile f
      val m = ref SM.empty

      fun addone line =
        let
          val (k, v) = StringUtil.token (fn #" " => true | #"\t" => true | _ => false) line
        in
          case SM.find(!m, k) of
            SOME _ => raise Script ("duplicate key " ^ k ^ " in alist file " ^ f)
          | NONE => m := SM.insert(!m, k, v)
        end

      val () = app addone lines
      val m = !m
      fun lookup k = SM.find(m, k)
    in
      { alist = SM.foldri (fn (k, v, l) => (k, v) :: l) nil m,
        lookup = lookup }
    end

end
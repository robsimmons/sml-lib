
structure StreamUtil :> STREAMUTIL =
struct

    (* converts a string to a char stream *)
    fun stostream s =
      let
        val ss = size s
        fun next n () = 
          if n >= ss
          then Stream.empty
          else Stream.lcons (CharVector.sub(s, n),
                             next (n + 1))
      in
        Stream.old_delay (next 0)
      end


    (* convert a file to a char stream *)
    fun ftostream f =
        let
            val ff = BinIO.openIn f

            fun rd () =
                case BinIO.input1 ff of
                    NONE => (BinIO.closeIn ff; 
                             Stream.empty)
                  | SOME c => Stream.lcons(chr (Word8.toInt c), rd)
        in
            Stream.old_delay rd
        end

    fun ltostream l =
        foldr Stream.cons Stream.empty l

    fun concat l r =
      Stream.delay
      (fn () =>
       case Stream.force l of
         Stream.Nil => Stream.force r
       | Stream.Cons (h, t) => Stream.Cons(h, concat t r))

    fun flatten s =
      Stream.delay
      (fn () =>
       (case Stream.force s of
          Stream.Nil => Stream.Nil
        | Stream.Cons (a, t) => Stream.force (concat a (flatten t))))

    fun headn 0 _ = nil
      | headn n s =
      (case Stream.force s of
         Stream.Nil => nil
       | Stream.Cons (a, t) => a :: headn (n - 1) t)

end

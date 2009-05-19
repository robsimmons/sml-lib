
structure StreamUtil :> STREAMUTIL =
struct

    (* converts a string to a char stream *)
    fun stostream s =
      let
        val ss = size s

        fun next n () = 
          if n >= ss
          then Stream.Nil
          else Stream.Cons(CharVector.sub(s, n), 
                           Stream.delay (next (n + 1)))
      in
        Stream.delay (next 0)
      end


    (* convert a file to a char stream *)
    fun ftostream f =
        let
            val ff = BinIO.openIn f

            fun step () =
                case BinIO.input1 ff of
                    NONE => (BinIO.closeIn ff; Stream.Nil)
                  | SOME b => Stream.Cons(Byte.byteToChar b, 
                                          Stream.delay step)
        in
            Stream.delay step
        end

    (* convert a file to a byte stream *)
    fun ftobytestream f = 
        let 
          val ff = BinIO.openIn f

          fun step () = 
              case BinIO.input1 ff of
                  NONE => (BinIO.closeIn ff; Stream.Nil)
                | SOME b => Stream.Cons(b, Stream.delay step)
        in 
          Stream.delay step
        end

    fun ftoUTF8stream f = 
        let 
          val ff = BinIO.openIn f

          exception UTF8
          fun getn(s,0) = s
            | getn(s,i) = 
              case BinIO.input1 ff of
                NONE => raise UTF8
              | SOME b => 
                if Word8.>>(b,0wx6) = 0wx2
                then getn(s ^ str(Byte.byteToChar b), i-1)
                else raise UTF8

          fun step () = 
              case BinIO.input1 ff of
                  NONE => (BinIO.closeIn ff; Stream.Nil)
                | SOME b => 
                  let val s = 
                          if Word8.>>(b,0wx7) = 0wx0 
                          then str(Byte.byteToChar b)
                          else if Word8.>>(b,0wx5) = 0wx6
                          then getn(str(Byte.byteToChar b),1)
                          else if Word8.>>(b,0wx4) = 0wxE
                          then getn(str(Byte.byteToChar b),2)
                          else if Word8.>>(b,0wx3) = 0wx1E
                          then getn(str(Byte.byteToChar b),3)
                          else if Word8.>>(b,0wx2) = 0wx3E
                          then getn(str(Byte.byteToChar b),4)
                          else if Word8.>>(b,0wx1) = 0wx7E
                          then getn(str(Byte.byteToChar b),5)
                          else raise UTF8
                  in Stream.Cons(s, Stream.delay step) end
                  handle UTF8 => Stream.Nil
        in 
          Stream.delay step
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

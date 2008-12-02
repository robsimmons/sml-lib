(* Encodes UTF8. 

   I pulled this out so it can be used in the simple XML interface
   without forcing the output to go to a file.   - Tom 7
*)
structure EncodeUTF8 :
sig
    (* writeCharUtf8 f (c, s)
       The function f may be called many times to write bytes to the output. *)
    val writeCharUtf8 : ('stream * Word8.word -> 'stream) -> (UniChar.Char * 'stream) -> 'stream
end =
struct

    open UniChar EncodeBasic EncodeError

   infix 8 >>
   infix 7 &&
   infix 6 ||

   val op && = Chars.andb
   val op >> = Chars.>>
   val op || = Word8.orb

   fun Char2Byte c = Word8.fromLargeWord(Chars.toLargeWord c)

   (*---------------------------------------------------------------------*)
   (* UTF-8                                                               *)
   (*---------------------------------------------------------------------*)
   fun writeCharUtf8 writeByte (c,f) = 
      if c<0wx80 then writeByte(f,Char2Byte c)
      else if c<0wx800 
              then let val f1 = writeByte(f,0wxC0 || Char2Byte(c >> 0w6))
                       val f2 = writeByte(f1,0wx80 || Char2Byte(c && 0wx3F))
                   in f2 
                   end
      else if c<0wx10000
              then let val f1 = writeByte(f, 0wxE0 || Char2Byte(c >> 0w12))
                       val f2 = writeByte(f1,0wx80 || Char2Byte((c >> 0w6) && 0wx3F))
                       val f3 = writeByte(f2,0wx80 || Char2Byte(c && 0wx3F))
                   in f3
                   end
      else if c<0wx200000
              then let val f1 = writeByte(f, 0wxF0 || Char2Byte(c >> 0w18))
                       val f2 = writeByte(f1,0wx80 || Char2Byte((c >> 0w12) && 0wx3F))
                       val f3 = writeByte(f2,0wx80 || Char2Byte((c >> 0w6) && 0wx3F))
                       val f4 = writeByte(f3,0wx80 || Char2Byte(c && 0wx3F))
                   in f4
                   end
      else if c<0wx4000000
              then let val f1 = writeByte(f, 0wxF8 || Char2Byte(c >> 0w24))
                       val f2 = writeByte(f1,0wx80 || Char2Byte((c >> 0w18) && 0wx3F))
                       val f3 = writeByte(f2,0wx80 || Char2Byte((c >> 0w12) && 0wx3F))
                       val f4 = writeByte(f3,0wx80 || Char2Byte((c >> 0w6) && 0wx3F))
                       val f5 = writeByte(f4,0wx80 || Char2Byte(c && 0wx3F))
                   in f5
                   end
           else let val f1 = writeByte(f, 0wxFC || Char2Byte(c >> 0w30))
                    val f2 = writeByte(f1,0wx80 || Char2Byte((c >> 0w24) && 0wx3F))
                    val f3 = writeByte(f2,0wx80 || Char2Byte((c >> 0w18) && 0wx3F))
                    val f4 = writeByte(f3,0wx80 || Char2Byte((c >> 0w12) && 0wx3F))
                    val f5 = writeByte(f4,0wx80 || Char2Byte((c >> 0w6) && 0wx3F))
                    val f6 = writeByte(f5,0wx80 || Char2Byte(c && 0wx3F))
                in f6
                end

end

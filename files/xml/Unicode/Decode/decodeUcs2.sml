






signature DecodeUcs2 =
   sig
      val getCharUcs2b : DecodeFile.File -> UniChar.Char * DecodeFile.File
      val getCharUcs2l : DecodeFile.File -> UniChar.Char * DecodeFile.File
   end

structure DecodeUcs2 : DecodeUcs2 = 
   struct
      open 
	 UniChar Encoding
	 DecodeFile DecodeError DecodeUtil

      fun getCharUcs2b f =
         let 
	    val (b1,f1) = getByte f
	    val (b2,f2) = getByte f1 handle exn as EndOfFile f 
	       => raise DecodeError(f,true,ERR_EOF_UCS2 b1)
	    val c = Chars.orb(Chars.<<(Byte2Char b1,0w8),Byte2Char b2) 
	 in (c,f2)
         end

      fun getCharUcs2l f =
         let 
	    val (b1,f1) = getByte f
	    val (b2,f2) = getByte f1 handle exn as EndOfFile f 
	       => raise DecodeError(f,true,ERR_EOF_UCS2 b1)
	    val c = Chars.orb(Chars.<<(Byte2Char b2,0w8),Byte2Char b1) 
         in (c,f2)
         end
   end

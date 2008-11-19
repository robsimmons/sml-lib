







signature DecodeUcs4 =
   sig
      val getCharUcs4b  : DecodeFile.File -> UniChar.Char * DecodeFile.File
      val getCharUcs4l  : DecodeFile.File -> UniChar.Char * DecodeFile.File
      val getCharUcs4sb : DecodeFile.File -> UniChar.Char * DecodeFile.File
      val getCharUcs4sl : DecodeFile.File -> UniChar.Char * DecodeFile.File
   end

structure DecodeUcs4 : DecodeUcs4 = 
   struct
      open 
         UniChar UniClasses 
         DecodeFile DecodeError DecodeUtil

      fun getCharUcs4b f = 
         let 
            val (b1,f1) = getByte f
            val (b2,f2) = getByte f1 handle EndOfFile f 
	       => raise DecodeError(f,true,ERR_EOF_UCS4(1,[b1]))
            val (b3,f3) = getByte f2 handle EndOfFile f 
	       => raise DecodeError(f,true,ERR_EOF_UCS4(1,[b1,b2]))
	    val (b4,f4) = getByte f3 handle EndOfFile f 
	       => raise DecodeError(f,true,ERR_EOF_UCS4(1,[b1,b2,b3]))
	    val c = Chars.orb(Chars.orb(Chars.<<(Byte2Char b1,0w24),
					Chars.<<(Byte2Char b2,0w16)),
			      Chars.orb(Chars.<<(Byte2Char b3,0w08),
					Byte2Char b4))
         in if isUnicode c then (c,f4)
	    else raise DecodeError(f4,false,ERR_NON_UNI_UCS4 c)
         end

      fun getCharUcs4l f = 
         let 
            val (b1,f1) = getByte f
            val (b2,f2) = getByte f1 handle EndOfFile f 
	       => raise DecodeError(f,true,ERR_EOF_UCS4(1,[b1]))
            val (b3,f3) = getByte f2 handle EndOfFile f 
	       => raise DecodeError(f,true,ERR_EOF_UCS4(1,[b1,b2]))
	    val (b4,f4) = getByte f3 handle EndOfFile f 
	       => raise DecodeError(f,true,ERR_EOF_UCS4(1,[b1,b2,b3]))
	    val c = Chars.orb(Chars.orb(Chars.<<(Byte2Char b4,0w24),
					Chars.<<(Byte2Char b3,0w16)),
			      Chars.orb(Chars.<<(Byte2Char b2,0w08),
					Byte2Char b1))
         in if isUnicode c then (c,f4)
	    else raise DecodeError(f4,false,ERR_NON_UNI_UCS4 c)
         end

      fun getCharUcs4sb f = 
         let 
            val (b1,f1) = getByte f
            val (b2,f2) = getByte f1 handle EndOfFile f 
	       => raise DecodeError(f,true,ERR_EOF_UCS4(1,[b1]))
            val (b3,f3) = getByte f2 handle EndOfFile f 
	       => raise DecodeError(f,true,ERR_EOF_UCS4(1,[b1,b2]))
	    val (b4,f4) = getByte f3 handle EndOfFile f 
	       => raise DecodeError(f,true,ERR_EOF_UCS4(1,[b1,b2,b3]))
	    val c = Chars.orb(Chars.orb(Chars.<<(Byte2Char b2,0w24),
					Chars.<<(Byte2Char b1,0w16)),
			      Chars.orb(Chars.<<(Byte2Char b4,0w08),
					Byte2Char b3))
         in if isUnicode c then (c,f4)
	    else raise DecodeError(f4,false,ERR_NON_UNI_UCS4 c)
         end

      fun getCharUcs4sl f = 
         let 
            val (b1,f1) = getByte f
            val (b2,f2) = getByte f1 handle EndOfFile f 
	       => raise DecodeError(f,true,ERR_EOF_UCS4(1,[b1]))
            val (b3,f3) = getByte f2 handle EndOfFile f 
	       => raise DecodeError(f,true,ERR_EOF_UCS4(1,[b1,b2]))
	    val (b4,f4) = getByte f3 handle EndOfFile f 
	       => raise DecodeError(f,true,ERR_EOF_UCS4(1,[b1,b2,b3]))
	    val c = Chars.orb(Chars.orb(Chars.<<(Byte2Char b3,0w24),
					Chars.<<(Byte2Char b4,0w16)),
			      Chars.orb(Chars.<<(Byte2Char b1,0w08),
					Byte2Char b2))
         in if isUnicode c then (c,f4)
	    else raise DecodeError(f4,false,ERR_NON_UNI_UCS4 c)
         end
   end


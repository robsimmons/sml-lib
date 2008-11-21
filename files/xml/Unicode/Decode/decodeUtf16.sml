






signature DecodeUtf16 =
   sig
      val getCharUtf16b : DecodeFile.File -> UniChar.Char * DecodeFile.File
      val getCharUtf16l : DecodeFile.File -> UniChar.Char * DecodeFile.File
   end

structure DecodeUtf16 : DecodeUtf16 = 
   struct
      open 
	 UniChar Encoding
	 DecodeFile DecodeError DecodeUtil

      fun getCharUtf16b f =
         let 
	    val (b1,f1) = getByte f
	    val (b2,f2) = getByte f1 handle exn as EndOfFile f 
	       => raise DecodeError(f,true,ERR_EOF_UTF16 b1)
	    val c = Chars.orb(Chars.<<(Byte2Char b1,0w8),Byte2Char b2) 
        in 
	   if isSurrogate c then (* Chars.orb(c,0wx7FF)=0wxDFFF *)
	       if isLowSurrogate c then raise DecodeError(f2,false,ERR_LOW_SURROGATE c)
	       else let 
		       val (b3,f3) = getByte f2 handle exn as EndOfFile f 
			  => raise DecodeError(f,true,ERR_EOF_SURROGATE c)
		       val (b4,f4) = getByte f3 handle exn as EndOfFile f 
			  => raise DecodeError(f,true,ERR_EOF_UTF16 b3)
		       val c1 = Chars.orb(Chars.<<(Byte2Char b3,0w8),Byte2Char b4)
		    in if isLowSurrogate c1 then (combineSurrogates(c,c1),f4)
		       else raise DecodeError(f4,false,ERR_HIGH_SURROGATE(c,c1))
		    end
            else (c,f2)
         end

      fun getCharUtf16l f =
         let 
	    val (b1,f1) = getByte f
	    val (b2,f2) = getByte f1 handle exn as EndOfFile f 
	       => raise DecodeError(f,true,ERR_EOF_UTF16 b1)
	    val c = Chars.orb(Chars.<<(Byte2Char b2,0w8),Byte2Char b1) 
         in 
	    if isSurrogate c then
	       if isLowSurrogate c then raise DecodeError(f2,false,ERR_LOW_SURROGATE c)
	       else let 
		       val (b3,f3) = getByte f2 handle exn as EndOfFile f 
			  => raise DecodeError(f,true,ERR_EOF_SURROGATE c)
		       val (b4,f4) = getByte f3 handle exn as EndOfFile f 
			  => raise DecodeError(f,true,ERR_EOF_UTF16 b3)
		       val c1 = Chars.orb(Chars.<<(Byte2Char b4,0w8),Byte2Char b3)
		    in if isLowSurrogate c1 then (combineSurrogates(c,c1),f4)
		       else raise DecodeError(f4,false,ERR_HIGH_SURROGATE(c,c1))
		    end
            else (c,f2)
         end
   end

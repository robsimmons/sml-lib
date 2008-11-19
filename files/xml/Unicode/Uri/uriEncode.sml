signature UriEncode =
   sig
      val Data2UriUtf8  : UniChar.Data -> string
      val Data2UriLatin : UniChar.Data -> string

      val Vector2UriUtf8  : UniChar.Vector -> string
      val Vector2UriLatin : UniChar.Vector -> string

      val String2UriUtf8  : string -> string
      val String2UriLatin : string -> string
   end

structure UriEncode : UriEncode = 
   struct

      open UniChar UniClasses

      infix 8 >> >>>
      infix 7 && &&&
      infix 6 ||

      val op &&  = Word8.andb
      val op &&& = Chars.andb
      val op >>  = Word8.>>
      val op >>> = Chars.>>
      val op ||  = Word8.orb

      val Char2Byte = Word8.fromLargeWord o Chars.toLargeWord

      fun encodeCharUtf8 c = 
	 if c<0wx80 then [Char2Byte c]
	 else if c<0wx800 
		 then [0wxC0 || Char2Byte(c >>> 0w6),
		       0wx80 || Char2Byte(c &&& 0wx3F)]
	 else if c<0wx10000
		 then [0wxE0 || Char2Byte(c >>> 0w12),
		       0wx80 || Char2Byte((c >>> 0w6) &&& 0wx3F),
		       0wx80 || Char2Byte(c &&& 0wx3F)]
	 else if c<0wx200000
		 then [0wxF0 || Char2Byte(c >>> 0w18),
		       0wx80 || Char2Byte((c >>> 0w12) &&& 0wx3F),
		       0wx80 || Char2Byte((c >>> 0w6) &&& 0wx3F),
		       0wx80 || Char2Byte(c &&& 0wx3F)]
	 else if c<0wx4000000
		 then [0wxF8 || Char2Byte(c >>> 0w24),
		       0wx80 || Char2Byte((c >>> 0w18) &&& 0wx3F),
		       0wx80 || Char2Byte((c >>> 0w12) &&& 0wx3F),
		       0wx80 || Char2Byte((c >>> 0w6) &&& 0wx3F),
		       0wx80 || Char2Byte(c &&& 0wx3F)]
	      else [0wxFC || Char2Byte(c >>> 0w30),
		    0wx80 || Char2Byte((c >>> 0w24) &&& 0wx3F),
		    0wx80 || Char2Byte((c >>> 0w18) &&& 0wx3F),
		    0wx80 || Char2Byte((c >>> 0w12) &&& 0wx3F),
		    0wx80 || Char2Byte((c >>> 0w6) &&& 0wx3F),
		    0wx80 || Char2Byte(c &&& 0wx3F)]

      fun Byte2Cc b = 
	 let fun Quad2C b = if b<0wxA then Byte.byteToChar(b+0wx30) else Byte.byteToChar(b+0wx37)
	 in (Quad2C(b >> 0w4),Quad2C(b && 0wx0F))
	 end

      fun precedesHex (i,cv) = 
	 if Vector.length cv <= i+2 then false
	 else let val (c1,c2) = (Vector.sub(cv,i+1),Vector.sub(cv,i+2))
	      in isHex c1 andalso isHex c2
	      end

      fun Vector2UriUtf8 cv = 
	 let val revd = Vector.foldli
	    (fn (i,c,s) => if c<0wx80 andalso (c<>0wx25 orelse precedesHex(i,cv)) 
			      then Char2char c::s
			   else foldl (fn (b,s) => let val (c1,c2) = Byte2Cc b
						   in c2::c1:: #"%"::s
						   end) 
			      s (encodeCharUtf8 c)) 
	    nil cv
	 in String.implode (rev revd)
	 end

      fun Vector2UriLatin cv = 
	 let val revd = Vector.foldli
	    (fn (i,c,s) => if c<0wx80 andalso (c<>0wx25 orelse precedesHex(i,cv)) 
			      then Char2char c::s
			   else (if c>= 0w100 then s
				 else let val (c1,c2) = Byte2Cc (Char2Byte c)
				      in c2::c1:: #"%"::s
				      end)) 
	    nil cv
	 in String.implode (rev revd)
	 end

      val Data2UriUtf8 = Vector2UriUtf8 o Data2Vector
      val Data2UriLatin = Vector2UriLatin o Data2Vector

      val String2UriUtf8 = Vector2UriUtf8 o String2Vector
      val String2UriLatin = Vector2UriLatin o String2Vector
   end


(*
require "basis.__word";
require "basis.__word8";
require "basis.__word8_vector";

require "chars";
require "encodeBasic";
require "encodeError";
*)
signature EncodeMisc = 
   sig
      val writeCharAscii  : UniChar.Char * EncodeBasic.File -> EncodeBasic.File 
      val writeCharEbcdic : UniChar.Char * EncodeBasic.File -> EncodeBasic.File 
      val writeCharLatin1 : UniChar.Char * EncodeBasic.File -> EncodeBasic.File  
      val writeCharUcs4B  : UniChar.Char * EncodeBasic.File -> EncodeBasic.File 
      val writeCharUcs4L  : UniChar.Char * EncodeBasic.File -> EncodeBasic.File 
      val writeCharUcs4SB : UniChar.Char * EncodeBasic.File -> EncodeBasic.File 
      val writeCharUcs4SL : UniChar.Char * EncodeBasic.File -> EncodeBasic.File 
      val writeCharUtf8   : UniChar.Char * EncodeBasic.File -> EncodeBasic.File  
      val writeCharUtf16B : UniChar.Char * EncodeBasic.File -> EncodeBasic.File 
      val writeCharUtf16L : UniChar.Char * EncodeBasic.File -> EncodeBasic.File 
      val writeCharUcs2B  : UniChar.Char * EncodeBasic.File -> EncodeBasic.File 
      val writeCharUcs2L  : UniChar.Char * EncodeBasic.File -> EncodeBasic.File 

      val validCharAscii  : UniChar.Char -> bool
      val validCharEbcdic : UniChar.Char -> bool
      val validCharLatin1 : UniChar.Char -> bool
   end

structure EncodeMisc : EncodeMisc =
   struct
      open UniChar EncodeBasic EncodeError

      infix 8 >>
      infix 7 &&
      infix 6 ||

      val op && = Chars.andb
      val op >> = Chars.>>
      val op || = Word8.orb

      fun splitSurrogates (c : Char) = 
	 (((c-0wx10000) >> 0w10)+0wxD800,c && 0wx3FF + 0wxDC00)

      fun Char2Byte c = Word8.fromLargeWord(Chars.toLargeWord c)

      (*---------------------------------------------------------------------*)
      (* Ascii                                                               *)
      (*---------------------------------------------------------------------*)
      fun validCharAscii (c : Char) = c<0wx80
      fun writeCharAscii(c,f) = 
	 if c<0wx80 then writeByte(f,Char2Byte c)
	 else raise EncodeError(f,ERR_ILLEGAL_CHAR(c,"ASCII"))

      (*---------------------------------------------------------------------*)
      (* Ebcdic                                                              *)
      (*---------------------------------------------------------------------*)
      val latin2ebcdicTab = Word8Vector.fromList 
	  [0wx00,0wx01,0wx02,0wx03,0wx37,0wx2D,0wx2E,0wx2F,
	   0wx16,0wx05,0wx25,0wx0B,0wx0C,0wx0D,0wx0E,0wx0F,
	   0wx10,0wx11,0wx12,0wx13,0wx3C,0wx3D,0wx32,0wx26,
	   0wx18,0wx19,0wx3F,0wx27,0wx1C,0wx1D,0wx1E,0wx1F,
	   0wx40,0wx4F,0wx7F,0wx7B,0wx5B,0wx6C,0wx50,0wx7D,
	   0wx4D,0wx5D,0wx5C,0wx4E,0wx6B,0wx60,0wx4B,0wx61,
	   0wxF0,0wxF1,0wxF2,0wxF3,0wxF4,0wxF5,0wxF6,0wxF7,
	   0wxF8,0wxF9,0wx7A,0wx5E,0wx4C,0wx7E,0wx6E,0wx6F,
	   0wx7C,0wxC1,0wxC2,0wxC3,0wxC4,0wxC5,0wxC6,0wxC7,
	   0wxC8,0wxC9,0wxD1,0wxD2,0wxD3,0wxD4,0wxD5,0wxD6,
	   0wxD7,0wxD8,0wxD9,0wxE2,0wxE3,0wxE4,0wxE5,0wxE6,
	   0wxE7,0wxE8,0wxE9,0wx4A,0wxE0,0wx5A,0wx5F,0wx6D,
	   0wx79,0wx81,0wx82,0wx83,0wx84,0wx85,0wx86,0wx87,
	   0wx88,0wx89,0wx91,0wx92,0wx93,0wx94,0wx95,0wx96,
	   0wx97,0wx98,0wx99,0wxA2,0wxA3,0wxA4,0wxA5,0wxA6,
	   0wxA7,0wxA8,0wxA9,0wxC0,0wx6A,0wxD0,0wxA1,0wx07,
	   0wx20,0wx21,0wx22,0wx23,0wx24,0wx15,0wx06,0wx17,
	   0wx28,0wx29,0wx2A,0wx2B,0wx2C,0wx09,0wx0A,0wx1B,
	   0wx30,0wx31,0wx1A,0wx33,0wx34,0wx35,0wx36,0wx08,
	   0wx38,0wx39,0wx3A,0wx3B,0wx04,0wx14,0wx3E,0wxE1,
	   0wx41,0wx42,0wx43,0wx44,0wx45,0wx46,0wx47,0wx48,
	   0wx49,0wx51,0wx52,0wx53,0wx54,0wx55,0wx56,0wx57,
	   0wx58,0wx59,0wx62,0wx63,0wx64,0wx65,0wx66,0wx67,
	   0wx68,0wx69,0wx70,0wx71,0wx72,0wx73,0wx74,0wx75,
	   0wx76,0wx77,0wx78,0wx80,0wx8A,0wx8B,0wx8C,0wx8D,
	   0wx8E,0wx8F,0wx90,0wx9A,0wx9B,0wx9C,0wx9D,0wx9E,
	   0wx9F,0wxA0,0wxAA,0wxAB,0wxAC,0wxAD,0wxAE,0wxAF,
	   0wxB0,0wxB1,0wxB2,0wxB3,0wxB4,0wxB5,0wxB6,0wxB7,
	   0wxB8,0wxB9,0wxBA,0wxBB,0wxBC,0wxBD,0wxBE,0wxBF,
	   0wxCA,0wxCB,0wxCC,0wxCD,0wxCE,0wxCF,0wxDA,0wxDB,
	   0wxDC,0wxDD,0wxDE,0wxDF,0wxEA,0wxEB,0wxEC,0wxED,
	   0wxEE,0wxEF,0wxFA,0wxFB,0wxFC,0wxFD,0wxFE,0wxFF
	   ]
      fun validCharEbcdic (c : Char) = c<0wx100
      fun writeCharEbcdic(c,f) = 
	 if c<0wx100 then writeByte(f,Word8Vector.sub(latin2ebcdicTab,Chars.toInt c))
	 else raise EncodeError(f,ERR_ILLEGAL_CHAR(c,"EBCDIC"))

      (*---------------------------------------------------------------------*)
      (* Latin1                                                              *)
      (*---------------------------------------------------------------------*)
      fun validCharLatin1 (c : Char) = c<0wx100
      fun writeCharLatin1(c,f) = 
	 if c<0wx100 then writeByte(f,Char2Byte c)
 	 else raise EncodeError(f,ERR_ILLEGAL_CHAR(c,"LATIN-1"))

      (*---------------------------------------------------------------------*)
      (* UCS-4                                                               *)
      (*---------------------------------------------------------------------*)
      fun ucs4Bytes c = (Char2Byte(c >> 0w24),
			 Char2Byte(c >> 0w16),
			 Char2Byte(c >> 0w8),
			 Char2Byte c)
      fun writeCharUcs4 perm = 
	 fn (c,f) => let val bytes = ucs4Bytes c
			 val (b1,b2,b3,b4) = perm bytes
			 val f1 = writeByte(f,b1)
			 val f2 = writeByte(f1,b2)
			 val f3 = writeByte(f2,b3)
			 val f4 = writeByte(f3,b4)
		     in f4
		     end
      fun permUcs4B x = x
      fun permUcs4L (b1,b2,b3,b4) = (b4,b3,b2,b1)
      fun permUcs4SB (b1,b2,b3,b4) = (b2,b1,b4,b3)
      fun permUcs4SL (b1,b2,b3,b4) = (b3,b4,b1,b2)

      val writeCharUcs4B  = writeCharUcs4 permUcs4B
      val writeCharUcs4L  = writeCharUcs4 permUcs4L
      val writeCharUcs4SB = writeCharUcs4 permUcs4SB
      val writeCharUcs4SL = writeCharUcs4 permUcs4SL

      (*---------------------------------------------------------------------*)
      (* UTF-8                                                               *)
      (*---------------------------------------------------------------------*)
      fun writeCharUtf8(c,f) = 
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

      (*---------------------------------------------------------------------*)
      (* UTF-16                                                              *)
      (*---------------------------------------------------------------------*)
      fun oneUtf16 isL (c,f) = 
	 let val (b1,b2) = (Char2Byte(c >> 0w8),Char2Byte c)
	 in if isL then writeByte(writeByte(f,b2),b1)
	    else writeByte(writeByte(f,b1),b2)
	 end
      fun writeCharUtf16 isL = 
	 fn (c,f) => 
	 if c<0wx10000 then oneUtf16 isL (c,f)
	 else let val (hi,lo) = splitSurrogates c
		  val f1 = oneUtf16 isL (hi,f)
		  val f2 = oneUtf16 isL (lo,f1)
	      in f2
	      end
      val writeCharUtf16B  = writeCharUtf16 false
      val writeCharUtf16L  = writeCharUtf16 true

      (*---------------------------------------------------------------------*)
      (* UCS-2                                                               *)
      (*---------------------------------------------------------------------*)
      fun writeCharUcs2 isL = 
	 fn (c,f) => 
	 if c<0wx10000 
	    then let val (b1,b2) = (Char2Byte(c >> 0w8),Char2Byte c)
		 in if isL then writeByte(writeByte(f,b2),b1)
		    else writeByte(writeByte(f,b1),b2)
		 end
	 else raise EncodeError(f,ERR_ILLEGAL_CHAR(c,"UCS-2"))

      val writeCharUcs2B  = writeCharUcs2 false
      val writeCharUcs2L  = writeCharUcs2 true

   end

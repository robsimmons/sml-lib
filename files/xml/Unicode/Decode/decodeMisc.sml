signature DecodeMisc = 
   sig
      val getCharAscii  : DecodeFile.File -> UniChar.Char * DecodeFile.File
      val getCharEbcdic : DecodeFile.File -> UniChar.Char * DecodeFile.File
      val getCharEof    : DecodeFile.File -> UniChar.Char * DecodeFile.File
      val getCharLatin1 : DecodeFile.File -> UniChar.Char * DecodeFile.File
   end

structure DecodeMisc : DecodeMisc = 
   struct
      open 
	 UniChar DecodeFile DecodeError
	 
      fun getCharEof f = raise EndOfFile f

      (*--------------------------------------------------------------------*)
      (* ASCII characters must be lower than 0wx80                          *)
      (*--------------------------------------------------------------------*)
      fun getCharAscii f = 
	 let val (b,f1) = getByte f
	 in if b<0wx80 then (Byte2Char b,f1)
	    else raise DecodeError(f1,false,ERR_ILLEGAL_CHAR(b,"ASCII"))
	 end

      (*--------------------------------------------------------------------*)
      (* LATIN-1 is the first plane of Unicode.                             *)
      (*--------------------------------------------------------------------*)
      fun getCharLatin1 f = let val (b,f1) = getByte f
			    in (Byte2Char b,f1)
			    end

      (*--------------------------------------------------------------------*)
      (* EBCDIC is mapped to the first plane of Unicode.                    *)
      (*--------------------------------------------------------------------*)
      (* according to rfc-1345 (and gnu recode experiments) *)
      val ebcdic2latinTab = Vector.fromList 
	  [0wx00,0wx01,0wx02,0wx03,0wx9C,0wx09,0wx86,0wx7F,
	   0wx97,0wx8D,0wx8E,0wx0B,0wx0C,0wx0D,0wx0E,0wx0F,
	   0wx10,0wx11,0wx12,0wx13,0wx9D,0wx85,0wx08,0wx87,
	   0wx18,0wx19,0wx92,0wx8F,0wx1C,0wx1D,0wx1E,0wx1F,
	   0wx80,0wx81,0wx82,0wx83,0wx84,0wx0A,0wx17,0wx1B,
	   0wx88,0wx89,0wx8A,0wx8B,0wx8C,0wx05,0wx06,0wx07,
	   0wx90,0wx91,0wx16,0wx93,0wx94,0wx95,0wx96,0wx04,
	   0wx98,0wx99,0wx9A,0wx9B,0wx14,0wx15,0wx9E,0wx1A,
	   0wx20,0wxA0,0wxA1,0wxA2,0wxA3,0wxA4,0wxA5,0wxA6,
	   0wxA7,0wxA8,0wx5B,0wx2E,0wx3C,0wx28,0wx2B,0wx21,
	   0wx26,0wxA9,0wxAA,0wxAB,0wxAC,0wxAD,0wxAE,0wxAF,
	   0wxB0,0wxB1,0wx5D,0wx24,0wx2A,0wx29,0wx3B,0wx5E,
	   0wx2D,0wx2F,0wxB2,0wxB3,0wxB4,0wxB5,0wxB6,0wxB7,
	   0wxB8,0wxB9,0wx7C,0wx2C,0wx25,0wx5F,0wx3E,0wx3F,
	   0wxBA,0wxBB,0wxBC,0wxBD,0wxBE,0wxBF,0wxC0,0wxC1,
	   0wxC2,0wx60,0wx3A,0wx23,0wx40,0wx27,0wx3D,0wx22,
	   0wxC3,0wx61,0wx62,0wx63,0wx64,0wx65,0wx66,0wx67,
	   0wx68,0wx69,0wxC4,0wxC5,0wxC6,0wxC7,0wxC8,0wxC9,
	   0wxCA,0wx6A,0wx6B,0wx6C,0wx6D,0wx6E,0wx6F,0wx70,
	   0wx71,0wx72,0wxCB,0wxCC,0wxCD,0wxCE,0wxCF,0wxD0,
	   0wxD1,0wx7E,0wx73,0wx74,0wx75,0wx76,0wx77,0wx78,
	   0wx79,0wx7A,0wxD2,0wxD3,0wxD4,0wxD5,0wxD6,0wxD7,
	   0wxD8,0wxD9,0wxDA,0wxDB,0wxDC,0wxDD,0wxDE,0wxDF,
	   0wxE0,0wxE1,0wxE2,0wxE3,0wxE4,0wxE5,0wxE6,0wxE7,
	   0wx7B,0wx41,0wx42,0wx43,0wx44,0wx45,0wx46,0wx47,
	   0wx48,0wx49,0wxE8,0wxE9,0wxEA,0wxEB,0wxEC,0wxED,
	   0wx7D,0wx4A,0wx4B,0wx4C,0wx4D,0wx4E,0wx4F,0wx50,
	   0wx51,0wx52,0wxEE,0wxEF,0wxF0,0wxF1,0wxF2,0wxF3,
	   0wx5C,0wx9F,0wx53,0wx54,0wx55,0wx56,0wx57,0wx58,
	   0wx59,0wx5A,0wxF4,0wxF5,0wxF6,0wxF7,0wxF8,0wxF9,
	   0wx30,0wx31,0wx32,0wx33,0wx34,0wx35,0wx36,0wx37,
	   0wx38,0wx39,0wxFA,0wxFB,0wxFC,0wxFD,0wxFE,0wxFF
	   ] 
	   
      fun ebcdic2latin b = Vector.sub(ebcdic2latinTab,Word8.toInt b)

      fun getCharEbcdic f = let val (b,f1) = getByte f
			    in (ebcdic2latin b,f1)
			    end
   end

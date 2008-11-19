




(*--------------------------------------------------------------------------*)
(* Structure: DecodeError                                                   *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   decodeMessage : none                                                   *)
(*--------------------------------------------------------------------------*)
signature DecodeError = 
   sig
      datatype DecodeError =
	  ERR_ILLEGAL_CHAR of DecodeFile.Byte * string
	| ERR_NON_UNI_UCS4 of UniChar.Char
	| ERR_EOF_UCS4 of int * DecodeFile.Byte list
	| ERR_NON_DIRECT_UTF7 of DecodeFile.Byte
	| ERR_PADDING_UTF7 of UniChar.Char
	| ERR_ILLFORMED_UTF8 of DecodeFile.Byte * int * int
	| ERR_ILLEGAL_UTF8 of DecodeFile.Byte
	| ERR_INVALID_UTF8_SEQ of DecodeFile.Byte list
	| ERR_EOF_UTF8 of int * int
	| ERR_NON_UNI_UTF8 of UniChar.Char * int
	| ERR_EOF_UCS2 of DecodeFile.Byte
	| ERR_EOF_UTF16 of DecodeFile.Byte
	| ERR_LOW_SURROGATE of UniChar.Char
	| ERR_HIGH_SURROGATE of UniChar.Char * UniChar.Char
	| ERR_EOF_SURROGATE of UniChar.Char
	| ERR_NO_ENC_DECL of string
	| ERR_UNSUPPORTED_ENC of string
	| ERR_INCOMPATIBLE_ENC of string * string

      val decodeMessage : DecodeError -> string list

      exception DecodeError of DecodeFile.File * bool * DecodeError 
   end

structure DecodeError : DecodeError =
   struct
      open 
	 DecodeFile UtilString UniChar 

      datatype DecodeError =
	  ERR_ILLEGAL_CHAR of DecodeFile.Byte * string
	| ERR_NON_UNI_UCS4 of UniChar.Char
	| ERR_EOF_UCS4 of int * DecodeFile.Byte list
	| ERR_NON_DIRECT_UTF7 of DecodeFile.Byte
	| ERR_PADDING_UTF7 of UniChar.Char
	| ERR_ILLFORMED_UTF8 of DecodeFile.Byte * int * int
	| ERR_ILLEGAL_UTF8 of DecodeFile.Byte
	| ERR_INVALID_UTF8_SEQ of DecodeFile.Byte list
	| ERR_EOF_UTF8 of int * int
	| ERR_NON_UNI_UTF8 of UniChar.Char * int
	| ERR_EOF_UCS2 of DecodeFile.Byte
	| ERR_EOF_UTF16 of DecodeFile.Byte
	| ERR_LOW_SURROGATE of UniChar.Char
	| ERR_HIGH_SURROGATE of UniChar.Char * UniChar.Char
	| ERR_EOF_SURROGATE of UniChar.Char
	| ERR_NO_ENC_DECL of string
	| ERR_UNSUPPORTED_ENC of string
	| ERR_INCOMPATIBLE_ENC of string * string

      fun Char2Hex c = "0x"^UtilString.toUpperString(StringCvt.padLeft #"0" 4 (Chars.toString c))
	  
      fun decodeMessage err = 
	 case err
	   of ERR_ILLEGAL_CHAR(b,what) => 
	      [Byte2Hex b,"is not",prependAnA what,"character"]

	    | ERR_NON_UNI_UCS4 c => 
	      ["UCS-4 coded non-Unicode character",Char2Uni c]
	    | ERR_EOF_UCS4(pos,bytes) => 
	      ["End of file after",Int2String pos,"bytes of UCS-4 character",
	       "starting with ",List2String0 Byte2Hex bytes]
	   
	    | ERR_NON_DIRECT_UTF7 b => 
	      ["Indirect UTF-7 character ",Byte2Hex b,"in non-shifted mode"]
	    | ERR_PADDING_UTF7 pad => 
	      ["Non-zero padding",Char2Hex pad,"at end of UTF-7 shifted sequence"]

	    | ERR_ILLFORMED_UTF8 (b,len,pos) => 
	      [numberNth pos,"byte",Byte2Hex b,"of a",Int2String len^"-byte", 
	       "UTF-8 sequence does not start with bits 10"] 
	    | ERR_ILLEGAL_UTF8 b => 
	      ["Byte",Byte2Hex b,"is neither ASCII nor does it start",
	       "a valid multi-byte UTF-8 sequence"]
	    | ERR_EOF_UTF8 (len,pos) => 
	      ["End of file terminates a ",Int2String len^"-byte",
	       "UTF-8 sequence before the ",numberNth pos,"byte"]
	    | ERR_NON_UNI_UTF8 (c,len) => 
	      [Int2String len^"-byte UTF-8 sequence decodes to non-Unicode character",Char2Uni c]
	    | ERR_INVALID_UTF8_SEQ bs => 
	      ["Invalid UTF-8 sequence",List2xString (""," ","") Byte2Hex bs]
	      
	    | ERR_EOF_UCS2 b => 
	      ["End of file before second byte of UCS-2 character starting with",Byte2Hex b]
	    | ERR_EOF_UTF16 b => 
	      ["End of file before second byte of UTF-16 character starting with",Byte2Hex b]

	    | ERR_LOW_SURROGATE c => 
	      ["Low surrogate",Char2Uni c,"without preceding high surrogate"]
	    | ERR_HIGH_SURROGATE (c,c1) => 
	      ["High surrogate",Char2Uni c,"followed by",Char2Uni c1,"instead of low surrogate"]
	    | ERR_EOF_SURROGATE c => 
	      ["High surrogate",Char2Uni c,"followed by the end of file"]

	    | ERR_NO_ENC_DECL auto => 
	      ["Couldn't parse encoding declaration but auto-detected encoding",auto,"required so"]
	    | ERR_UNSUPPORTED_ENC enc => 
	      ["Unsupported encoding",enc]
	    | ERR_INCOMPATIBLE_ENC (enc,auto) => 
	      ["Encoding",enc,"is incompatible with auto-detected encoding",auto]

      exception DecodeError of File * bool * DecodeError 
   end



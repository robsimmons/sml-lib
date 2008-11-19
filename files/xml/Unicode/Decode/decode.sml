(*--------------------------------------------------------------------------*)
(* Structure: Decode                                                        *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   checkEncoding : NoSuchFile                                             *)
(*   encCloseFile  : none                                                   *)
(*   encFileName   : none                                                   *)
(*--------------------------------------------------------------------------*)
signature Decode =
   sig
      structure Error : DecodeError

      type DecFile

      exception DecEof of DecFile
      exception DecError of DecFile * bool * Error.DecodeError

      val decUri       : DecFile -> Uri.Uri 
      val decName      : DecFile -> string
      val decEncoding  : DecFile -> Encoding.Encoding

      val decOpenXml   : Uri.Uri option -> DecFile
      val decOpenUni   : Uri.Uri option * Encoding.Encoding -> DecFile
      val decClose     : DecFile -> DecFile

      val decCommit    : DecFile -> unit
      val decSwitch    : DecFile * string -> DecFile

      val decGetChar   : DecFile -> UniChar.Char * DecFile
      val decGetArray  : DecFile -> UniChar.Char array -> int * DecFile * Error.DecodeError option
   end

structure Decode : Decode = 
   struct
      structure Error = DecodeError
      open
	 UniChar Encoding Error 
	 DecodeFile DecodeMisc DecodeUcs2 DecodeUcs4 
	 DecodeUtf16 DecodeUtf8 DecodeUtil

      type DecFile = Encoding * File 
      exception DecEof of DecFile
      exception DecError of DecFile * bool * DecodeError

      (*--------------------------------------------------------------------*)
      (* close an encoded entity.                                           *)
      (*--------------------------------------------------------------------*)
      fun decClose (_,f) = (NOENC,f) before closeFile f
      (*--------------------------------------------------------------------*)
      (* get the uri string of an encoded entity.                           *)
      (*--------------------------------------------------------------------*)
      fun decName (_,f) = fileName f
      (*--------------------------------------------------------------------*)
      (* get the uri of an encoded entity.                                  *)
      (*--------------------------------------------------------------------*)
      fun decUri (_,f) = fileUri f
      (*--------------------------------------------------------------------*)
      (* get the encoding of an encoded entity.                             *)
      (*--------------------------------------------------------------------*)
      fun decEncoding (enc,_) = enc

      (*--------------------------------------------------------------------*)
      (* commit the auto-detected encoding.                                 *)
      (*--------------------------------------------------------------------*)
      fun decCommit (enc,f) = 
	 case enc
	   of UTF8 => ()
	    | UTF16B => ()
	    | UTF16L => ()
	    | _ => raise DecError((enc,f),false,ERR_NO_ENC_DECL(encodingName enc))

      (*--------------------------------------------------------------------*)
      (* change to another - compatible - encoding.                         *)
      (*--------------------------------------------------------------------*)
      fun decSwitch ((enc,f),decl) =
	 let
	    val decEnc = isEncoding decl
	    val _ = if decEnc<>NOENC then () 
		    else raise DecError((enc,f),false,ERR_UNSUPPORTED_ENC decl)
	    val newEnc = switchEncoding(enc,decEnc)
	    val _ = if decEnc<>NOENC orelse enc=NOENC then () 
		    else raise DecError((enc,f),false,ERR_INCOMPATIBLE_ENC(encodingName enc,decl))
	 in (newEnc,f)
	 end
      
      (*--------------------------------------------------------------------*)
      (* get a character from an encoded entity.                            *)
      (*--------------------------------------------------------------------*)
      fun decGetChar (enc,f) =
	 let val (c,f1) = 
	    case enc
	      of NOENC  => raise EndOfFile f
	       | ASCII  => getCharAscii f
	       | EBCDIC => getCharEbcdic f
	       | LATIN1 => getCharLatin1 f
	       | UCS2B  => getCharUcs2b f
	       | UCS2L  => getCharUcs2l f
	       | UCS4B  => getCharUcs4b f
	       | UCS4L  => getCharUcs4l f
	       | UCS4SB => getCharUcs4sb f
	       | UCS4SL => getCharUcs4sl f
	       | UTF8   => getCharUtf8 f
	       | UTF16B => getCharUtf16b f
	       | UTF16L => getCharUtf16l f
	 in (c,(enc,f1))
	 end
      handle EndOfFile f            => raise DecEof(NOENC,f)
	   | DecodeError(f,eof,err) => raise DecError((enc,f),eof,err)

      (*--------------------------------------------------------------------*)
      (* Load new characters, depending on the current entity's encoding.   *)
      (*--------------------------------------------------------------------*)
      fun decGetArray (enc,f) arr = 
	 let 
	    (*--------------------------------------------------------------*)
	    (* Load the buffer with len new characters, or until the entity *)
	    (* end is reached. Close the current file in that case.         *)
	    (* Local exception Ended is needed in order to preserve tail    *)
	    (* recursion.                                                   *)
	    (*--------------------------------------------------------------*)
            fun loadArray getChar = 
	       let 
		  val ende = Array.length arr
		  exception Error of int * exn
		  fun doit (idx,f) = 
		     if idx=ende then (ende,(enc,f),NONE)
		     else let val (c,f1) = getChar f handle exn => raise Error (idx,exn)
			      val _ = Array.update(arr,idx,c)
			  in doit (idx+1,f1)
			  end
	       in doit (0,f) handle Error(idx,exn) 
		  => case exn 
		       of EndOfFile f => (idx,(NOENC,f),NONE)
			| DecodeError (f,_,err) => (idx,(enc,f),SOME err)
			| _ => raise exn
	       end
	 in case enc
	      of NOENC  => (0,(NOENC,f),NONE)
	       | ASCII  => loadArray getCharAscii 
	       | EBCDIC => loadArray getCharEbcdic
	       | LATIN1 => loadArray getCharLatin1
	       | UCS2B  => loadArray getCharUcs2b 
	       | UCS2L  => loadArray getCharUcs2l 
	       | UCS4B  => loadArray getCharUcs4b 
	       | UCS4L  => loadArray getCharUcs4l 
	       | UCS4SB => loadArray getCharUcs4sb
	       | UCS4SL => loadArray getCharUcs4sl
	       | UTF8   => loadArray getCharUtf8 
	       | UTF16B => loadArray getCharUtf16b
	       | UTF16L => loadArray getCharUtf16l
	 end


      (*--------------------------------------------------------------------*)
      (* open an XML file and try to auto-detect its encoding.              *)
      (*--------------------------------------------------------------------*)
      (* Auto-detection of the encoding of XML entities according to App. F *)
      (* of the XML recommendation.                                         *)
      (*                                                                    *)
      (* The file is opened in basic mode and upto four bytes are read from *)
      (* it in order to detect the encoding: if they constitute a prefix of *)
      (* "<?xml" in a recognized encoding, this encoding is taken.          *)
      (*--------------------------------------------------------------------*)
      (* read upto four bytes from the file, detect the encoding, and unget *)
      (* the read bytes. Return the resulting encoded file and its encoding *)
      (*--------------------------------------------------------------------*)
      (**************************************************************************)
      (*  NB 24.08.2000 Autodetection of encoding is affected by the            *)
      (*  XML 1.0 Specification Errata (10.08.2000) E 44                        *)
      (*                                                                        *)
      (*  The first four bytes read are interpreted according to:               *)
      (*                                                                        *)
      (* "Append the following to the second paragraph:                         *)
      (* The notation ## is used to denote any byte value except 00.            *)
      (* Adjust the itemized list of detection cases to read as follows:        *)
      (*                                                                        *)
      (* With a Byte Order Mark:                                                *)
      (* 00 00 FE FF: UCS-4, big-endian machine (1234 order)                    *)
      (* FF FE 00 00: UCS-4, little-endian machine (4321 order)                 *)
      (* FE FF 00 ##:  UTF-16, big-endian                                       *)
      (* FF FE ## 00:  UTF-16, little-endian                                    *) 
      (* EF BB BF: UTF-8                                                        *)
      (* Without a Byte Order Mark:                                             *)
      (* 00 00 00 3C: UCS-4, big-endian machine (1234 order)                    *)
      (* 3C 00 00 00: UCS-4, little-endian machine (4321 order)                 *)
      (* 00 00 3C 00: UCS-4, unusual octet order (2143)                         *)
      (* 00 3C 00 00: UCS-4, unusual octet order (3412)                         *)
      (* 00 3C ## ##,                                                           *)
      (* 00 25 ## ##,                                                           *)
      (* 00 20 ## ##,                                                           *)
      (* 00 09 ## ##,                                                           *)
      (* 00 0D ## ## or                                                         *)
      (* 00 0A ## ##: Big-endian UTF-16 or ISO-10646-UCS-2. Note that, absent   *)
      (*           an encoding declaration, these cases are strictly            *)
      (*           speaking in error.                                           *)
      (* 3C 00 ## ##,                                                           *)
      (* 25 00 ## ##,                                                           *)
      (* 20 00 ## ##,                                                           *)
      (* 09 00 ## ##,                                                           *)
      (* 0D 00 ## ## or                                                         *)
      (* 0A 00 ## ##: Little-endian UTF-16 or ISO-10646-UCS-2. Note that, absent*)
      (*           an encoding declaration, these cases are strictly            *)
      (*           speaking in error.                                           *)
      (* 3C 3F 78 6D: UTF-8, ISO 646, ASCII, some part of ISO 8859, Shift-JIS,  *)
      (*           EUC, or any other 7-bit, 8-bit, or mixed-width encoding      *)
      (*           which ensures that the characters of ASCII have their        *)
      (*            normal positions, width, and values; the actual encoding    *)
      (*           declaration must be read to detect which of these            *)
      (*           applies, but since all of these encodings use the same       *)
      (*           bit patterns for the ASCII characters, the encoding          *)
      (*            declaration itself may be read reliably                     *)
      (* 4C 6F A7 94: EBCDIC (in some flavor; the full encoding declaration     *)
      (*           must be read to tell which code page is in use)              *)
      (* other: UTF-8 without an encoding declaration, or else the data stream  *)
      (*      is corrupt, fragmentary, or enclosed in a wrapper of some kind    *)
      (**************************************************************************)



      fun decOpenXml uri = 
	 let 
	    fun get4Bytes (n,f) = 
	       if n=4 then (nil,f)
	       else let val (b,f1) = getByte f
			val (bs,f2) = get4Bytes (n+1,f1)
		    in (b::bs,f2)
		    end
		 handle EndOfFile f => (nil,f)
		    
	    fun detect bs = 
	      case bs 
		of
		  [0wx0,0wx0,0wxFE,0wxFF] => (UCS4B,nil)
		| [0wxFF,0wxFE,0wx0,0wx0] => (UCS4L,nil)
		| [0wxFE,0wxFF,0wx0,b4] => 
		    if b4 <> 0wx0 then (UTF16B,[0wx0,b4])
		    else (UTF8,bs)
	        | [0wxFF,0wxFE,b3,0wx0] =>
		    if b3 <> 0wx0 then (UTF16L,[b3,0wx0])
		    else (UTF8,bs)
		| [0wxEF,0wxBB,0wxBF,b4] => (UTF8,[b4])
		| [0wx0,0wx0,0wx0,0wx3C] => (UCS4B,bs)
		| [0wx3C,0wx0,0wx0,0wx0] => (UCS4L,bs)
		| [0wx0,0wx0,0wx3C,0wx0] => (UCS4SB,bs)
		| [0wx0,0wx3C,0wx0,0wx0] => (UCS4SL,bs)
		| [0wx0,b2,b3,b4] =>
		    if (b2=0wx3C orelse b2=0wx25 orelse b2=0wx20 
			orelse b2=0wx09 orelse b2=0wx0D orelse b2=0wx0A)
		      andalso (b3<>0wx0 orelse b4<>0wx0) then (UTF16B,bs)
		    else (UTF8,bs)
		| [b1,0wx0,b3,b4] =>
		    if (b1=0wx3C orelse b1=0wx25 orelse b1=0wx20 
			orelse b1=0wx09 orelse b1=0wx0D orelse b1=0wx0A)
		      andalso (b3<>0wx0 orelse b4<>0wx0) then (UTF16L,bs)
		    else (UTF8,bs)
		| [0wx4C,0wx6F,0wxA7,0wx94] => (EBCDIC,bs)
		| _ => (UTF8,bs)

	    val f = openFile uri
	    val (bs,f1) = get4Bytes(0,f)
	    val (enc,unget) = detect bs
	 in (enc,ungetBytes(f1,unget))
	 end

      (*--------------------------------------------------------------------*)
      (* open a Unicode file. Check whether it starts with a byte order     *)
      (* mark. If yes, chose UTF16 encoding, otherwise use the default that *)
      (* is provided as second argument.                                    *)
      (*                                                                    *)
      (* return the encoded file, a list of bytes looked ahead and the      *)
      (* encoding.                                                          *)
      (*--------------------------------------------------------------------*)
      fun decOpenUni (uri,default) = 
	 let
	    fun def(f,bs) = 
	       (default,ungetBytes(f,bs))
	    fun detect f = 
	       let val (b1,f1) = getByte f 
	       in case b1
		    of 0wxFE => (let val (b2,f2) = getByte f1 
				 in if b2 = 0wxFF then (UTF16B,f2)
				    else def(f2,[b1,b2])
				 end handle EndOfFile f => def(f,[b1]))
		     | 0wxFF => (let val (b2,f2) = getByte f1
				 in if b2 = 0wxFE then (UTF16L,f2)
				    else def(f2,[b1,b2])
				 end handle EndOfFile f => def(f,[b1]))
		     | _ => def(f1,[b1])
	       end handle EndOfFile f => def(f,nil)
	    val f = openFile uri
	    val (enc,f1) = detect f 
	 in (enc,f1)
	 end
   end




signature Encoding =
   sig
      datatype Encoding = 
	 NOENC  | ASCII  | EBCDIC | LATIN1 
       | UCS4B  | UCS4L  | UCS4SB | UCS4SL 
       | UCS2B  | UCS2L  | UTF16B | UTF16L
       | UTF8

      val UCS2  : Encoding
      val UCS4  : Encoding
      val UTF16 : Encoding

      val encodingName   : Encoding -> string
      val isEncoding     : string -> Encoding
      val switchEncoding : Encoding * Encoding -> Encoding 
   end

structure Encoding : Encoding = 
   struct
      open StringDict

      datatype Encoding = 
	 NOENC  | ASCII  | EBCDIC | LATIN1 
       | UCS4B  | UCS4L  | UCS4SB | UCS4SL 
       | UCS2B  | UCS2L  | UTF16B | UTF16L
       | UTF8

      val UCS2  = UCS2B
      val UCS4  = UCS4B
      val UTF16 = UTF16B

      fun encodingName enc =
	 case enc 
	   of NOENC  => "NONE"
	    | ASCII  => "ASCII"
	    | EBCDIC => "EBCDIC"
	    | LATIN1 => "ISO-8859-1"
	    | UCS2B  => "UCS-2"
	    | UCS2L  => "UCS-2"
	    | UCS4B  => "UCS-4"
	    | UCS4L  => "UCS-4"
	    | UCS4SB => "UCS-4"
	    | UCS4SL => "UCS-4"
	    | UTF8   => "UTF-8"
	    | UTF16B => "UTF-16"
	    | UTF16L => "UTF-16"

      val encDict = makeDict("encoding",6,NOENC)
      val encAliases = 
	 [(ASCII,["ANSI_X3.4-1968","ANSI_X3.4-1986","ASCII","US-ASCII","US",		       
		    "ISO646-US","ISO-IR-6","ISO_646.IRV:1991","IBM367","CP367"]),
	  (EBCDIC,["EBCDIC"]),                 
	  (LATIN1,["ISO_8859-1:1987","ISO-8859-1","ISO_8859-1",
		     "ISO-IR-100","CP819","IBM819","L1","LATIN1"]),
	  (UCS2,["UCS-2","ISO-10646-UCS-2"]),
	  (UCS4,["UCS-4","ISO-10646-UCS-4"]),
	  (UTF16,["UTF-16"]),
	  (UTF8,["UTF-8"])
	  ]
      val _ = app (fn (x,ys) => app (fn y => setByKey(encDict,y,x)) ys) encAliases
      fun isEncoding name = getByKey(encDict,name)

      fun compatAscii new =
	 case new
	   of ASCII  => new
	    | LATIN1 => new
	    | UTF8   => new
	    | _      => NOENC
      fun compatUcs4 (old,new) = 
	 if new=UCS4 then old else NOENC

      fun switchEncoding(old,new) =
	 case old
	   of NOENC  => NOENC
	    | ASCII  => compatAscii new
	    | EBCDIC => if new=EBCDIC then new else NOENC
	    | LATIN1 => compatAscii new
	    | UCS4B  => compatUcs4(old,new)
	    | UCS4L  => compatUcs4(old,new)
	    | UCS4SB => compatUcs4(old,new)
	    | UCS4SL => compatUcs4(old,new)
	    | UTF16B => if new=UTF16 then old else if new=UCS2 then UCS2B else NOENC 
	    | UTF16L => if new=UTF16 then old else if new=UCS2 then UCS2L else NOENC 
	    | UCS2B  => if new=UCS2 then old else if new=UTF16 then UTF16B else NOENC
	    | UCS2L  => if new=UCS2 then old else if new=UTF16 then UTF16L else NOENC
	    | UTF8   => compatAscii new
   end

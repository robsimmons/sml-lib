









signature Encode = 
   sig
      include EncodeError

      type File
      type EncFile

      val encNoFile    : EncFile
      val encStdOut    : EncFile
      val encOpenFile  : string * Encoding.Encoding * string -> EncFile
      val encCloseFile : EncFile -> unit
      val encAdapt     : EncFile * File -> EncFile

      val encPutChar   : EncFile * UniChar.Char -> EncFile
      val encValidChar : EncFile * UniChar.Char -> bool
   end

structure Encode : Encode = 
   struct
      open 
	 Encoding UtilError
	 EncodeBasic EncodeError EncodeMisc
	 
      type EncFile = Encoding * File

      val encNoFile = (NOENC,stdOutFile)
      val encStdOut = (LATIN1,stdOutFile)

      fun encAdapt((enc,_),f) = (enc,f)

      fun encValidChar((enc,_),c) = 
	 case enc
	   of ASCII  => validCharAscii c
	    | EBCDIC => validCharEbcdic c
	    | LATIN1 => validCharLatin1 c
	    | _ => true

      fun encPutChar((enc,f),c) = 
	 let val f1 = 
	    case enc
	      of NOENC  => f
	       | ASCII  => (writeCharAscii(c,f))
	       | EBCDIC => (writeCharEbcdic(c,f))
	       | LATIN1 => (writeCharLatin1(c,f))
	       | UCS2B  => (writeCharUcs2B(c,f))
	       | UCS2L  => (writeCharUcs2L(c,f))
	       | UCS4B  => (writeCharUcs4B(c,f))
	       | UCS4L  => (writeCharUcs4L(c,f))
	       | UCS4SB => (writeCharUcs4SB(c,f))
	       | UCS4SL => (writeCharUcs4SL(c,f))
	       | UTF8   => (writeCharUtf8(c,f))
	       | UTF16B => (writeCharUtf16B(c,f))
	       | UTF16L => (writeCharUtf16L(c,f))
	 in (enc,f1)
	 end

      fun encCloseFile(_,f) = closeFile f

      fun encOpenFile (fname,enc,name) = 
	 let 
	    val outEnc = 
	       case enc 
		 of NOENC => 
		    (case isEncoding name
		       of NOENC => raise NoSuchFile(fname,"Unsupported encoding \""^name^"\"")
			| enc => enc)
		  | enc => enc
	    val f   = openFile fname
	    val f1  = case outEnc 
			of UTF16B => writeByte(writeByte(f,0wxFE),0wxFF)
			 | UTF16L => writeByte(writeByte(f,0wxFF),0wxFE)
			 | _      => f 
	 in (outEnc,f1)
	 end
   end


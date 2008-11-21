(*--------------------------------------------------------------------------*)
(* Structure: DecodeBasic                                                   *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   closeFile : none                                                       *)
(*   filePos   : none                                                       *)
(*   fileName  : none                                                       *)
(*   nextByte  : EndOfFile                                                  *)
(*   openFile  : NoSuchFile                                                 *)
(*--------------------------------------------------------------------------*)
signature DecodeReader =
sig
   type state

   exception EndOfFile of state

   val Char2Byte : UniChar.Char -> Word8.word
   val Byte2Char : Word8.word -> UniChar.Char
   val Byte2Hex  : Word8.word -> string

   val openFile   : Uri.Uri option -> state
   val closeFile  : state -> unit

   val getByte    : state -> Word8.word * state
   val ungetBytes : state * Word8.word list -> state
end

structure DecodeReader : DecodeReader =
struct
   open
      UniChar Uri UtilError 

   fun Byte2Char b = Chars.fromLargeWord(Word8.toLargeWord b)
   fun Byte2Hex b = 
      "0x"^UtilString.toUpperString(StringCvt.padLeft #"0" 2 (Word8.toString b))
   fun Char2Byte c = Word8.fromLargeWord(Chars.toLargeWord c)

   type instream = TextIO.instream 
   val closeIn   = TextIO.closeIn
   val input     = TextIO.input
   val input1    = TextIO.input1
   val openIn    = TextIO.openIn
   val stdIn     = TextIO.stdIn

   (*--------------------------------------------------------------------*)
   (* a file type is stdin or a uri with its string representation and   *)
   (* the file it is mapped to.                                          *)
   (* a file position is a stream, an int position and a file type.      *)
   (* a file is a file position, a buffer, its size and current index.   *)
   (*--------------------------------------------------------------------*)
   datatype FileType = STD | FNAME of (Uri * string * string * bool)
   type FilePos = FileType * instream * int
   type state = FilePos * Word8Vector.vector * int * int

   exception EndOfFile of state
   val nullVec = Word8Vector.fromList nil


   (*--------------------------------------------------------------------*)
   (* open a file; report IO errors by raising NoSuchFile.               *)
   (*--------------------------------------------------------------------*)
   fun openFile uriOpt = 
      let val (typ,stream) = 
	 case uriOpt 
	   of NONE => (STD,stdIn)
	    | SOME uri => let val (str,fname,tmp) = retrieveUri uri
			  in (FNAME(uri,str,fname,tmp),openIn fname)
			  end
		       handle IO.Io {name,cause,...} 
		       => raise NoSuchFile(name,exnMessage cause)
      in ((typ,stream,0),nullVec,0,0)
      end

   (*--------------------------------------------------------------------*)
   (* close the file; ignore IO errors.                                  *)
   (*--------------------------------------------------------------------*)
   fun closeStream (typ,stream,_) =
      case typ
	of STD => ()
	 | FNAME(_,uri,fname,tmp) 
	   => let val _ = closeIn stream handle IO.Io _ => ()
		  val _ = (if tmp andalso OS.FileSys.access(fname,nil) 
			      then OS.FileSys.remove fname else ())
		     handle exn as OS.SysErr _ =>
			TextIO.output(TextIO.stdErr,String.concat
				      ["Error removing temporary file ",fname,"for URI",uri,
				       "(",exnMessage exn,")\n"])

	      in ()
	      end
   fun closeFile (tsp,_,_,_) = closeStream tsp

   (*--------------------------------------------------------------------*)
   (* read a byte from the file; if at the end of buffer, reload it.     *)
   (* if a reload fails or returns an IO error, raise EndOfFile. --------*)
   (*--------------------------------------------------------------------*)
   fun getByte (tsp,vec,s,i) =
      if i<s then (Word8Vector.sub(vec,i),(tsp,vec,s,i+1))
      else let val (typ,stream,pos) = tsp
	       val v = Byte.stringToBytes (input stream) handle IO.Io _ => nullVec
	       val s = Word8Vector.length v
	   in if s=0 then let val _ = closeStream tsp
			  in raise EndOfFile(tsp,v,0,0)
			  end
	      else (Word8Vector.sub(v,0),((typ,stream,pos+s),v,s,1))
	   end

   (*--------------------------------------------------------------------*)
   (* un-get some bytes. this should only happen while checking for a    *)
   (* byte-order mark or xml/text declaration. It should be efficient in *)
   (* that case, otherwise might be very space-consuming.                *)
   (*--------------------------------------------------------------------*)
   fun ungetBytes ((tsp,vec,s,i),bs) =
      let val len = length bs
      in if len<=i then (tsp,vec,s,i-len)
	 else let val diff = len-i
		  val vec0 = Word8Vector.fromList(List.take(bs,diff))
	      in (tsp,Word8Vector.concat [vec0,vec],s+diff,0)
	      end
      end
end

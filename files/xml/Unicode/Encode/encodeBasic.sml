





(*--------------------------------------------------------------------------*)
(* Structure: EncodeBasic                                                   *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   closeFile : none                                                       *)
(*   fileName  : none                                                       *)
(*   openFile  : NoSuchFile                                                 *)
(*   writeByte : Io                                                         *)
(*--------------------------------------------------------------------------*)
signature EncodeBasic =
   sig
      type File

      val stdOutFile : File
      val closeFile  : File -> unit
      val fileName   : File -> string
      val openFile   : string -> File
      val writeByte  : File * Word8.word -> File
   end

structure EncodeBasic : EncodeBasic =
   struct
      open UtilError

      type outstream = TextIO.outstream
      val closeOut   = TextIO.closeOut
      val openOut    = TextIO.openOut
      val output1    = TextIO.output1
      val stdOut     = TextIO.stdOut
	 
      type File = string * outstream

      val stdOutFile = ("-",stdOut)

      fun closeFile(fname,s) = if fname="-" then () else closeOut s
      fun fileName(fname,_) = if fname="-" then "<stdout>" else fname
      fun openFile fname = 
	 if fname = "-" then (fname,stdOut)
	 else (fname,openOut fname)
	    handle IO.Io {name,cause,...} => raise NoSuchFile(name,exnMessage cause)
      fun writeByte (f as (_,s),b) = f before output1(s,chr(Word8.toInt b))
   end

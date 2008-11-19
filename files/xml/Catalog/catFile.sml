








signature CatFile = 
   sig
      type CatFile
      type Position
	 
      val catOpenFile  : Uri.Uri -> CatFile 
      val catCloseFile : CatFile -> unit
      val catGetChar   : CatFile -> UniChar.Char * CatFile
      val catPos       : CatFile -> CatError.Position
   end

functor CatFile ( structure Params : CatParams ) : CatFile = 
   struct
      open UniChar CatError Decode Params Uri UtilError 

      (* column, line, break *)  
      type PosInfo = int * int * bool
      val startPos = (0,1,false)

      datatype CatFile =
	 NOFILE of string * PosInfo 
       | DIRECT of DecFile * PosInfo

      fun catPos cf =
	 case cf 
	   of NOFILE (uri,(col,line,_)) => (uri,line,col)
	    | DIRECT (dec,(col,line,_)) => (decName dec,line,col)
	      
      fun catOpenFile uri =
	 let val dec = decOpenUni(SOME uri,!O_CATALOG_ENC)
	 in DIRECT(dec,startPos)
	 end
      handle NoSuchFile fmsg => let val _ = catError(nullPosition,ERR_NO_SUCH_FILE fmsg)
				in NOFILE(Uri2String uri,startPos)
				end
	 
      fun catCloseFile cf =
	 case cf
	   of NOFILE _ => ()
	    | DIRECT(dec,_) => ignore (decClose dec)

      fun catGetChar cf =
	 case cf
	   of NOFILE _ => (0wx00,cf)
	    | DIRECT(dec,(col,line,brk)) => 
	      (let val (c,dec1) = decGetChar dec
	       in case c
		    of 0wx09 => (c,DIRECT(dec1,(col+1,line,false)))
		     | 0wx0A => if brk then catGetChar(DIRECT(dec1,(col,line,false)))
				else (c,DIRECT(dec1,(0,line+1,false)))
		     | 0wx0D => (0wx0A,DIRECT(dec1,(0,line+1,true)))
		     | _     => if c>=0wx20 then (c,DIRECT(dec1,(col+1,line,false)))
				else let val err = ERR_ILLEGAL_HERE(c,LOC_CATALOG)
					 val _ = catError(catPos cf,err)
				     in catGetChar(DIRECT(dec1,(col+1,line,false))) 
				     end
	       end 
		  handle DecEof dec => (0wx00,NOFILE(decName dec,(col,line,brk)))
		       | DecError(dec,_,err) => 
		     let val _ = catError(catPos cf,ERR_DECODE_ERROR err)
		     in catGetChar(DIRECT(dec,(col,line,false)))
		     end
		     )
   end


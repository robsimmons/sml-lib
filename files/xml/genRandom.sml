structure GenRandom = 
   struct
      open Encode UniChar UniClasses DecodeFile

      infix 6 || 
      infix 8 << 
      val op || = Chars.orb
      val op << = Chars.<<

      fun combineUcs4big(b1,b2,b3,b4) = 
	 Byte2Char b1 << 0w24 || Byte2Char b2 << 0w16 || Byte2Char b3 << 0w08 || Byte2Char b4

      fun genRandom (name,size,mask) =
	 let 
	    val urand = BinIO.openIn ("/dev/urandom")
	    val f = encOpenFile(name,Encoding.UCS4,"UCS-4")
	    val f1 = foldl (fn (c,f) => encPutChar(f,char2Char c)) f 
	       (String.explode "<?xml version='1.0' encoding='UCS-4'?>\n<a>\n")

	    exception Error of Encode.EncFile * int
	    fun doit (f,n) = if n<=1 then f
			     else let val w = let val b1 = valOf (BinIO.input1 urand)
						  val b2 = valOf (BinIO.input1 urand)
						  val b3 = valOf (BinIO.input1 urand)
						  val b4 = valOf (BinIO.input1 urand)
					      in combineUcs4big(b1,b2,b3,b4)
					      end
					   handle Option => raise Error(f,n)
				      val c = Chars.andb(mask,w) 
				  in if isUnicode c andalso isXml c 
				     andalso c<>0wx3C andalso c<>0wx26 
				     andalso c<>0wx0D andalso c<>0wx3E
					then let val (f1,n1) = 
					   if n mod 80 = 0 
					      then (encPutChar(f,0wx0A),n-1) else (f,n)
						 val f2 = encPutChar(f1,c)
					     in doit(f2,n1-1)
					     end
				     else (print "."; doit (f,n))
				  end
			       
	    val f2 = doit (f1,size-1) handle Error(f,n) 
	       => (f before print ("Error after "^Int.toString (size-n)^" characters!\n"))
	    val f3 = foldl (fn (c,f) => encPutChar(f,char2Char c)) f (String.explode "\n</a>\n")
	    val _ = encCloseFile f3
	    val _ = BinIO.closeIn urand
	 in ()
	 end
   end

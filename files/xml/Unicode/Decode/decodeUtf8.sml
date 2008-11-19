signature DecodeUtf8 =
   sig
      val getCharUtf8 : DecodeFile.File -> UniChar.Char * DecodeFile.File
   end

structure DecodeUtf8 : DecodeUtf8 =
   struct
      open 
	 UniChar UniClasses UtilError UtilInt
	 DecodeFile DecodeError DecodeUtil

      val THIS_MODULE = "DecodeUtf8"

      infix 8 <<<
      infix 7 &&
      infix 6 |||

      val op && = Bytes.andb
      val op <<< = Chars.<<
      val op ||| = Chars.orb

      val byte1switch = Vector.tabulate 
	 (256,fn i => 
	  if i<0x80 then 1 
	  else if i<0xC0 then 0
	  else if i<0xE0 then 2
	  else if i<0xF0 then 3
	  else if i<0xF8 then 4
	  else if i<0xFC then 5
	  else if i<0xFE then 6
	       else 0)

      val diff2 : Char = 0wx00003080
      val diff3 : Char = diff2 <<< 0wx6 ||| 0wx00020080
      val diff4 : Char = diff3 <<< 0wx6 ||| 0wx00400080
      val diff5 : Char = diff4 <<< 0wx6 ||| 0wx08000080 
      val diff6 : Char = diff5 <<< 0wx6 ||| 0wx00000080

      fun getCharUtf8 f =
	 let val (b1,f1) = getByte f
	 in if b1<0wx80 then (Byte2Char b1,f1)
	    else let val n = Vector.sub(byte1switch,Word8.toInt b1)
		 in case n
		      of 0 (* error   *) => raise DecodeError(f1,false,ERR_ILLEGAL_UTF8 b1)
		       | 1 => (Byte2Char b1,f1)
		       | 2 => 
			 let 
			    val (b2,f2) = getByte f1 handle EndOfFile f
			       => raise DecodeError(f,true,ERR_EOF_UTF8(n,2))
			 in  if b2 && 0wxC0 <> 0wx80 
				then raise DecodeError(f2,false,ERR_ILLFORMED_UTF8(b2,n,2))
			     else let val c = Byte2Char b1 <<< 0w6 + Byte2Char b2 - diff2
				  in if c>=0wx80 then (c,f2)
				     else raise DecodeError(f2,false,ERR_INVALID_UTF8_SEQ [b1,b2])
				  end
			 end
		       | 3 => 
			 let 
			    val (b2,f2) = getByte f1 handle EndOfFile f
			       => raise DecodeError(f,true,ERR_EOF_UTF8(n,2))
			    val (b3,f3) = getByte f2 handle EndOfFile f
			       => raise DecodeError(f,true,ERR_EOF_UTF8(n,3))
			 in  
			    if b2 && 0wxC0 <> 0wx80 
			       then raise DecodeError(f3,false,ERR_ILLFORMED_UTF8(b2,n,2))
			    else if b3 && 0wxC0 <> 0wx80 
				    then raise DecodeError(f3,false,ERR_ILLFORMED_UTF8(b2,n,3))
				 else let val c = (Byte2Char b1 <<< 0w12 + 
						   Byte2Char b2 <<< 0w06 + 
						   Byte2Char b3 - diff3)
				      in if c>=0wx800 then (c,f3)
					 else raise DecodeError
					    (f3,false,ERR_INVALID_UTF8_SEQ [b1,b2,b3])
				      end
			 end
		       | 4 => 
			 let 
			    val (b2,f2) = getByte f1 handle EndOfFile f
			       => raise DecodeError(f,true,ERR_EOF_UTF8(n,2))
			    val (b3,f3) = getByte f2 handle EndOfFile f
			       => raise DecodeError(f,true,ERR_EOF_UTF8(n,3))
			    val (b4,f4) = getByte f3 handle EndOfFile f
			       => raise DecodeError(f,true,ERR_EOF_UTF8(n,4))
			 in  
			    if b2 && 0wxC0 <> 0wx80 
			       then raise DecodeError(f4,false,ERR_ILLFORMED_UTF8(b2,n,2))
			    else if b3 && 0wxC0 <> 0wx80 
				    then raise DecodeError(f4,false,ERR_ILLFORMED_UTF8(b2,n,3))
			    else if b4 && 0wxC0 <> 0wx80 
				    then raise DecodeError(f4,false,ERR_ILLFORMED_UTF8(b2,n,4))
				 else let val c = (Byte2Char b1 <<< 0w18 + 
						   Byte2Char b2 <<< 0w12 + 
						   Byte2Char b3 <<< 0w06 + 
						   Byte2Char b4 - diff4)
				      in 
					 if c>=0wx100000 andalso c<=0wx10FFFF then (c,f4)
					 else if c<0wx10000 
						 then raise DecodeError
						    (f4,false,ERR_INVALID_UTF8_SEQ [b1,b2,b3,b4])
					      else raise DecodeError
						 (f4,false,ERR_NON_UNI_UTF8(c,n))
				      end
			 end
		       | 5 => 
			 let 
			    val (b2,f2) = getByte f1 handle EndOfFile f
			       => raise DecodeError(f,true,ERR_EOF_UTF8(n,2))
			    val (b3,f3) = getByte f2 handle EndOfFile f
			       => raise DecodeError(f,true,ERR_EOF_UTF8(n,3))
			    val (b4,f4) = getByte f3 handle EndOfFile f
			       => raise DecodeError(f,true,ERR_EOF_UTF8(n,4))
			    val (b5,f5) = getByte f4 handle EndOfFile f
			       => raise DecodeError(f,true,ERR_EOF_UTF8(n,5))
			 in  
			    if b2 && 0wxC0 <> 0wx80 
			       then raise DecodeError(f5,false,ERR_ILLFORMED_UTF8(b2,n,2))
			    else if b3 && 0wxC0 <> 0wx80 
				    then raise DecodeError(f5,false,ERR_ILLFORMED_UTF8(b2,n,3))
			    else if b4 && 0wxC0 <> 0wx80 
				    then raise DecodeError(f5,false,ERR_ILLFORMED_UTF8(b2,n,4))
			    else if b5 && 0wxC0 <> 0wx80 
				    then raise DecodeError(f5,false,ERR_ILLFORMED_UTF8(b2,n,5))
				 else let val c = (Byte2Char b1 <<< 0w24 + 
						   Byte2Char b2 <<< 0w18 + 
						   Byte2Char b3 <<< 0w12 + 
						   Byte2Char b4 <<< 0w06 + 
						   Byte2Char b5 - diff5)
				      in if c<0wx200000 
					    then raise DecodeError
					       (f5,false,ERR_INVALID_UTF8_SEQ [b1,b2,b3,b4,b5])
					 else raise DecodeError
						 (f5,false,ERR_NON_UNI_UTF8(c,n))
				      end
			 end
		       | 6 => 
			 let 
			    val (b2,f2) = getByte f1 handle EndOfFile f
			       => raise DecodeError(f,true,ERR_EOF_UTF8(n,2))
			    val (b3,f3) = getByte f2 handle EndOfFile f
			       => raise DecodeError(f,true,ERR_EOF_UTF8(n,3))
			    val (b4,f4) = getByte f3 handle EndOfFile f
			       => raise DecodeError(f,true,ERR_EOF_UTF8(n,4))
			    val (b5,f5) = getByte f4 handle EndOfFile f
			       => raise DecodeError(f,true,ERR_EOF_UTF8(n,5))
			    val (b6,f6) = getByte f5 handle EndOfFile f
			       => raise DecodeError(f,true,ERR_EOF_UTF8(n,6))
			 in  
			    if b2 && 0wxC0 <> 0wx80 
			       then raise DecodeError(f6,false,ERR_ILLFORMED_UTF8(b2,n,2))
			    else if b3 && 0wxC0 <> 0wx80 
				    then raise DecodeError(f6,false,ERR_ILLFORMED_UTF8(b2,n,3))
			    else if b4 && 0wxC0 <> 0wx80 
				    then raise DecodeError(f6,false,ERR_ILLFORMED_UTF8(b2,n,4))
			    else if b5 && 0wxC0 <> 0wx80 
				    then raise DecodeError(f6,false,ERR_ILLFORMED_UTF8(b2,n,5))
			    else if b6 && 0wxC0 <> 0wx80 
				    then raise DecodeError(f6,false,ERR_ILLFORMED_UTF8(b2,n,6))
				 else let val c = (Byte2Char b1 <<< 0w30 + 
						   Byte2Char b2 <<< 0w24 + 
						   Byte2Char b3 <<< 0w18 + 
						   Byte2Char b4 <<< 0w12 + 
						   Byte2Char b5 <<< 0w06 + 
						   Byte2Char b6 - diff6)
				      in if c<0wx4000000 
					    then raise DecodeError
					       (f6,false,ERR_INVALID_UTF8_SEQ [b1,b2,b3,b4,b5,b6])
					 else raise DecodeError
						 (f6,false,ERR_NON_UNI_UTF8(c,n))
				      end
			 end
		       | _ => raise InternalError(THIS_MODULE,"getCharUtf8",
						  "byte1switch holds "^Int.toString n^
						  ">6 for byte "^Bytes.toString b1)
		 end
	 end
   end

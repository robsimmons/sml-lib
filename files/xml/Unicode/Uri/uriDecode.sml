signature UriDecode =
   sig
      val decodeUriLatin : string -> string
      val decodeUriUtf8  : string -> string
   end

structure UriDecode : UriDecode =
   struct
      open UniChar UtilInt

      infix 8 << <<<
      infix 7 &&
      infix 6 || |||

      val op &&  = Word8.andb
      val op <<  = Word8.<<
      val op <<< = Chars.<<
      val op ||  = Word8.orb
      val op ||| = Chars.orb

      val Byte2Char = Chars.fromLargeWord o Word8.toLargeWord

      fun hexValue c = 
         if #"0"<=c andalso #"9">=c then SOME (Byte.charToByte c-0wx30)
         else if #"A"<=c andalso #"F">=c then SOME (Byte.charToByte c-0wx37)
         else if #"a"<=c andalso #"f">=c then SOME (Byte.charToByte c-0wx57)
              else NONE

      exception Failed of char list

      fun getQuads cs = 
         case cs 
           of c1::c2::cs1 => (case (hexValue c1,hexValue c2) 
                                of (SOME b1,SOME b2) => ((b1 << 0w4 || b2),cs1)
                                 | _ => raise Failed cs1) 
            | _ => raise Failed nil
      
      (*--------------------------------------------------------------------*)
      (* decode UTF-8                                                       *)
      (*--------------------------------------------------------------------*)
      val byte1switch = Array.array(256,1)                                    (* 1 byte  *)
      val _ = appInterval (fn i => Array.update(byte1switch,i,0)) (0x80,0xBF) (* Error   *)
      val _ = appInterval (fn i => Array.update(byte1switch,i,2)) (0xC0,0xDF) (* 2 bytes *)
      val _ = appInterval (fn i => Array.update(byte1switch,i,3)) (0xE0,0xEF) (* 3 bytes *)
      val _ = appInterval (fn i => Array.update(byte1switch,i,4)) (0xF0,0xF7) (* 4 bytes *)
      val _ = appInterval (fn i => Array.update(byte1switch,i,5)) (0xF8,0xFB) (* 5 bytes *)
      val _ = appInterval (fn i => Array.update(byte1switch,i,6)) (0xFC,0xFD) (* 6 bytes *)

      val diff2 = 0wx00003080
      val diff3 = diff2 <<< 0wx6 ||| 0wx00020080
      val diff4 = diff3 <<< 0wx6 ||| 0wx00400080
      val diff5 = diff4 <<< 0wx6 ||| 0wx08000080 
      val diff6 = diff5 <<< 0wx6 ||| 0wx00000080
      val diffsByLen = Vector.fromList [0w0,0w0,diff2,diff3,diff4,diff5,diff6]

      fun getByte cs = 
         case cs 
           of #"%"::cs1 => getQuads cs1
            | c::cs1 => (Byte.charToByte c,cs1)
            | nil => raise Failed nil
                    
      fun getBytes(b,cs,n) =
         let 
            fun do_err (cs,m) = 
               if n<m then raise Failed cs 
               else let val (_,cs1) = getByte cs
                    in do_err (cs1,m+1)
                    end
            fun doit (w,cs,m) =
               if n<m then (w,cs)
               else let val (b,cs1) = getByte cs handle Failed cs => do_err(cs,m+1)
                        val w1 = if b && 0wxC0 = 0wx80 then w <<< 0w6 + Byte2Char b
                                 else do_err(cs1,m+1)
                    in doit (w1,cs1,m+1)
                    end
            val (w,cs1) = doit (Byte2Char b,cs,2)
            val diff = Vector.sub(diffsByLen,n)
            val c = w-diff
         in 
            if c<0wx100 then (Char2char c,cs1) 
            else raise Failed cs1
         end

      fun getCharUtf8 cs = 
         let val (b,cs1) = getQuads cs
         in case Array.sub(byte1switch,Word8.toInt b)
              of 0 (* error   *) => raise Failed cs1
               | 1 (* 1 byte  *) => (Byte.byteToChar b,cs1)
               | n (* n bytes *) => getBytes(b,cs1,n)
         end

      fun decodeUriUtf8 str =
         let 
            val cs = String.explode str
            
            fun doit yet nil = yet
              | doit yet (c::cs) =
	       if #"%"<>c then doit (c::yet) cs
	       else let val (yet1,cs1) = let val (ch,cs1) = getCharUtf8 cs
					 in (ch::yet,cs1)
					 end
				      handle Failed cs => (yet,cs)
		    in doit yet1 cs1
		    end
	 in 
	    String.implode(rev(doit nil cs))
         end

      (*--------------------------------------------------------------------*)
      (* decode Latin                                                       *)
      (*--------------------------------------------------------------------*)
      fun getChar cs = 
         case cs 
           of #"%"::cs1 => let val (b,cs2) = getQuads cs1 
			   in (Byte.byteToChar b,cs2)
			   end
            | c::cs1 => (c,cs1)
            | nil => raise Failed nil
                    
      fun decodeUriLatin str =
         let 
            val cs = String.explode str
            
            fun doit yet nil = yet
              | doit yet (c::cs) = 
	       let val (yet1,cs1) = let val (ch,cs1) = getChar cs
				    in (ch::yet,cs1)
				    end
				 handle Failed cs => (yet,cs)
	       in doit yet1 cs1
	       end
	 in 
	    String.implode(rev(doit nil cs))
         end
   end

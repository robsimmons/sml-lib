(*--------------------------------------------------------------------------*)
(* Structure: UniChar                                                       *)
(*                                                                          *)
(* Depends on:                                                              *)
(*   UtilString                                                             *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*--------------------------------------------------------------------------*)
signature UniChar = 
   sig
      structure Chars : WORD

      type Char = Chars.word
      type Data = Char list
      type Vector = Char vector
	 
      val nullData   : Data 
      val nullVector : Vector

      val hashChar   : Char -> word
      val hashData   : Data -> word
      val hashVector : Vector -> word

      val compareChar   : Char * Char -> order
      val compareData   : Data * Data -> order
      val compareVector : Vector * Vector -> order

      val char2Char : char -> Char
      val Char2char : Char -> char

      val Char2Uni    : Char -> string
      val Char2String : Char -> string

      val String2Data  : string -> Data
      val Data2String  : Data -> string
      val Latin2String : Data -> string

      val Data2Vector  : Data -> Vector
      val Vector2Data  : Vector -> Data

      val String2Vector : string -> Vector
      val Vector2String : Vector -> string

      val quoteUni    : Char -> string -> string
      val quoteChar   : Char -> Char -> string
      val quoteData   : Char -> Data -> string
      val quoteVector : Char -> Vector -> string
   end

structure UniChar : UniChar =
   struct
      val O_VECTOR_PRINTLEN = 48

      structure Chars = Word

      val _ = if Chars.wordSize > 21 then () 
	      else let val str = ("UniChar: Chars.wordSize is too small.\n"^
				  "Cannot compile on this system!\n" )
		       val _ = print str
		   in raise Fail str
		   end

      type Char = Chars.word
      type Data = Char list

      type CharInterval = Char * Char
      type CharRange    = CharInterval list
	 
      type Vector = Char vector

      val nullChar = 0wx0:Char
      val nullData = nil:Data
      val nullVector = Vector.fromList nullData

      val hashChar = Word.fromLargeWord o Chars.toLargeWord
      val hashData = UtilHash.hashList hashChar
      val hashVector = UtilHash.hashVector hashChar

      val compareChar = Chars.compare
      val compareData = UtilCompare.compareList compareChar
      val compareVector = UtilCompare.compareVector compareChar

      val char2Char = Chars.fromLargeWord o Word8.toLargeWord o Byte.charToByte
      val Char2char = Byte.byteToChar o Word8.fromLargeWord o Chars.toLargeWord

      fun Char2Uni c = 
	 "U+"^UtilString.toUpperString(StringCvt.padLeft #"0" 4 (Chars.toString c))
      fun Char2String c = 
	 case c
	   of 0wx9 => "\\t"
	    | 0wxA => "\\n"
	    | _ => if c<0wx100 then String.implode [Char2char c]
		   else Char2Uni c
		      
      fun String2Data s = map char2Char (String.explode s)
      fun Data2String cs = String.concat (map Char2String cs)
      fun Latin2String cs = String.implode (map Char2char cs)

      val Data2Vector   = Vector.fromList
      fun String2Vector s = Vector.tabulate(String.size s,fn i => char2Char(String.sub(s,i)))

      fun Vector2Data vec = Vector.foldr (op ::) nil vec
      fun Vector2String vec =
	 let 
	    val maxlen = O_VECTOR_PRINTLEN
	    val len = Vector.length vec
	 in  
	    if len<=maxlen orelse maxlen=0 
	       then Data2String (Vector2Data vec)
	    else let 
		    val cs1 = VectorSlice.foldri 
		       (fn (_,c,cs) => c::cs) nil 
		       (VectorSlice.slice (vec,0,SOME (maxlen div 2)))
		    val cs2 = VectorSlice.foldri 
		       (fn (_,c,cs) => c::cs) nil 
		       (VectorSlice.slice (vec,len-3-maxlen div 2,NONE))
		 in Data2String cs1^"..."^Data2String cs2
		 end
	 end

      fun quoteUni q s = let val sQ = Char2String q in sQ^s^sQ end
      fun quoteChar q c = if c=0wx0 then "entity end" else quoteUni q (Char2String c)
      fun quoteData q cs = quoteUni q (Data2String cs)
      fun quoteVector q v = quoteUni q (Vector2String v)
   end



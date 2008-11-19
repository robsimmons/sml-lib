(*--------------------------------------------------------------------------*)
(* Structure: CharClasses                                                   *)
(*                                                                          *)
(* Notes:                                                                   *)
(*   This implementation uses the UNSAFE array operations, and does NO      *)
(*   range checks. This is for efficiency reasons.                          *)
(*   If class=makeCharClass(lo,hi) then a filed of size hi-lo+1 is allo-    *)
(*   cated. In order to lookup a character, first make sure it in [lo..hi], *)
(*   then subtract lo before calling inCharClass!                           *)
(*   The same holds for addChar.                                            *)
(*                                                                          *)
(* Depends on:                                                              *)
(*   UniChar                                                                *)
(*   UtilInt                                                                *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   addChar       : none                                                   *)
(*   addCharClass  : none                                                   *)
(*   inCharClass   : none                                                   *)
(*   makeCharClass : none                                                   *)
(*--------------------------------------------------------------------------*)
signature CharClasses =
   sig
      type CharClass
      type MutableClass
      type CharInterval = UniChar.Char * UniChar.Char
      type CharRange = CharInterval list

      val initialize : CharInterval -> MutableClass
      val finalize   : MutableClass -> CharClass

      val addChar      : MutableClass * UniChar.Char * UniChar.Char * UniChar.Char -> unit
      val addCharRange : MutableClass * UniChar.Char * UniChar.Char * CharRange -> CharRange

      val inCharClass : UniChar.Char * CharClass -> bool
   end

structure CharClasses : CharClasses =
   struct 
      open UniChar

      type CharInterval = Char * Char
      type CharRange = CharInterval list
	 
      val Char2Word = Word.fromLargeWord o Chars.toLargeWord

      (*--------------------------------------------------------------------*)
      (* helpers                                                            *)
      (*--------------------------------------------------------------------*)
      infix 5 >> >>> <<<
      infix 6 || |||
      infix 6 --
      infix 7 & && &&&
      val op >> = Chars.>>
      val op -- = Chars.-
      val op || = Chars.orb
      val op && = Chars.andb
      val op >>> = Word32.>>
      val op <<< = Word32.<<
      val op &&& = Word32.andb
      val op ||| = Word32.orb
      val op & = Word.andb

      val max32 = Word32.notb 0wx0
   
      (*--------------------------------------------------------------------*)
      (* a char class is an array of words, interpreted as bitvectors.      *)
      (*--------------------------------------------------------------------*)
      type MutableClass = Word32.word array
      type CharClass = Word32.word vector

      (*--------------------------------------------------------------------*)
      (* each word in a char class holds 32 entries. Thus the for a char c  *)
      (* is c div 32 == c >> 5. The bitmask is a word of zeros, only the    *)
      (* significant bit for c, i.e. the (c && 31==0x1F)th bit set to one.  *)
      (*--------------------------------------------------------------------*)
      fun indexMask c = let val idx = Chars.toInt(c>>0w5)
			    val mask = 0wx1 <<< Char2Word c & 0w31
			in (idx,mask)
			end

      (*--------------------------------------------------------------------*)
      (* generate index and mask, then lookup.                              *)
      (*--------------------------------------------------------------------*)
      fun inCharClass(c,vec) = let val (idx,mask) = indexMask c
			       in mask &&& Vector.sub(vec,idx) <> 0wx0
			       end 

      (*--------------------------------------------------------------------*)
      (* generate a CharClass large enough to hold (max-min+1) characters.  *)
      (*--------------------------------------------------------------------*)
      fun initialize(min,max) = 
	 Array.array((Chars.toInt max-Chars.toInt min+1) div 32+1,0wx0):MutableClass
      fun finalize arr = Array.vector arr
	 
      (*--------------------------------------------------------------------*)
      (* add a single character to a CharClass.                             *)
      (*--------------------------------------------------------------------*)
      fun addChar(cls,min,max,c) =
	 let 
	    val (idx,new) = indexMask c
	    val old = Array.sub(cls,idx)
	 in
	    Array.update(cls,idx,old|||new)
	 end 
			    
      (*--------------------------------------------------------------------*)
      (* add a full range of characters to a CharClass.                     *)
      (* this is the only function that computes the offset before access   *)
      (* to the array.                                                      *)
      (*--------------------------------------------------------------------*)
      fun addCharRange(cls,min,max,range) = (* returns intervals from range which are not between min and max *)
	 let 
	    fun doOne (lo,hi) = 
	       let 
		  val (l,h) = (lo-min,hi-min)
		  val (idxL,idxH) = ((Chars.toInt l) div 32,(Chars.toInt h) div 32)
		  val (bitL,bitH) = (Char2Word l & 0w31,Char2Word h & 0w31)
	       in 
		  if idxL=idxH then 
		     let 
			val new = (max32>>>(0w31-bitH+bitL))<<<bitL
			val old = Array.sub(cls,idxL)
			val _ = Array.update(cls,idxL,old|||new)
		     in ()
		     end
		  else if idxL<idxH then
		     let
			val newL = max32<<<bitL
			val newH = max32>>>(0w31-bitH)
			val oldL = Array.sub(cls,idxL)
			val oldH = Array.sub(cls,idxH)
			val _ = Array.update(cls,idxL,oldL|||newL)
			val _ = Array.update(cls,idxH,oldH|||newH)
			val _ = UtilInt.appInterval (fn i => Array.update(cls,i,max32)) 
			   (idxL+1,idxH-1)
		     in ()
		     end
		       else ()
	       end
	    fun doAll nil = nil
	      | doAll ((lh as (lo,hi))::lhs) = 
	       if hi<lo then doAll lhs
	       else if hi<min then doAll lhs
	       else if lo>max then lh::doAll lhs
	       else if lo<min andalso hi<=max
		       then (doOne(min,hi); doAll lhs)
	       else if lo>=min andalso hi<=max
		       then (doOne lh; doAll lhs)
	       else if lo>=min andalso hi>max
		       then (doOne(lo,max); (max+0w1,hi)::lhs)
		    else (doOne(min,max); (max+0w1,hi)::lhs)
	    val _ = doAll range
	 in 
	    doAll range
	 end
   end


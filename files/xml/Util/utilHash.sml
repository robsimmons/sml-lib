signature UtilHash =
   sig
      val hashPair   : ('a -> word) * ('b -> word) -> 'a * 'b -> word
      val hashTriple : ('a -> word) * ('b -> word) * ('c -> word) -> 'a * 'b * 'c -> word

      val hashOption : ('a -> word) -> 'a option -> word
      val hashList   : ('a -> word) -> 'a list -> word 
      val hashVector : ('a -> word) -> 'a vector -> word 

      val hashString : string -> word

      val hashInt       : int -> word
      val hashIntPair   : int * int -> word
      val hashIntTriple : int * int * int -> word

      val hashWord       : word -> word
      val hashWordPair   : word * word -> word
      val hashWordTriple : word * word * word -> word
   end

structure UtilHash : UtilHash =
   struct
      fun hashPair (hashA,hashB) (a,b) = 
	 0w1327 * hashA a + 0w3853 * hashB b
      fun hashTriple (hashA,hashB,hashC) (a,b,c) = 
	 0w1327 * hashA a + 0w3853 * hashB b + 0w2851 * hashC c

      val hashInt =
	 Word.fromInt
      fun hashIntPair (i,j) = 
	 0w1327 * Word.fromInt i + 0w3853 * Word.fromInt j
      fun hashIntTriple (i,j,k) = 
	 0w1327 * Word.fromInt i + 0w3853 * Word.fromInt j + 0w2851 * Word.fromInt k

      fun hashWord w = w
      fun hashWordPair (i,j) = 0w1327 * i + 0w3853 * j
      fun hashWordTriple (i,j,k) = 0w1327 * i + 0w3853 * j + 0w2851 * k

      val hashChar = Word.fromInt o ord 
      fun hashString s = 
	 case String.size s
	   of 0 => 0wx0
	    | 1 => 0w1 + hashChar(String.sub(s,0))
	    | 2 => let val w1 = String.sub(s,0)
		       val w2 = String.sub(s,1)
		   in 0w2 + hashChar w1 * 0wx1327 + hashChar w2
		   end
	    | n => let val w1 = String.sub(s,0)
		       val w2 = String.sub(s,1)
		       val wn = String.sub(s,n-1)
		   in 0w3 + hashChar w1 * 0wx3853 + hashChar w2 * 0wx1327 + hashChar wn
		   end
	 

      fun hashOption hashA opt = 
	 case opt
	   of NONE => 0w0
	    | SOME a => 0w1 + hashA a

      fun hashList hashA l = 
	 case l
	   of nil => 0wx0
	    | [a] => 0w1 + hashA a
	    | a1::a2::_ => 0w2 + 0w3853 * hashA a1 + 0wx1327 * hashA a2

      fun hashVector hashA cv = 
	 case Vector.length cv
	   of 0 => 0wx0
	    | 1 => 0w1 + hashA(Vector.sub(cv,0))
	    | 2 => let val w1 = Vector.sub(cv,0)
		       val w2 = Vector.sub(cv,1)
		   in 0w2 + hashA w1 * 0wx1327 + hashA w2
		   end
	    | n => let val w1 = Vector.sub(cv,0)
		       val w2 = Vector.sub(cv,1)
		       val wn = Vector.sub(cv,n-1)
		   in 0w3 + hashA w1 * 0wx3853 + hashA w2 * 0wx1327 + hashA wn
		   end
   end









signature IntSets =
   sig 
      eqtype IntSet

      val emptyIntSet   : IntSet
      val singleIntSet  : int -> IntSet
      val fullIntSet    : int -> IntSet

      val isEmptyIntSet : IntSet -> bool
      val inIntSet      : int * IntSet -> bool

      val compareIntSets: IntSet * IntSet -> order 
      val hashIntSet    : IntSet -> word

      val addIntSet     : int * IntSet -> IntSet
      val delIntSet     : int * IntSet -> IntSet

      val cupIntSets    : IntSet * IntSet -> IntSet
      val capIntSets    : IntSet * IntSet -> IntSet
      val diffIntSets   : IntSet * IntSet -> IntSet

      val IntSet2List : IntSet -> int list
      val IntList2Set : int list -> IntSet
   end

structure IntSets : IntSets = 
   struct
      structure W = Word32
      val wordSize = W.wordSize
 
      type IntSet = W.word vector
      
      infix 7 << >>
      infix 6 &&
      infix 5 ||

      val op >> = W.>>
      val op << = W.<<
      val op && = W.andb
      val op || = W.orb
      val !! = W.notb

      fun normalize (vec:IntSet) = 
	 let val max = VectorSlice.foldli
	    (fn (i,w,max) => if w=0wx0 then i else max) 0 
	    (VectorSlice.slice (vec,0,NONE))
	 in VectorSlice.vector (VectorSlice.slice (vec,0,SOME max))
	 end

      val emptyIntSet = Vector.fromList nil : IntSet

      fun fullIntSet n = let val size = (n+wordSize-1) div wordSize
			     val full = 0w0-0w1:W.word
			     val bits = (n-1) mod wordSize+1
			     val last = full >> (Word.fromInt (wordSize-bits))
			 in Vector.tabulate(n div wordSize+1,
					    fn i => if i<size-1 then full else last):IntSet
			 end

      fun singleIntSet n = 
	 let 
	    val idx = n div wordSize
	    val mask = 0w1 << (Word.fromInt (n mod wordSize))
	 in 
	    Vector.tabulate(idx+1,fn i => if i=idx then mask else 0w0):IntSet
	 end

      fun isEmptyIntSet vec = Vector.length vec=0

      fun inIntSet(n,vec) = 
	 let val idx = n div wordSize
	 in if idx>=Vector.length vec then false 
	    else let val mask = 0w1 << (Word.fromInt (n mod wordSize))
		 in Vector.sub(vec,idx) && mask <> 0w0
		 end
	 end

      fun addIntSet(n,vec) = 
	 let
	    val idx = n div wordSize
	    val mask = 0w1 << (Word.fromInt (n mod wordSize))
	    val size = Vector.length vec
	 in 
	    if size>idx 
	       then Vector.mapi (fn (i,x) => if i=idx then x||mask else x) vec
	    else Vector.tabulate 
	       (idx+1,fn i => if i<size then Vector.sub(vec,i) else if i=idx then mask else 0w0)
	 end

      fun delIntSet(n,vec) = 
	 let
	    val idx = n div wordSize
	    val size = Vector.length vec
	    val vec1 = if size<=idx then vec
		       else let val mask = !! (0w1 << (Word.fromInt (n mod wordSize)))
			    in Vector.mapi 
			       (fn (i,x) => if i=idx then x && mask else x) vec
			    end
	 in normalize vec1
	 end
      
      fun capIntSets(vec1,vec2) = 
	 let 
	    val l12 = Int.min(Vector.length vec1,Vector.length vec2)
	    val v12 = Vector.tabulate(l12,fn i => Vector.sub(vec1,i) && Vector.sub(vec2,i))
	 in 
	    normalize v12
	 end

      fun cupIntSets(vec1,vec2) = 
	 let 
	    val (l1,l2) = (Vector.length vec1,Vector.length vec2)
	    val (shorter,longer,v) = if l1<=l2 then (l1,l2,vec2) else (l2,l1,vec1)
	 in 
	    Vector.tabulate (longer,fn i => if i>=shorter then Vector.sub(v,i)
					    else Vector.sub(vec1,i) || Vector.sub(vec2,i))
	 end

      fun diffIntSets(vec1,vec2) = 
	 let 
	    val (l1,l2) = (Vector.length vec1,Vector.length vec2)
	    val vec1 = Vector.tabulate 
	       (l1,fn i => if i>=l2 then Vector.sub(vec1,i)
			   else Vector.sub(vec1,i) && !!(Vector.sub(vec2,i)))
	 in normalize vec1
	 end
      
      fun IntList2Set l = List.foldl addIntSet emptyIntSet l

      fun IntSet2List vec = 
	 let 
	    val size = Vector.length vec
	    fun doOne (w,off,yet) = 
	       let fun doit (i,mask) = 
		  if i=wordSize then yet 
		  else if w&&mask=0w0 then doit(i+1,mask<<0wx1)
		       else (off+i)::doit(i+1,mask<<0wx1)
	       in doit(0,0wx1)
	       end
	    fun doAll i = if i>=size then nil
			  else doOne(Vector.sub(vec,i),wordSize*i,(doAll (i+1)))
	 in doAll 0
	 end

      fun compareIntSets (vec1,vec2:IntSet) =
	 let 
	    val (l1,l2) = (Vector.length vec1,Vector.length vec2)
	    val (l12,ifEq) = case Int.compare(l1,l2)
			       of LESS => (l1,LESS)
				| order => (l2,order)
	    fun doit i = if i>=l12 then ifEq
			 else case W.compare(Vector.sub(vec1,i),Vector.sub(vec2,i))
				of EQUAL => doit (i+1)
				 | order => order
	 in doit 0
	 end

      val intShift = case Int.precision 
		       of NONE => 0w0
			| SOME x => Word.fromInt(Int.max(wordSize-x+1,0))

      fun hashIntSet vec = 
	 case Vector.length vec
	   of 0 => 0w0 
	    | 1 => Word.fromInt(W.toInt(W.>>(Vector.sub(vec,0),intShift)))
	    | l => Word.fromInt(W.toInt(W.>>(Vector.sub(vec,0)+Vector.sub(vec,l-1),intShift)))
   end			      

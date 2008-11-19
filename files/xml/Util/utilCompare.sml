signature UtilCompare =
   sig
      type 'a Comparer = 'a * 'a -> order

      val comparePair   : 'a Comparer * 'b Comparer -> ('a * 'b) Comparer
      val compareTriple : 'a Comparer * 'b Comparer * 'c Comparer -> ('a * 'b * 'c) Comparer

      val compareOption : 'a Comparer -> 'a option Comparer 
      val compareList   : 'a Comparer -> 'a list Comparer 
      val compareVector : 'a Comparer -> 'a vector Comparer

      val compareInt       : int Comparer
      val compareIntPair   : (int * int) Comparer
      val compareIntTriple : (int * int * int) Comparer

      val compareWord       : word Comparer
      val compareWordPair   : (word * word) Comparer  
      val compareWordTriple : (word * word * word) Comparer 
   end

structure UtilCompare : UtilCompare =
   struct
      type 'a Comparer = 'a * 'a -> order

      fun comparePair (compareA,compareB) ((a1,b1),(a2,b2)) = 
	 case compareA(a1,a2)
	   of EQUAL => compareB(b1,b2)
	    | order => order 
      fun compareTriple (compareA,compareB,compareC) ((a1,b1,c1),(a2,b2,c2)) = 
	 case compareA(a1,a2)
	   of EQUAL => (case compareB(b1,b2)
			  of EQUAL => compareC(c1,c2)
			   | order => order) 
	    | order => order 

      val compareInt = Int.compare
      fun compareIntPair((x1,y1),(x2,y2)) =
	 case Int.compare(x1,x2) 
	   of EQUAL => Int.compare (y1,y2)
	    | order => order 
      fun compareIntTriple((x1,y1,z1),(x2,y2,z2)) =
	 case Int.compare(x1,x2) 
	   of EQUAL => (case Int.compare (y1,y2)
			  of EQUAL => Int.compare (z1,z2)
			   | order => order)
	    | order => order 

      val compareWord = Word.compare
      fun compareWordPair((x1,y1),(x2,y2)) =
	 case Word.compare(x1,x2) 
	   of EQUAL => Word.compare (y1,y2)
	    | order => order 
      fun compareWordTriple((x1,y1,z1),(x2,y2,z2)) =
	 case Word.compare(x1,x2) 
	   of EQUAL => (case Word.compare (y1,y2)
			  of EQUAL => Word.compare (z1,z2)
			   | order => order)
	    | order => order 

      fun compareOption compareA opts =
	 case opts
	   of (NONE,NONE) => EQUAL
	    | (NONE,SOME x) => LESS
	    | (SOME x,NONE) => GREATER
	    | (SOME x,SOME y) => compareA(x,y)

      fun compareList compA ll = 
	 let fun doit (nil,nil) = EQUAL
	       | doit (nil,_) = LESS
	       | doit (_,nil) = GREATER
	       | doit (a1::as1,a2::as2) = case compA(a1,a2)
					    of EQUAL => doit(as1,as2)
					     | order => order
	 in doit ll
	 end
      
      fun compareVector compA (vec1,vec2) =
	 let val (l,l2) = (Vector.length vec1,Vector.length vec2)
	 in case Int.compare(l,l2)
              of EQUAL => let fun doit i = if i>=l then EQUAL
                                           else case compA(Vector.sub(vec1,i),Vector.sub(vec2,i))
                                                  of EQUAL => doit (i+1)
                                                   | order => order 
                          in doit 0
                          end
               | order => order 
         end
   end


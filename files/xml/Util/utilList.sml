


(*--------------------------------------------------------------------------*)
(* Structure: UtilList                                                      *)
(*                                                                          *)
(* Depends on:                                                              *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   member        : none                                                   *)
(*   findAndDelete : none                                                   *)
(*--------------------------------------------------------------------------*)
signature UtilList =
   sig
      val split         : ('a -> bool) -> 'a list -> 'a list list
      val member        : ''a -> ''a list -> bool
      val mapAllPairs   : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
      val findAndMap    : ('a -> 'b option) -> 'a list -> 'b option
      val findAndDelete : ('a -> bool) -> 'a list -> ('a option * 'a list)

      val sort          : ('a * 'a -> order) -> 'a list -> 'a list
      val merge         : ('a * 'a -> order) -> 'a list * 'a list -> 'a list
      val diff          : ('a * 'a -> order) -> 'a list * 'a list -> 'a list
      val cap           : ('a * 'a -> order) -> 'a list * 'a list -> 'a list
      val sub           : ('a * 'a -> order) -> 'a list * 'a list -> bool
      val insert        : ('a * 'a -> order) -> 'a * 'a list -> 'a list
      val delete        : ('a * 'a -> order) -> 'a * 'a list -> 'a list
      val elem          : ('a * 'a -> order) -> 'a * 'a list -> bool
   end
      
structure UtilList : UtilList =
   struct
      (*--------------------------------------------------------------------*)
      (* split a list into a list of lists at each element fullfilling p.   *)
      (*--------------------------------------------------------------------*) 
      fun split p l = 
	 let val (one,ls) = foldr
	    (fn (a,(curr,ls)) => if p a then (nil,curr::ls) else (a::curr,ls)) 
	    (nil,nil) l
	 in one::ls
	 end

      (*--------------------------------------------------------------------*)
      (* is x a member of l?                                                *)
      (*--------------------------------------------------------------------*)
      fun member x l = List.exists (fn y => x=y) l

      (*--------------------------------------------------------------------*)
      (* for [a1,...,an] and [b1,...,bk], generate                          *)
      (* [f(a1,b1),f(a1,b2),...,f(an,bk-1),f(an,bk)].                       *)
      (*--------------------------------------------------------------------*)
      fun mapAllPairs f (ass,bs) = 
	 foldr 
	 (fn (a,cs) => foldr (fn (b,cs) => f(a,b)::cs) cs bs) 
	 nil ass

      (*--------------------------------------------------------------------*)
      (* find the first element x of l such that f x = SOME y, and return   *)
      (* f x. If there is no such x, return NONE.                           *)
      (*--------------------------------------------------------------------*)
      fun findAndMap _ nil = NONE 
	| findAndMap f (x::xs) = case f x of NONE => findAndMap f xs | y => y

      (*--------------------------------------------------------------------*)
      (* find the first element x of l such that f x = true, delete it from *)
      (* l, and return SOME x with the modified list. If there is no such x *)
      (* return (NONE,l).                                                   *)
      (*--------------------------------------------------------------------*)
      fun findAndDelete _ nil = (NONE,nil)
	| findAndDelete f (x::xs) = 
	 if f x then (SOME x,xs)
	 else let val (y,ys) = findAndDelete f xs in (y,x::ys) end  

      (*--------------------------------------------------------------------*)
      (* given a function that compares elements, merge two sorted lists.   *)
      (*--------------------------------------------------------------------*)
      fun merge comp (l1,l2) = 
	 let 
	    fun go (nil,l) = l 
	      | go (l,nil) = l
	      | go (l1 as (x1::r1),l2 as (x2::r2)) = 
	       case comp(x1,x2) 
		 of LESS => x1::go(r1,l2)
		  | EQUAL => go(l1,r2) 
		  | GREATER =>  x2::go(l1,r2)
	 in go(l1,l2)
	 end

      (*--------------------------------------------------------------------*)
      (* given a comparing function, compute the intersection of two        *)
      (* ordered lists.                                                     *)
      (*--------------------------------------------------------------------*)
      fun cap comp (l1,l2) = 
	 let 
	    fun go (nil,l) = nil
	      | go (l,nil) = nil
	      | go (l1 as (x1::r1),l2 as (x2::r2)) = 
	       case comp(x1,x2) 
		 of LESS => go(r1,l2)
		  | EQUAL => x1::go(r1,r2)
		  | GREATER =>  go(l1,r2)
	 in go(l1,l2)
	 end

      (*--------------------------------------------------------------------*)
      (* given a comparing function, compute the difference of two          *)
      (* ordered lists.                                                     *)
      (*--------------------------------------------------------------------*)
      fun diff comp (l1,l2) = 
	 let 
	    fun go (nil,l) = nil
	      | go (l,nil) = l
	      | go (l1 as (x1::r1),l2 as (x2::r2)) = 
	       case comp(x1,x2) 
		 of LESS => x1::go(r1,l2)
		  | EQUAL => go(r1,r2)
		  | GREATER => go(l1,r2)
	 in go(l1,l2)
	 end

      (*--------------------------------------------------------------------*)
      (* given a comparing function, find out whether an ordered list is    *)
      (* contained in an other ordered list.                                *)
      (*--------------------------------------------------------------------*)
      fun sub comp (l1,l2) = 
	 let 
	    fun go (nil,l) = true
	      | go (l,nil) = false
	      | go (l1 as (x1::r1),l2 as (x2::r2)) = 
	       case comp(x1,x2) 
		 of LESS => false
		  | EQUAL => go(r1,r2)
		  | GREATER => go(l1,r2)
	 in go(l1,l2)
	 end

      (*--------------------------------------------------------------------*)
      (* given a function that compares elements, insert an element into an *)
      (* ordered list.                                                      *)
      (*--------------------------------------------------------------------*)
      fun insert comp (x,l) = 
	 let 
	    fun go nil = [x]
	      | go (l as y::ys) = 
	       case comp(x,y) 
		 of LESS => x::l
		  | EQUAL => l
		  | GREATER => y::go ys
	 in go l
	 end

      (*--------------------------------------------------------------------*)
      (* given a function that compares elements, delete an element from    *)
      (* an ordered list.                                                   *)
      (*--------------------------------------------------------------------*)
      fun delete comp (x,l) = 
	 let 
	    fun go nil = [x]
	      | go (l as y::ys) = 
	       case comp(x,y) 
		 of LESS => l
		  | EQUAL => ys
		  | GREATER => y::go ys
	 in go l
	 end

      (*--------------------------------------------------------------------*)
      (* given a function that compares elements, insert an element into an *)
      (* ordered list.                                                      *)
      (*--------------------------------------------------------------------*)
      fun elem comp (x,l) = 
	 let 
	    fun go nil = false
	      | go (l as y::ys) = 
	       case comp(x,y) 
		 of LESS => false
		  | EQUAL => true
		  | GREATER => go ys
	 in go l
	 end

      (*--------------------------------------------------------------------*)
      (* merge-sort a list of elements comparable with the function in the  *)
      (* 1st argument. Preserve duplicate elements.                         *)
      (*--------------------------------------------------------------------*)
      fun sort _ nil = nil 
	| sort comp l = 
	 let fun mergeOne (x::y::l) = merge comp (x,y)::mergeOne l
	       | mergeOne l = l
	     fun mergeAll [l] = l
	       | mergeAll ls = mergeAll (mergeOne ls)
	     val singles = map (fn x => [x]) l
	 in 
	    mergeAll singles
	 end

   end


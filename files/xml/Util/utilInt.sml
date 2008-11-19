(*--------------------------------------------------------------------------*)
(* Structure: UtilInt                                                       *)
(*                                                                          *)
(* Depends on:                                                              *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   appInterval  : none                                                    *)
(*   insertInt    : none                                                    *)
(*   insertNewInt : none                                                    *)
(*   nextPowerTwo : none                                                    *)
(*--------------------------------------------------------------------------*)
signature UtilInt =
   sig
      val intervalList : (int * int) -> int list
      val appInterval  : (int -> unit) -> (int * int) -> unit
      val insertInt    : int * int list -> int list
      val insertNewInt : int * int list -> int list option
      val powerOfTwo   : int -> int
      val nextPowerTwo : int -> int
   end

structure UtilInt : UtilInt =
   struct
      (*--------------------------------------------------------------------*)
      (* generate the list [n,...,m]                                        *)
      (*--------------------------------------------------------------------*)
      fun intervalList(n,m) = if n>m then nil else n::intervalList(n+1,m)

      (*--------------------------------------------------------------------*)
      (* apply f to each number in [n...m]                                  *)
      (*--------------------------------------------------------------------*)
      fun appInterval f (n,m) =
	 let fun doit i = 
	    if i>m then () 
	    else let val _ = f i
		 in doit (i+1)
		 end
	 in doit n
	 end
      
      (*--------------------------------------------------------------------*)
      (* insert an integer into a sorted list without duplicates.           *)
      (*--------------------------------------------------------------------*)
      fun insertInt (x:int,l) = 
	 let fun go nil = [x]
	       | go (l as y::ys) = case Int.compare (x,y)
				     of LESS => x::l
				      | EQUAL => l 
				      | GREATER =>  y::go ys
	 in go l
	 end

      (*--------------------------------------------------------------------*)
      (* insert an integer into a sorted list if it is not yet in it.       *)
      (*--------------------------------------------------------------------*)
      fun insertNewInt (x:int,l) = 
	 let 
	    fun go nil = SOME [x]
	      | go (l as y::ys) = case Int.compare (x,y)
				     of LESS => SOME(x::l)
				      | EQUAL => NONE 
				      | GREATER => case go ys
						     of NONE => NONE
						      | SOME xys => SOME(y::xys)
	 in go l
	 end

      (*--------------------------------------------------------------------*)
      (* compute the power to the base of two.                              *)
      (*--------------------------------------------------------------------*)
      fun powerOfTwo n = 
	 if n=0 then 1
	 else if n mod 2=0 then let val x=powerOfTwo (n div 2) in x*x end
	      else let val x=powerOfTwo (n-1) in 2*x end
	   
      (*--------------------------------------------------------------------*)
      (* find the smallest p with 2^p >= n.                                 *)
      (*--------------------------------------------------------------------*)
      fun nextPowerTwo n = 
	 let fun doit (p,m) = 
	    if m>=n then p
	    else if m*m<2*n then doit (2*p,m*m)
		 else doit (1+p,2*m)
	 in doit (1,2)
	 end
   end

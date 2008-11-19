





(*--------------------------------------------------------------------------*)
(* Structure: DfaUtil                                                       *)
(*                                                                          *)
(* Depends on:                                                              *)
(*   DfaData                                                                *)
(*   UtilInt                                                                *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   boundsFollow : none                                                    *) 
(*   cmSymbols    : none                                                    *) 
(*   makeRow      : none                                                    *) 
(*   mergeFirst   : ConflictFirst                                           *) 
(*   mergeFollow  : ConflictFollow                                          *) 
(*--------------------------------------------------------------------------*)
signature DfaUtil =
   sig
      val mergeFirst   : bool -> DfaBase.First * DfaBase.First -> DfaBase.First 
      val mergeFollow  : bool -> DfaBase.Follow * DfaBase.Follow  -> DfaBase.Follow 
      val boundsFollow : DfaBase.Follow -> DfaBase.Sigma * DfaBase.Sigma 
      val cmSymbols    : DfaBase.ContentModel -> DfaBase.Sigma list
      val makeRow      : DfaBase.Follow * bool -> DfaBase.Row 
   end

structure DfaUtil : DfaUtil =
   struct
      open UtilInt DfaBase
	 
      (*--------------------------------------------------------------------*)
      (* merge two First sets, raise ConflictFirst at conflict: there may   *)
      (* not be two entries (q1,a) and (q2,a) in the same First set, if     *)
      (* nondet is false.                                                   *)
      (*--------------------------------------------------------------------*)
      fun mergeFirst nondet ll = 
	 let 
	    fun go_det (nil,l) = l 
	      | go_det (l,nil) = l
	      | go_det (l1 as (x1 as (q1,a1))::r1,l2 as (x2 as (q2,a2))::r2) = 
	       case Int.compare(a1,a2) 
		 of LESS => x1::go_det(r1,l2) 
		  | GREATER => x2::go_det(l1,r2)
		  | EQUAL => raise ConflictFirst(a1,q1,q2)

	    fun go_nondet (nil,l) = l 
	      | go_nondet (l,nil) = l
	      | go_nondet (l1 as (x1 as (q1,a1))::r1,l2 as (x2 as (q2,a2))::r2) = 
	       case Int.compare(a1,a2) 
		 of LESS => x1::go_nondet(r1,l2) 
		  | GREATER => x2::go_nondet(l1,r2)
		  | EQUAL => case Int.compare(q1,q2)
			       of LESS => x1::go_nondet(r1,l2)
				| GREATER => x2::go_nondet(l1,r2)
				| EQUAL => go_nondet(l1,r2)
	 in 
	    if nondet then go_nondet ll else go_det ll
	 end

      (*--------------------------------------------------------------------*)
      (* merge two Follow sets, raise ConflictFollow at conflict. there may *)
      (* not be two entries (q1,a) and (q2,a) with q1<>q2 in the same Follow*)
      (* set, if nondet is false. Note that, e.g. for (a+)+, Follow(a) =    *)
      (* Follow(a+) U First(a+), so duplicate occurrences of the same (q,a) *)
      (* are possible (as opposed to First).                                *)
      (*--------------------------------------------------------------------*)
      fun mergeFollow nondet ll = 
	 let 
	    fun go_det (nil,l) = l 
	      | go_det (l,nil) = l
	      | go_det (l1 as (x1 as (q1,a1))::r1,l2 as (x2 as (q2,a2))::r2) = 
	       case Int.compare(a1,a2) 
		 of LESS => x1::go_det(r1,l2) 
		  | GREATER => x2::go_det(l1,r2)
		  | EQUAL => if q1=q2 then go_det(l1,r2)
			     else raise ConflictFollow(a1,q1,q2)

	    fun go_nondet (nil,l) = l 
	      | go_nondet (l,nil) = l
	      | go_nondet (l1 as (x1 as (q1,a1))::r1,l2 as (x2 as (q2,a2))::r2) = 
	       case Int.compare(a1,a2) 
		 of LESS => x1::go_nondet(r1,l2) 
		  | GREATER => x2::go_nondet(l1,r2)
		  | EQUAL => case Int.compare(q1,q2)
			       of LESS => x1::go_nondet(r1,l2)
				| GREATER => x2::go_nondet(l1,r2)
				| EQUAL => go_nondet(l1,r2)
	 in 
	    if nondet then go_nondet ll else go_det ll
	 end

      (*--------------------------------------------------------------------*)
      (* what are the least and largest symbol occurring in a Follow set?   *)
      (*--------------------------------------------------------------------*)
      fun boundsFollow (nil:Follow) = (1,0)
	| boundsFollow [(q,a)]      = (a,a)
	| boundsFollow ((q,a)::xs)  = (a,#2(List.last xs))

      (*--------------------------------------------------------------------*)
      (* return the list of all symbols occurring in a content model.       *)
      (*--------------------------------------------------------------------*)
      fun cmSymbols cm = 
	 let 
	    fun do_cm(cm,yet) = 
	       case cm 
		 of CM_ELEM a  => insertInt(a,yet)
		  | CM_OPT cm  => do_cm(cm,yet)
		  | CM_REP cm  => do_cm(cm,yet)
		  | CM_PLUS cm => do_cm(cm,yet)
		  | CM_ALT cms => foldr do_cm yet cms
		  | CM_SEQ cms => foldr do_cm yet cms
	 in do_cm(cm,nil)
	 end

      (*--------------------------------------------------------------------*)
      (* given the follow set and the final flag, make a row in the dfa.    *)
      (*--------------------------------------------------------------------*)
      fun makeRow (flw,fin) = 
	 let 
	    val (lo,hi) = boundsFollow flw
	    val tab = Array.array(hi-lo+1,dfaError)
	    val _ = app (fn (q,a) => Array.update (tab,a-lo,q)) flw
	 in 
	    (lo,hi,Array.vector tab,fin)
	 end

   end

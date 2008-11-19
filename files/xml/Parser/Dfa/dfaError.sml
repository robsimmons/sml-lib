

(*--------------------------------------------------------------------------*)
(* Structure: DfaError                                                      *)
(*                                                                          *)
(* Note:                                                                    *)
(*   The function in this structure is for producing good error messages    *)
(*   for ambiguous content models. It numbers the nodes of a cm exactly     *)
(*   like passOne does, but counts the occurrences of symbol a in order to  *)
(*   indicate which are in conflict. It is only executed in case of error.  *)
(*                                                                          *)
(* Depends on:                                                              *)
(*  DfaData                                                                 *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   countOccs : none                                                       *)
(*--------------------------------------------------------------------------*)
signature DfaError =
   sig
      val countOccs : DfaBase.Sigma * DfaBase.State * DfaBase.State 
	 -> DfaBase.ContentModel -> DfaBase.Sigma * int * int
   end

structure DfaError : DfaError =
   struct
      open DfaBase

      fun countOccs (a,q1,q2) cm =
	 let 
	    val (q1,q2) = if q1>q2 then (q2,q1) else (q1,q2)

	    fun next a nil = (1,[(a,2)])
	      | next a ((b,n)::rest) =
	       if a=b then (n,(b,n+1)::rest)
	       else if a<b then (1,(a,2)::(b,n)::rest)
		    else let val (m,new) = next a rest
			 in (m,(b,n)::new)
			 end

	    fun insert a (q,yet,n1,n2) =
	       let val (n,new) = next a yet
	       in (q+1,new,if q=q1 then n else n1,if q=q2 then n else n2)
	       end

	    fun doit (cm,yet) =
	       case cm
		 of CM_ELEM a => insert a yet
		  | CM_OPT cmi => doit (cmi,yet)
		  | CM_REP cmi => doit (cmi,yet)
		  | CM_PLUS cmi => doit (cmi,yet)
		  | CM_ALT cmis => foldl doit yet cmis
		  | CM_SEQ cmis => foldl doit yet cmis

	    val (_,_,n1,n2) = doit (cm,(1,nil,0,0))
	 in 
	    (a,n1,n2)
	 end
   end

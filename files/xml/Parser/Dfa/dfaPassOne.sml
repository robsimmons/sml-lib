


(*--------------------------------------------------------------------------*)
(* Structure: DfaPassOne                                                    *)
(*                                                                          *)
(* Depends on:                                                              *)
(*  DfaData                                                                 *)
(*  DfaUtil                                                                 *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   passOne : ConflictFirst                                                *)
(*--------------------------------------------------------------------------*)
signature DfaPassOne = 
   sig
      val passOne : bool -> DfaBase.ContentModel -> DfaBase.CM
   end

structure DfaPassOne : DfaPassOne = 
   struct 
      open DfaBase DfaUtil

      (*--------------------------------------------------------------------*)
      (* Given a content model, number the leafs, compute Empty and First   *)
      (* for each node, and construct a corresponding CM annotated with     *)
      (* these informations.                                                *)
      (*                                                                    *)
      (* Numbering:                                                         *)
      (* The leafs are numbered in left-to-right, depth-first order,        *)
      (* starting with 1 (0 will be the start state of the DFA).            *)
      (*                                                                    *)
      (* Empty a = false                                                    *)
      (* Empty e? = Empty e* = true                                         *)
      (* Empty e+ = Empty e                                                 *)
      (* Empty e1|...|eN = Empty e1 \/ ... \/ Empty eN                      *)
      (* Empty e1,...,eN = Empty e1 /\ ... /\ Empty eN                      *)
      (*                                                                    *)
      (* First a = {q,a}, where q is the number of this leaf.               *)
      (* First e? = First e* = First e+ = First e                           *)
      (* First e1|...|eN = First e1 ++ ... ++ First eN                      *)
      (* First e1,...,eN = ++{First eI | Empty eJ=False forall j<i}         *)
      (*                                                                    *)
      (* F1++F2 = F1 U F2, if a2<>a1 forall (q1,a1) in F1, (q1,a1) in F1}   *)
      (*          error,   if exist (q1,a) in F1, (q2,a) in F2              *)
      (*                   then raise ConflictFirst(a,q1,q2)                *)
      (*--------------------------------------------------------------------*)
      fun passOne nondet cm = 
	 let 
	    fun und(a,b) = a andalso b
	    fun oder(a,b) = a orelse b
	       
	    fun op_fst_seq (fst,fsts,mt) = if mt then mergeFirst nondet (fst,fsts) else fst
	    fun op_fst_or  (fst,fsts,_)  = mergeFirst nondet (fst,fsts)
	       
	    fun do_cm cm q =
	       case cm
		 of CM_ELEM a  => (ELEM a,(q+1,false,[(q+1,a)]))
		  | CM_OPT cm  => let val cmi as (_,(q1,_,fst)) = do_cm cm q
				  in (OPT cmi,(q1,true,fst))
				  end
		  | CM_REP cm  => let val cmi as (_,(q1,_,fst)) = do_cm cm q
				  in (REP cmi,(q1,true,fst))
				  end
		  | CM_PLUS cm => let val cmi as (_,info1) = do_cm cm q
				  in (PLUS cmi,info1) 
				  end 
		  | CM_ALT cms => do_cms (ALT,false,oder,op_fst_or) cms q
		  | CM_SEQ cms => do_cms (SEQ,true,und,op_fst_seq) cms q
				  
	    and do_cms(con,null_mt,op_mt,op_fst) cms q = 
	       let 
		  fun doit [] q = ([],(q,null_mt,[]))
		    | doit (cm::cms) q =
		     let 
			val cmi as (_,(q1,mt1,fst1)) = do_cm cm q
			val (cmis,(q2,mt2,fst2)) = doit cms q1
		     in (cmi::cmis,(q2,op_mt(mt1,mt2),op_fst(fst1,fst2,mt1)))
		     end
		  val (cmis,info1) = doit cms q
	       in (con cmis,info1)
	       end
	    
	 in do_cm cm 0
	 end
   end

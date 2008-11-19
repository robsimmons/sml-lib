




(*--------------------------------------------------------------------------*)
(* Structure: DfaPassTwo                                                    *)
(*                                                                          *)
(* Depends on:                                                              *)
(*  DfaData                                                                 *)
(*  DfaUtil                                                                 *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   passTwo : ConflictFollow                                               *)
(*--------------------------------------------------------------------------*)
signature DfaPassTwo = 
   sig
      val passTwo: bool -> DfaBase.CM -> (DfaBase.Follow * bool) vector
   end

structure DfaPassTwo : DfaPassTwo = 
   struct 
      open DfaBase DfaUtil
	 
      (*--------------------------------------------------------------------*)
      (* Given a CM annotated with leaf numbers (states), Empty and First,  *)
      (* compute Follow and Fin foreach node, and generate the transition   *)
      (* row if node is a leaf. Follow and Fin are computed top-down:       *)
      (*                                                                    *)
      (* (Top-Level):							    *)
      (* Follow e = {}, Fin e = true                                        *)
      (* 								    *)
      (* (e=e1?):							    *)
      (* Follow e1 = Follow e, Fin e1 = Fin e				    *)
      (* 								    *)
      (* (e=e1*, e=e1+)							    *)
      (* Follow e1 = Follow e1 ++ First e1, Fin e1 = Fin e		    *)
      (*								    *)
      (* (e=e1|...|eN) = 						    *)
      (* Follow eI = Follow e, Fin eI = Fin e for i=0...n		    *)
      (*								    *)
      (* (e=e1,...,eN) = 						    *)
      (* Follow eN = Follow e, Fin eN = Fin e				    *)
      (* Follow eI = First eI+1,                if Empty eI+1 = false, i<n  *)
      (*             First eI+1 ++ Follow eI+1, if Empty eI+1 = true,  i<n  *)
      (* Fin eI = false,    if Empty eI+1 = false, i<n			    *)
      (*          Fin eI+1, if Empty eI+1 = true,  i<n			    *)
      (*                                                                    *)
      (* F1++F2 = F1 U F2, if a2<>a1 forall (q1,a1) in F1, (q1,a1) in F1}   *)
      (*          error,   if exist (q1,a) in F1, (q2,a) in F2              *)
      (*                   then raise ConflictFirst(a,q1,q2)                *)
      (*--------------------------------------------------------------------*)
      fun passTwo nondet (cmi as (_,(n,mt,fst))) =                                   
	 let 
	    val table = Array.array(n+1,(nil,false))
	       	    
	    val _ = Array.update(table,0,(fst,mt))

	    fun do_cm (ff as (flw,fin)) (cm,(q,mt,fst)) =
	       case cm 
		 of ELEM a   => Array.update(table,q,ff)
		  | OPT cmi  => do_cm ff cmi
		  | REP cmi  => do_cm (mergeFollow nondet (fst,flw),fin) cmi
		  | PLUS cmi => do_cm (mergeFollow nondet (fst,flw),fin) cmi
		  | ALT cmis => app (do_cm ff) cmis
		  | SEQ cmis => ignore (do_seq ff cmis)
	    and do_seq ff cmis = foldr 
	       (fn (cmi as (_,(_,mt,fst)),ff as (flw,fin)) 
		=> (do_cm ff cmi; 
		    if mt then (mergeFollow nondet (fst,flw),fin) else (fst,false))) 
	       ff cmis

	    val _ = do_cm (nil,true) cmi

	 in Array.vector table
	 end
    end

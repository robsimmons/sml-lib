










(*--------------------------------------------------------------------------*)
(* Structure: DfaPassThree                                                  *)
(*                                                                          *)
(* Depends on:                                                              *)
(*  DfaData                                                                 *)
(*  DfaUtil                                                                 *)
(*  IntSets                                                                 *)
(*  IntSetDict                                                              *)
(*  ParseOptions                                                            *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   passThree : TooLarge                                                   *)
(*--------------------------------------------------------------------------*)
signature DfaPassThree = 
   sig
      val passThree: bool -> (DfaBase.Follow * bool) vector -> DfaBase.Dfa
   end

functor DfaPassThree (structure DfaOptions : DfaOptions) : DfaPassThree = 
   struct 
      open 
	 IntSets IntSetDict DfaBase DfaOptions DfaUtil
	 
      (*--------------------------------------------------------------------*)
      (* do the subset construction.                                        *)
      (*--------------------------------------------------------------------*)
      (* given an automaton (Q,q0,F,delta), the subset automaton is         *)
      (* (Q',q0',F',delta') with:                                           *)
      (* - Q' = 2^Q                                                         *)
      (* - q0'= {q0}                                                        *)
      (* - F' = {S | S cap F <> empty}                                      *)
      (* - delta'(S,a) = {p | (q,a,p) in delta, q in S}                     *) 
      (*--------------------------------------------------------------------*)
      fun makeDet tab = 
	 let 
	    (* the new start state is the singleton of the old start state  *)
	    val sNull = singleIntSet 0

	    (* create a dictionary for the subsets, make sNull get index 0  *)
	    val tau = makeDict("",!O_DFA_INITIAL_WIDTH,(nil:Follow,false))
	    val pInitial = getIndex(tau,sNull) 

	    (* enter a new set state. raise DfaTooLarge if the new state    *)
	    (* would have a too large index                                 *)
	    fun makeState s = 
	       let val (max,i) = (!O_DFA_MAX_STATES,getIndex(tau,s))
	       in if max>i then i else raise DfaTooLarge max
	       end

	    (* compute the follow set for a set state from the follow sets  *)
	    (* of its members                                               *)
	    fun makeFollow NONE nil = nil
	      | makeFollow (SOME(s,a)) nil = [(makeState s,a)]
	      | makeFollow NONE ((q,a)::qas) = makeFollow (SOME(singleIntSet q,a)) qas
	      | makeFollow (SOME(s,a)) ((q,b)::qas) = 
	       if a=b then makeFollow (SOME(addIntSet(q,s),a)) qas
	       else (makeState s,a)::makeFollow (SOME(singleIntSet q,b)) qas

	    (* continue until all entries in the state dictionary are done -*)
	    fun doit i = 
	       if i>=usedIndices tau then i
	       else let val sI = getKey(tau,i)
			val lI = IntSet2List sI
			val ffs = map (fn j => Vector.sub(tab,j)) lI
			val (followJs,finI) = foldl 
			   (fn ((flwJ,finJ),(flw,fin)) => (mergeFollow true (flwJ,flw),
							   finJ orelse fin))
			   (nil,false) ffs
			val followI = makeFollow NONE followJs
			val _ = setByIndex(tau,i,(followI,finI))
		    in doit (i+1)
		    end

	    val size = doit 0
	 in (* finally create a vector holding the new follow/fin pairs     *)
	    Vector.tabulate (size,fn i => getByIndex(tau,i))
	 end
	 
      (*--------------------------------------------------------------------*)
      (* given a vector of Follow and boolean final condition, make a dfa   *)
      (* out of it. if the first arg is true, then the content model was    *)
      (* ambiguous; in this case we must first apply a subset construction  *)
      (* in order to obtain a deterministic finite machine.                 *)
      (*--------------------------------------------------------------------*)
      fun passThree nondet tab =  
	 let 
	    val det = if nondet then makeDet tab else tab
	 in Vector.map makeRow det
	 end
   end











(*--------------------------------------------------------------------------*)
(* Structure: Dfa                                                           *)
(*                                                                          *)
(* Depends on:                                                              *)
(*   DfaData                                                                *)
(*   DfaError                                                               *)
(*   DfaPassOne                                                             *)
(*   DfaPassTwo                                                             *)
(*   DfaString                                                              *)
(*   DfaUtil                                                                *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   ContentModel2String : none                                             *)
(*   dfaFinal            : none                                             *)
(*   dfaTrans            : none                                             *)
(*   makeAmbiguous       : DfaTooLarge                                      *)
(*   makeChoiceDfa       : none                                             *)
(*   makeDfa             : Ambiguous                                        *)
(*   Dfa2String          : none                                             *)
(*--------------------------------------------------------------------------*)
signature Dfa = 
   sig
      eqtype DfaState
	 
      val dfaError   : DfaState
      val dfaInitial : DfaState

      exception DfaTooLarge of int
      exception Ambiguous of int * int * int

      val emptyDfa : DfaData.Dfa

      val makeDfa       : DfaData.ContentModel -> DfaData.Dfa
      val makeAmbiguous : DfaData.ContentModel -> DfaData.Dfa
      val makeChoiceDfa : DfaData.ContentModel -> DfaData.Dfa

      val dfaFinal : DfaData.Dfa * DfaState -> bool
      val dfaTrans : DfaData.Dfa * DfaState * int -> DfaState
   end

functor Dfa (structure DfaOptions : DfaOptions) : Dfa =
   struct	
      structure DfaPassThree = DfaPassThree (structure DfaOptions = DfaOptions) 

      open 
	 DfaBase DfaError DfaPassOne DfaPassTwo DfaString DfaUtil

      type DfaState = State

      (*--------------------------------------------------------------------*)
      (* Create a dfa for the content model (a1|...|aN)*, where a1,...,aN   *)
      (* are the symbols occurring in the input dfa.                        *)
      (*--------------------------------------------------------------------*)
      fun makeChoiceDfa cm =
	 let 
	    val syms = cmSymbols cm
	    val flw = map (fn a => (dfaInitial,a)) syms
	 in 
	    Vector.fromList [makeRow(flw,true)]
	 end

      (*--------------------------------------------------------------------*)
      (* create a dfa for an ambiguous content model. Raise DfaTooLarge if  *)
      (* the subset construction yields too many states.                    *)
      (*--------------------------------------------------------------------*)
      fun makeAmbiguous cm = 
	 let 
	    val cmi = DfaPassOne.passOne true cm
	    val tab = DfaPassTwo.passTwo true cmi
	    val dfa = DfaPassThree.passThree true tab
	 in dfa
	 end
	    
      (*--------------------------------------------------------------------*)
      (* generate a dfa for a content model. Raise Ambiguous if the content *)
      (* model is ambiguous.                                                *)
      (*--------------------------------------------------------------------*)
      fun makeDfa cm = 
	 let 
	    val cmi = DfaPassOne.passOne false cm
	    val tab = DfaPassTwo.passTwo false cmi
	    val dfa = DfaPassThree.passThree false tab
	 in dfa
	 end
      handle ConflictFirst aqq => raise Ambiguous (countOccs aqq cm)
	   | ConflictFollow aqq => raise Ambiguous (countOccs aqq cm)
      
      (*--------------------------------------------------------------------*)
      (* make one transitions in the dfa.                                   *)
      (*--------------------------------------------------------------------*)
      fun dfaTrans(tab,q,a) = 
	 if q<0 then dfaDontCare 
	 else let val (lo,hi,tab,_) = Vector.sub(tab,q)
	      in if a>=lo andalso a<=hi then Vector.sub(tab,a-lo) else dfaError
	      end

      (*--------------------------------------------------------------------*)
      (* check whether a dfa's state is an accepting state.                 *)
      (*--------------------------------------------------------------------*)
      fun dfaFinal (tab,q) = 
	 q<0 orelse #4(Vector.sub(tab,q):Row)
   end

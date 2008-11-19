


(*--------------------------------------------------------------------------*)
(* Structure: DfaData                                                       *)
(*                                                                          *)
(* Depends on:                                                              *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   boundsFollow : none                                                    *) 
(*   mergeFirst   : ConflictFirst                                           *) 
(*   mergeFollow  : ConflictFollow                                          *) 
(*--------------------------------------------------------------------------*)
signature DfaData = 
   sig
      type Dfa

      datatype ContentModel =
	 CM_ELEM of int
       | CM_OPT of ContentModel
       | CM_REP of ContentModel
       | CM_PLUS of ContentModel
       | CM_ALT of ContentModel list 
       | CM_SEQ of ContentModel list
   end

structure DfaBase =
   struct
      (*--- visible to the parser ---*)
      datatype ContentModel =
	 CM_ELEM of int
       | CM_OPT of ContentModel
       | CM_REP of ContentModel
       | CM_PLUS of ContentModel
       | CM_ALT of ContentModel list 
       | CM_SEQ of ContentModel list

      type Sigma = int
      type State = int

      val dfaDontCare = ~2 
      val dfaError    = ~1 
      val dfaInitial  = 0 

      exception DfaTooLarge of int
      exception Ambiguous of Sigma * int * int
      exception ConflictFirst of Sigma * State * State
      exception ConflictFollow of Sigma * State * State

      type Empty  = bool
      type First  = (State * Sigma) list
      type Follow = First 
	 
      type Info = State * Empty * First

      datatype CM' =
	 ELEM of Sigma
       | OPT of CM
       | REP of CM
       | PLUS of CM
       | ALT of CM list
       | SEQ of CM list
      withtype CM = CM' * Info

      type Row = Sigma * Sigma * State vector * bool
      val nullRow : Row = (1,0,Vector.fromList nil,false)
	 
      type Dfa = Row vector

      val emptyDfa : Dfa = Vector.fromList [(1,0,Vector.fromList nil,true)] 
   end

structure DfaData = DfaBase : DfaData

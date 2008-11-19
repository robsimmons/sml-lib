









signature ParseMisc =
   sig
      (*----------------------------------------------------------------------
      include ParseBase

      val parseName    : UniChar.Char * AppData * State 
	 -> UniChar.Data * (UniChar.Char * AppData * State)
      val parseNmtoken : UniChar.Char * AppData * State 
         -> UniChar.Data * (UniChar.Char * AppData * State)
      val parseNameLit : UniChar.Data -> UniChar.Char * AppData * State
         -> UniChar.Data * UniChar.Data * (UniChar.Char * AppData * State)
      val parseEntName : UniChar.Data * UniChar.Data -> UniChar.Char * AppData * State 
         -> bool * UniChar.Data * UniChar.Data * (UniChar.Char * AppData * State)
      ----------------------------------------------------------------------*)
      include ParseNames

      val skipS    : UniChar.Char * AppData * State -> UniChar.Char * AppData * State
      val skipSopt : UniChar.Char * AppData * State -> UniChar.Char * AppData * State
      val skipSmay : UniChar.Char * AppData * State -> bool * (UniChar.Char * AppData * State)

      val parseSopt : UniChar.Data -> UniChar.Char * AppData * State 
	 -> UniChar.Data * (UniChar.Char * AppData * State)
      val parseSmay : UniChar.Data -> UniChar.Char * AppData * State 
	 -> bool * (UniChar.Data * (UniChar.Char * AppData * State))
	 
      val skipEq : UniChar.Char * AppData * State 
	 -> UniChar.Char * AppData * State
      val parseEq : UniChar.Char * AppData * State 
	 -> UniChar.Data * (UniChar.Char * AppData * State)

      val parseComment   : Errors.Position -> AppData * State -> (UniChar.Char * AppData * State)
      val parseProcInstr : Errors.Position -> AppData * State -> (UniChar.Char * AppData * State)
   end

(*--------------------------------------------------------------------------*)
(* Structure: ParseMisc                                                     *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   skipS          : none                                                  *)
(*   skipSopt       : none                                                  *)
(*   skipSmay       : none                                                  *)
(*   skipEq         : SyntaxError                                           *)
(*   skipComment    : none                                                  *)
(*   parseComment   : none                                                  *)
(*   parseProcInstr : none                                                  *)
(*--------------------------------------------------------------------------*)
functor ParseMisc (structure ParseBase : ParseBase) 
   : ParseMisc =
struct
   structure ParseNames = ParseNames (structure ParseBase = ParseBase)

   open 
      UniChar Errors ParseNames

   (*--------------------------------------------------------------------*)
   (* parse a sequence of white space. 2.3:                              *)
   (*                                                                    *)
   (*   [3] S ::= (#x20 | #x9 | #xD | #xA)+                              *)
   (*--------------------------------------------------------------------*)
   (* parse optional white space.                                        *)
   (*--------------------------------------------------------------------*)
   (* Return type: Char * AppData * State                               *)
   (*--------------------------------------------------------------------*)
   fun skipSopt (c,a,q) =
      case c 
	of 0wx09 => skipSopt (getChar (a,q))
	 | 0wx0A => skipSopt (getChar (a,q))
	 | 0wx20 => skipSopt (getChar (a,q))
	 | _ => (c,a,q)
   fun parseSopt cs (c,a,q) =
      case c 
	of 0wx09 => parseSopt (c::cs) (getChar (a,q))
	 | 0wx0A => parseSopt (c::cs) (getChar (a,q))
	 | 0wx20 => parseSopt (c::cs) (getChar (a,q))
	 | _ => (cs,(c,a,q))
   (*--------------------------------------------------------------------*)
   (* parse optional white space.                                        *)
   (*--------------------------------------------------------------------*)
   (* Return type: bool * (Char * AppData * State)                      *)
   (* the bool indicates whether white space was found or not.           *)
   (*--------------------------------------------------------------------*)
   fun skipSmay (c,a,q) =
      case c 
	of 0wx09 => (true,skipSopt (getChar (a,q)))
	 | 0wx0A => (true,skipSopt (getChar (a,q)))
	 | 0wx20 => (true,skipSopt (getChar (a,q)))
	 | _ => (false,(c,a,q))
   fun parseSmay cs (c,a,q) =
      case c 
	of 0wx09 => (true,parseSopt (c::cs) (getChar (a,q)))
	 | 0wx0A => (true,parseSopt (c::cs) (getChar (a,q)))
	 | 0wx20 => (true,parseSopt (c::cs) (getChar (a,q)))
	 | _ => (false,(cs,(c,a,q)))
   (*--------------------------------------------------------------------*)
   (* parse required white space.                                        *)
   (*--------------------------------------------------------------------*)
   (* print an error if no white space character is found.               *)
   (*--------------------------------------------------------------------*)
   (* Return type: Char * AppData * State                                *)
   (*--------------------------------------------------------------------*)
   fun skipS (c,a,q) = 
      case c
	of 0wx09 => skipSopt (getChar (a,q))
	 | 0wx0A => skipSopt (getChar (a,q))
	 | 0wx20 => skipSopt (getChar (a,q))
	 | _ => (c,hookError(a,(getPos q,ERR_MISSING_WHITE)),q)

   (*--------------------------------------------------------------------*)
   (* parse a "=" together with surrounding white space. Cf. 28:         *)
   (*                                                                    *)
   (*   [25] Eq ::= S? '=' S?                                            *)
   (*--------------------------------------------------------------------*)
   (* Raises:                                                            *)
   (*   SyntaxError if no "=" is found.                                  *)
   (*--------------------------------------------------------------------*)
   (* Return type: Char * AppData * State                               *)
   (*--------------------------------------------------------------------*)
   fun skipEq caq = 
      let val (c1,a1,q1) = skipSopt caq
      in if c1=0wx3D then skipSopt (getChar (a1,q1))
	 else let val a2 = hookError(a1,(getPos q1,ERR_EXPECTED(expEq,[c1])))
	      in raise SyntaxError(c1,a2,q1)
	      end
      end
   fun parseEq caq = 
      let val (cs1,(c1,a1,q1)) = parseSopt nil caq
      in if c1=0wx3D 
	    then let val (cs2,caq2)= parseSopt (c1::cs1) (getChar (a1,q1))
		 in (rev cs2,caq2)
		 end
	 else let val a2 = hookError(a1,(getPos q1,ERR_EXPECTED(expEq,[c1])))
	      in raise SyntaxError(c1,a2,q1)
	      end
      end

   (*--------------------------------------------------------------------*)
   (* parse a comment, the initial "<--" already consumed. cf. 2.5:      *)
   (*                                                                    *)
   (*   They are not part of the document's character data; an XML       *)
   (*   processor may, but need not, make it possible for an application *)
   (*   to retrieve the text of comments. For compatibility, the string  *)
   (*   "--" (double-hyphen) must not occur within comments.             *)
   (*                                                                    *)
   (*   [15] Comment ::= '<!--' (  (Char - '-')                          *) 
   (*                            | ('-' (Char - '-')))* '-->'            *)
   (*--------------------------------------------------------------------*)
   (* print an error and end the comment if an entity end is found.      *)
   (* print an error if the comment contains "--".                       *)
   (*--------------------------------------------------------------------*)
   (* add the comment to the user data.                                  *)
   (*--------------------------------------------------------------------*)
   (* Return type: Char * AppData * State                               *)
   (*--------------------------------------------------------------------*)
   fun parseComment startPos aq =
      let 
	 fun check_end yet (a0,q0) =
	    let val (c,a,q) = getChar (a0,q0)
	    in if c=0wx2D (* #"-" *)
		  then let val (c1,a1,q1) = getChar (a,q)
		       in if c1=0wx3E (* #">" *) 
			     then let val cs = Data2Vector(rev yet)
				      val a2 = hookComment(a1,((startPos,getPos q1),cs))
				  in getChar(a2,q1)
				  end
			  else let val a2 = if not (!O_COMPATIBILITY) then a1 
					    else hookError(a1,(getPos q0,ERR_FORBIDDEN_HERE
							       (IT_DATA [c,c],LOC_COMMENT)))
			       in doit (c::c::yet) (c1,a2,q1)
			       end
		       end
	       else doit (0wx2D::yet) (c,a,q)
	    end
	  and doit yet (c,a,q) = 
	     if c=0wx2D (* #"-" *) then check_end yet (a,q)
	     else if c<>0wx00 then doit (c::yet) (getChar (a,q))
		  else let val err = ERR_ENDED_BY_EE LOC_COMMENT
			   val a1 = hookError(a,(getPos q,err))
			   val cs = Data2Vector(rev yet)
			   val a2 = hookComment(a1,((startPos,getPos q),cs))
		       in (c,a2,q)
		       end
      in doit nil (getChar aq) 
      end

   (*--------------------------------------------------------------------*)
   (* check whether a name matches "xml", disregarding case, cf. 2.6:    *)
   (*                                                                    *)
   (*   [17] PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))   *)
   (*                                                                    *)
   (*   The target names "XML", "xml", and so on are reserved for        *)
   (*   standardization in this or future versions of this specification.*)
   (*--------------------------------------------------------------------*)
   (* print an error if it does match.                                   *)
   (*--------------------------------------------------------------------*)
   (* Return type: AppData                                              *)
   (*--------------------------------------------------------------------*)
   fun checkPiTarget (a,q) name = 
      case name
	of [c1,c2,c3] => if ((c1=0wx58 orelse c1=0wx78) andalso 
			     (c2=0wx4D orelse c2=0wx6D) andalso
			     (c3=0wx4C orelse c3=0wx6C)) 
			    then hookError(a,(getPos q,ERR_RESERVED(name,IT_TARGET)))
			 else a
	 | _ => a
   (*--------------------------------------------------------------------*)
   (* parse a processing instruction, the initial "<?" and target        *)
   (* already consumed. cf. 2.5:                                         *)
   (*                                                                    *)
   (*   [16] PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char* )))? '?>'*)
   (*                                                                    *)
   (* The first arg consists of the target and the (reversed) list of    *)
   (* leading characters of the text that have been looked ahead.        *)
   (*--------------------------------------------------------------------*)
   (* print an error and end the proc. instr. if an entity end is found. *)
   (*--------------------------------------------------------------------*)
   (* add the processing instruction to the user data.                   *)
   (*--------------------------------------------------------------------*)
   (* Return type: Char * AppData * State                               *)
   (*--------------------------------------------------------------------*)
   fun parseProcInstr' (startPos,target,txtPos,yetText) caq =
      let 
	 fun doit text (c1,a1,q1) = 
	    case c1 
	      of 0wx00 => let val a2 = hookError(a1,(getPos q1,ERR_ENDED_BY_EE LOC_PROC))
			  in (text,getPos q1,(c1,a2,q1))
			  end
	       | 0wx3F => (* #"?" *)
			  let val (c2,a2,q2) = getChar (a1,q1)
			  in case c2
			       of 0wx3E => (* #">" *) (text,getPos q2,getChar(a2,q2))
				| _ => doit (c1::text) (c2,a2,q2)
			  end
	       | _ => doit (c1::text) (getChar (a1,q1))
		    
	 val (cs,endPos,(c2,a2,q2)) = doit yetText caq  
	 val text = Data2Vector(rev cs)
	 val a3 = hookProcInst(a2,((startPos,endPos),target,txtPos,text))
      in 
	 (c2,a3,q2)
      end
   (*--------------------------------------------------------------------*)
   (* parse a processing instruction, the initial "<?" already read.     *)
   (*                                                                    *)
   (*   [16] PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char* )))? '?>'*)
   (*--------------------------------------------------------------------*)
   (* print an error and end the proc. instr. if an entity end is found. *)
   (* print an error if no target name is found.                         *)
   (* print an error if no whitespace follows the target.                *)
   (*--------------------------------------------------------------------*)
   (* add the processing instruction to the user data.                   *)
   (*--------------------------------------------------------------------*)
   (* Return type: Char * AppData * State                               *)
   (*--------------------------------------------------------------------*)
   fun parseProcInstr startPos (a,q) =
      let 
	 (* NotFound is handled after the 'in .. end' *)
	 val (target,(c1,a1,q1)) = parseName (getChar(a,q))
	 val a1 = checkPiTarget (a1,q) target
      in 
	 case c1 
	   of 0wx00 => 
	      let 
		 val a2 = hookError(a1,(getPos q1,ERR_ENDED_BY_EE LOC_PROC))
		 val a3 = hookProcInst(a2,((startPos,getPos q1),target,getPos q1,nullVector))
	      in (c1,a3,q1) 
	      end
	    | 0wx3F => (* #"?" *)
	      let val (c2,a2,q2) = getChar (a1,q1)
	      in case c2
		   of 0wx3E => (* #">" *) 
                      let val a3 = hookProcInst(a2,((startPos,getPos q2),target,
                                                    getPos q1,nullVector))
                      in getChar (a3,q2)
                      end
		    | _ => let val a3 = hookError(a2,(getPos q1,ERR_MISSING_WHITE))
			   in parseProcInstr' (startPos,target,getPos q1,[c1]) (c2,a3,q2)
			   end
	      end
	    | _ => let val (hadS,(c2,a2,q2)) = skipSmay (c1,a1,q1) 
		       val a3 = if hadS then a2 
				else hookError(a2,(getPos q2,ERR_MISSING_WHITE))
		   in parseProcInstr' (startPos,target,getPos q2,nil) (c2,a3,q2)
		   end
      end
   handle NotFound(c,a,q) => 
      let val a1 = hookError(a,(getPos q,ERR_EXPECTED(expATarget,[c]))) 
      in parseProcInstr' (startPos,nullData,getPos q,nil) (c,a1,q)
      end
end

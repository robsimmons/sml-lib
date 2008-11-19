signature ParseTags = 
   sig
      (*----------------------------------------------------------------------
      include ParseBase

      val parseName    : UniChar.Char * AppData * State 
	 -> UniChar.Data * (UniChar.Char * AppData * State)
      val parseNmtoken : UniChar.Char * AppData * State 
         -> UniChar.Data * (UniChar.Char * AppData * State)

      val parseComment   : Errors.Position -> AppData * State -> (UniChar.Char * AppData * State)
      val parseProcInstr : Errors.Position -> AppData * State -> (UniChar.Char * AppData * State)
      val skipS    : UniChar.Char * AppData * State -> UniChar.Char * AppData * State
      val skipSopt : UniChar.Char * AppData * State -> UniChar.Char * AppData * State
      val skipSmay : UniChar.Char * AppData * State -> bool * (UniChar.Char * AppData * State)

      val openExtern   : int * Uri.Uri -> AppData * State 
	 -> Encoding.Encoding * HookData.TextDecl option * (UniChar.Char * AppData * State)
      val openDocument : Uri.Uri option -> AppData 
	 -> Encoding.Encoding * HookData.XmlDecl option * (UniChar.Char * AppData * State)
      val openSubset   : Uri.Uri -> AppData 
	 -> Encoding.Encoding * HookData.TextDecl option * (UniChar.Char * AppData * State)

      val skipCharRef     : AppData * State -> (UniChar.Char *  AppData * State)
      val skipReference   : UniChar.Char * AppData * State -> (UniChar.Char *  AppData * State)
      val parseGenRef     : Dtd -> UniChar.Char * AppData * State 
	 -> (int * Base.GenEntity) * (AppData * State)
      val parseParRef     : Dtd -> UniChar.Char * AppData * State 
	 -> (int * Base.ParEntity) * (AppData * State)
      val parseCharRefLit : UniChar.Data -> AppData * State 
	 -> UniChar.Data * (UniChar.Char * AppData * State)
      val skipPS    : Dtd -> UniChar.Char * AppData * State 
	 -> UniChar.Char * AppData * State
      val skipPSopt : Dtd -> UniChar.Char * AppData * State 
	 -> UniChar.Char * AppData * State
      val skipPSmay : Dtd -> UniChar.Char * AppData * State 
	 -> bool * (UniChar.Char * AppData * State)
      val skipPSdec : Dtd -> UniChar.Char * AppData * State 
	 -> bool * (UniChar.Char * AppData * State)

      val parseSystemLiteral : UniChar.Char * AppData * State 
	 -> Uri.Uri * UniChar.Char * (UniChar.Char * AppData * State)
      val parsePubidLiteral  : UniChar.Char * AppData * State 
	 -> string * UniChar.Char * (UniChar.Char * AppData * State)
      val parseAttValue : Dtd -> UniChar.Char * AppData * State 
	 -> UniChar.Vector * UniChar.Data * (UniChar.Char * AppData * State)
      val parseEntityValue : Dtd -> (UniChar.Vector * UniChar.Vector -> 'a) 
	 -> UniChar.Char * AppData * State 
	 -> 'a * (UniChar.Char * AppData * State)
      ----------------------------------------------------------------------*)
      include ParseLiterals

      val skipTag   : Errors.Location -> AppData * State -> (UniChar.Char * AppData * State)

      val parseETag : Dtd -> AppData * State 
	 -> int * UniChar.Data * Errors.Position * (UniChar.Char * AppData * State)
      val parseSTag : Dtd -> Errors.Position -> UniChar.Char * AppData * State 
	 -> (HookData.StartTagInfo * Base.ElemInfo) * (UniChar.Char * AppData * State)
   end

(*--------------------------------------------------------------------------*)
(* Structure: ParseTags                                                     *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   skipTag   : none                                                       *)
(*   parseETag : SyntaxState                                                *)
(*   parseSTag : SyntaxState                                                *)
(*--------------------------------------------------------------------------*)
functor ParseTags (structure ParseBase : ParseBase) 
   : ParseTags =
struct
   structure ParseLiterals = ParseLiterals (structure ParseBase = ParseBase)

   open
      UtilList 
      Base Errors UniClasses
      ParseLiterals 

   (*--------------------------------------------------------------------*)
   (* parse an end-tag, the "</" already read. 3.1:                      *)
   (*                                                                    *) 
   (*   [42] ETag ::= '</' Name S? '>'                                   *)
   (*                                                                    *)
   (* and 3. states:                                                     *)
   (*                                                                    *)
   (* Validity Constraint: Element Valid                                 *)
   (* An element is valid if there is a declaration matching elementdecl *)
   (* where the Name matches the element type, and ...                   *)
   (*                                                                    *)
   (* print an error, recover and raise SyntaxState if no name is found. *)
   (* print an error and recover if no ">" is found.                     *)
   (* print an error if the element is not declared.                     *)
   (*                                                                    *) 
   (* return the index of the element, and the next char and state.      *)
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *) 
   (*--------------------------------------------------------------------*)
   fun parseETag dtd aq = 
      let
	 val caq0 as (_,_,q0) = getChar aq
	 val (elem,(c1,a1,q1)) = parseName caq0 
	    handle NotFound (c,a,q) => let val err = expectedOrEnded (expAName,LOC_ETAG) c
					   val a1 = hookError(a,(getPos q,err))
					   val caq1 = recoverETag (c,a1,q)
				       in raise SyntaxError caq1
				       end
	 val idx = Element2Index dtd elem
	 val elemInfo as {decl,...} = getElement dtd idx
	 val a1' = if isSome decl then a1 
		   else let val a2 = if not (!O_VALIDATE andalso hasDtd dtd) then a1 
				     else let val err = ERR_UNDECLARED(IT_ELEM,elem,LOC_ETAG)
                                              val a1' = hookError(a1,(getPos q0,err))
                                              val _ = if not (!O_ERROR_MINIMIZE) then ()
                                                      else ignore (handleUndeclElement dtd idx)
					  in a1'
					  end
			in checkElemName (a2,q0) elem
			end

	 val (cs,(c2,a2,q2)) = parseSopt nil (c1,a1',q1)
	 val space = rev cs
      in 
	 if c2=0wx3E (* #">" *) then (idx,space,getPos q2,getChar(a2,q2))
	 else let val err = expectedOrEnded (expGt,LOC_ETAG) c2
		  val a3 = hookError(a2,(getPos q2,err))
		  val caq3 = recoverETag(c2,a3,q2)
	      in (idx,space,getPos q2,caq3)
	      end
      end

   (*--------------------------------------------------------------------*)
   (* parse a start-tag or an empty-element-tag, the "<" already read.   *)
   (* 3.1:                                                               *)
   (*                                                                    *) 
   (*   [40]      STag ::= '<' Name (S Attribute)* S? '>'                *)
   (*                                           [ WFC: Unique Att Spec ] *)
   (*   [41] Attribute ::= Name Eq AttValue [ VC: Attribute Value Type ] *)
   (*                                                                    *)
   (*   Well-Formedness Constraint: Unique Att Spec                      *)
   (*   No attribute name may appear more than once in the same          *)
   (*   start-tag or empty-element tag.                                  *)
   (*                                                                    *)
   (*   Validity Constraint: Attribute Value Type                        *)
   (*   The attribute must have been declared; the value must be of the  *)
   (*   type declared for it.                                            *)
   (*                                                                    *)
   (*   [44] EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'            *)
   (*                                          [  WFC: Unique Att Spec ] *)
   (*                                                                    *)
   (* and 3. states:                                                     *)
   (*                                                                    *)
   (* Validity Constraint: Element Valid                                 *)
   (* An element is valid if there is a declaration matching elementdecl *)
   (* where the Name matches the element type, and ...                   *)
   (*                                                                    *)
   (* catch entity end exceptions in subfunctions by printing an error   *)
   (* and re-raising the exception.                                      *)
   (*                                                                    *)
   (* print an error, recover and raise SyntaxState if no element name   *)
   (* is found.                                                          *)
   (* print an error and recover if no ">" or "/>" is found.             *)
   (* print an error and continue if no "=" is found after an att name.  *)
   (* print an error and recover if no literal is found after the "=".   *)
   (* print an error if white space is missing.                          *)
   (* print an error if the element is not declared.                     *)
   (* print an error and ignore the attribute if an attribute is         *)
   (* specified twice.                                                   *)
   (* print an error if an attribute is not declared.                    *)
   (*                                                                    *) 
   (* return the index of the element, its ElemInfo, the list of         *)
   (* AttSpecs (specified and omitted atts) and a boolean whether it was *)
   (* an empty-element-tag, together with the next char and state.       *)
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *) 
   (*--------------------------------------------------------------------*)
   fun parseSTag dtd startPos (caq as (_,_,q)) = 
      let 
	 val (elem,(c1,a1,q1)) = parseName caq 
	    handle NotFound (c,a,q) => let val err = expectedOrEnded (expAName,LOC_STAG) c
					   val a1 = hookError(a,(getPos q,err))
					   val (_,caq1) = recoverSTag (c,a1,q)
				       in raise SyntaxError (c,a1,q)
				       end
	 val eidx = Element2Index dtd elem
	 val elemInfo as {atts,decl,...} = getElement dtd eidx
	 val defs = case atts
		      of NONE => nil
		       | SOME (defs,_) => defs
	 val (a1',elemInfo) = 
            if isSome decl then (a1,elemInfo)
            else 
               let val (a2,newInfo) = 
                  if not (!O_VALIDATE andalso hasDtd dtd) then (a1,elemInfo) 
                  else let val err = ERR_UNDECLARED(IT_ELEM,elem,LOC_STAG)
                           val a1' = hookError(a1,(getPos q,err))
                           val newInfo = if not (!O_ERROR_MINIMIZE) then elemInfo
                                         else handleUndeclElement dtd eidx
                       in (a1',newInfo)
                       end
               in (checkElemName (a2,q) elem,newInfo)
               end
            
	 val hscaq2 = parseSmay nil (c1,a1',q1)
	       
	 (*--------------------------------------------------------------*)	       
	 (* yet are the indices of attributes encountered yet, old are   *)
	 (* the valid attributes specified yet, and todo are the defs of *)
	 (* attributes yet to be specified. hadS indicates whether white *)
	 (* space preceded.                                              *)
	 (*--------------------------------------------------------------*)	       
	 fun doit (yet,old,todo) (hadS,(sp,(c,a,q))) = 
	    case c 
	      of 0wx3E (* #">" *) => (old,todo,sp,false,q,getChar(a,q))
	       | 0wx2F (* #"/" *) => 
		 let val (c1,a1,q1) = getChar(a,q) 
		 in if c1=0wx3E (* #">" *) then (old,todo,sp,true,q1,getChar(a1,q1))
		    else let val err = expectedOrEnded (expGt,LOC_STAG) c1
			     val a2 = hookError(a1,(getPos q1,err))
			     val (mt,caq2) = recoverSTag (c1,a2,q1)
			 in (old,todo,sp,mt,q,caq2)
			 end
		 end
	       | _ => 
		 if not (isNms c) 
		    then let val err = expectedOrEnded (expAttSTagEnd,LOC_STAG) c
			     val a1 = hookError(a,(getPos q,err))
			     val (mt,caq1) = recoverSTag (c,a1,q)
			 in (old,todo,sp,mt,q,caq1)
			 end
		 else 
		    let(* first parse the name of the attribute          *)  
		       val (att,(c1,a1,q1)) = parseName (c,a,q)
		       val a2 = if hadS then a1 
				else hookError(a1,(getPos q,ERR_MISSING_WHITE))
			     
		       (* now get its index, check whether it already    *)
		       (* occurred and get its definition.               *)
		       val aidx = AttNot2Index dtd att
		       val (hadIt,a3) = 
			  if member aidx yet 
			     then (true,hookError(a2,(getPos q,ERR_MULT_ATT_SPEC att)))
			  else (false,a2)

		       val (def,rest) = findAndDelete (fn (i,_,_,_) => i=aidx) todo
		       val a4 = if isSome def orelse hadIt then a3
				else handleUndeclAtt dtd (a3,q) (aidx,att,eidx,elem) 

		       (* consume the " = ", ignore errors               *)
		       val (eq,caq5 as (_,_,q5)) = parseEq (c1,a4,q1) 
			  handle SyntaxError caq => ([0wx3D],caq)
			     
		       (* now parse the attribute value                  *)
		       val (literal,value,(c6,a6,q6)) = parseAttValue dtd caq5

		       (* possibly make a new AttSpec                    *)
		       val space = rev sp
		       val (new,a7) = 
			  if hadIt then (old,a6) 
			  else case def 
				 of NONE => 
				    if !O_VALIDATE andalso hasDtd dtd then (old,a6)
				    else (let val (attVal,a7) = checkAttValue dtd (a6,q5) 
					     (defaultAttDef aidx,literal,value)
					  in ((aidx,attVal,SOME(space,eq))::old,a7)
					  end
					     handle AttValue a => (old,a))
				  | SOME ad => 
				       let val (attVal,a7) = checkAttValue dtd (a6,q5) 
					  (ad,literal,value)
				       in ((aidx,attVal,SOME(space,eq))::old,a7)
				       end
				    handle AttValue a => (old,a)
		       val hscaq8 = parseSmay nil (c6,a7,q6)
		    in 
		       doit (aidx::yet,new,rest) hscaq8 
		    end
		 handle NotFound (c,a,q) (* raised by parseAttValue above     *)
		 => let val err = expectedOrEnded (expLitQuote,LOC_STAG) c
			val a1 = hookError(a,(getPos q,err))
			val (mt,caq1) = recoverSTag (c,a1,q)
		    in (old,todo,sp,mt,q,caq1)
		    end
		    
	 val (specd,todo,sp,empty,qe,(c3,a3,q3)) = doit (nil,nil,defs) hscaq2
	 val space = rev sp

	 (* generate the defaults for unspecified attributes *)
	 val (all,a4) = genMissingAtts dtd (a3,qe) (todo,rev specd)
      in 
	 ((((startPos,getPos q3),eidx,all,space,empty),elemInfo),(c3,a4,q3))
      end

   (*--------------------------------------------------------------------*)
   (* skip a tag, the initial "<" or "</" already read, the first arg    *)
   (* being a string describing the tag.                                 *)
   (* don't care about whether it is a start- or end-tag. Ignore ">" and *)
   (* "/>" if within a literal.                                          *)
   (*                                                                    *)
   (* print an error and finish if an entity end is found.               *)
   (*                                                                    *)
   (* return the remaining char and state.                               *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *) 
   (*--------------------------------------------------------------------*)
   fun skipTag loc aq = 
      let 
	 fun do_lit ch (c,a,q) =
	    if c=0wx00 then let val a1 = hookError(a,(getPos q,ERR_ENDED_BY_EE loc))
			    in (c,a1,q)
			    end
	    else if c=ch then doit (getChar(a,q))
		 else do_lit ch (getChar(a,q))
		       
	 and doit (c,a,q) =
	    case c
	      of 0wx00 => let val a1 = hookError(a,(getPos q,ERR_ENDED_BY_EE loc))
			  in (c,a1,q)
			  end
	       | 0wx22 (* #"\""*) => do_lit c (getChar(a,q))
	       | 0wx27 (* #"'" *) => do_lit c (getChar(a,q))
	       | 0wx2F (* #"/" *) => (case getChar(a,q)
					of (0wx3E,a1,q1) (* #">" *) => getChar(a1,q1) 
					 | caq1 => doit caq1)
	       | 0wx3E (* #">" *) => getChar(a,q)
	       | _ => doit(getChar(a,q))
      in doit (getChar aq)
      end
end
			

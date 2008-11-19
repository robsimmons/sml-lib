signature ParseDecl =
   sig
      (*----------------------------------------------------------------------
      include ParseBase

      val parseName    : UniChar.Char * AppData * State 
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
      val skipPSopt       : Dtd -> UniChar.Char * AppData * State 
	 -> UniChar.Char * AppData * State

      val skipTag   : Errors.Location -> AppData * State -> (UniChar.Char * AppData * State)
      val parseETag : Dtd -> AppData * State 
	 -> int * UniChar.Data * Errors.Position * (UniChar.Char * AppData * State)
      val parseSTag : Dtd -> Errors.Position -> UniChar.Char * AppData * State 
	 -> (HookData.StartTagInfo * Base.ElemInfo) * (UniChar.Char * AppData * State)
      ----------------------------------------------------------------------*)
      include ParseTags

      val skipDecl : bool -> UniChar.Char * AppData * State -> UniChar.Char * AppData * State

      val parseExtIdSub : Dtd -> UniChar.Char * AppData * State 
	 -> Base.ExternalId * bool * (UniChar.Char * AppData * State) 
      
      val parseEntityDecl   : Dtd -> EntId * Errors.Position * bool 
	 -> UniChar.Char * AppData * State -> UniChar.Char * AppData * State 
      val parseElementDecl  : Dtd -> EntId * Errors.Position * bool 
	 -> UniChar.Char * AppData * State -> UniChar.Char * AppData * State 
      val parseNotationDecl : Dtd -> EntId * Errors.Position * bool 
	 -> UniChar.Char * AppData * State -> UniChar.Char * AppData * State 
      val parseAttListDecl  : Dtd -> EntId * Errors.Position * bool 
	 -> UniChar.Char * AppData * State -> UniChar.Char * AppData * State 
   end

(*--------------------------------------------------------------------------*)
(* Structure: ParseDecl                                                     *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   skipDecl          : none                                               *)
(*   parseExtIdSub     : NotFound SyntaxError                               *)
(*   parseEntityDecl   : none                                               *)
(*   parseElementDecl  : none                                               *)
(*   parseNotationDecl : none                                               *)
(*   parseAttListDecl  : none                                               *)
(*--------------------------------------------------------------------------*)
functor ParseDecl (structure ParseBase : ParseBase) 
   : ParseDecl =
struct 
   structure ParseTags = ParseTags (structure ParseBase = ParseBase)

   open 
      UtilInt UtilList
      Base Errors HookData
      ParseTags
      
   (*--------------------------------------------------------------------*)
   (* skip a markup declaration, the initial "<!" and name already read. *)
   (* ignore ">" if within a literal. yake care of internal subset if    *)
   (* the first arg is true.                                             *)
   (*                                                                    *)
   (* print an error and finish if an entity end is found.               *)
   (*                                                                    *)
   (* return the remaining char and state.                               *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *) 
   (*--------------------------------------------------------------------*)
   fun skipDecl hasSubset caq = 
      let 
	 fun do_lit ch (c,a,q) =
	    if c=0wx00 then (c,a,q)
	    else if c=ch then getChar (a,q)
		 else do_lit ch (getChar(a,q))
	 fun do_decl (c,a,q) =
	    case c
	      of 0wx00 => (c,a,q)
	       | 0wx22 (* #"\""" *) => do_decl (do_lit c (getChar(a,q)))
	       | 0wx27 (* #"'" *) => do_decl (do_lit c (getChar(a,q)))
	       | 0wx3E (* #">" *) => getChar(a,q)
	       | _ => do_decl (getChar(a,q))
         fun do_subset (c,a,q) =
	    case c
	      of 0wx00 => (c,a,q)
	       | 0wx3C (* #"<" *) => do_subset (do_decl (getChar(a,q)))
	       | 0wx5D (* #"]" *) => getChar(a,q)
	       | _ => do_subset (getChar(a,q))
	 fun doit (c,a,q) =
	    case c
	      of 0wx00 => (c,hookError(a,(getPos q,ERR_ENDED_BY_EE LOC_DECL)),q) 
	       | 0wx22 (* #"\"""*) => doit (do_lit c (getChar(a,q)))
	       | 0wx27 (* #"'" *) => doit (do_lit c (getChar(a,q)))
	       | 0wx3E (* #">" *) => getChar(a,q)
	       | 0wx5B (* #"[" *) => if hasSubset then doit (do_subset (getChar(a,q)))
				     else doit (getChar(a,q))
	       | _ => doit (getChar(a,q))
      in doit caq
      end
   
   (*--------------------------------------------------------------------*)
   (* parse an external id, or a public id if the first arg is true.     *)
   (* Cf. 4.2.2 and 4.7:                                                 *)
   (*                                                                    *) 
   (*   [75] ExternalID ::= 'SYSTEM' S SystemLiteral                     *)
   (*                     | 'PUBLIC' S PubidLiteral S SystemLiteral      *)
   (*                                                                    *) 
   (*   [83]   PublicID ::= 'PUBLIC' S PubidLiteral                      *)
   (*                                                                    *) 
   (* raise NotFound if no name is found first.                          *)
   (* print an error if white space is missing.                          *)
   (* print an error and raise SyntaxState if a wrong name is found.     *)
   (* print an Error and raise SyntaxState if a required literal is not  *)
   (* found (depends on optSys).                                         *)
   (*                                                                    *)
   (* return the public and system identifiers as string options,        *)
   (* a boolean, whether whit space followed the external id,            *)
   (* and the next character and the remaining state.                    *)
   (*--------------------------------------------------------------------*)
   (* might raise: NotFound SyntaxState                                  *)
   (*--------------------------------------------------------------------*)
   fun parseExternalId dtd optSys (caq as (_,_,q))=
      let 
	 (* do not handle NotFound: in this case no extId was found *)
	 val (name,caq1) = parseName caq
	 val caq2 as (_,_,q2)= skipPS dtd caq1 
      in 
	 case name
	   of [0wx50,0wx55,0wx42,0wx4c,0wx49,0wx43] => (* "PUBLIC" *)
	      let 
		 val (pub,pquote,caq3) = parsePubidLiteral caq2
		    handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expLitQuote,[c])
						   val a1 = hookError(a,(getPos q,err))
					       in raise SyntaxError (c,a1,q)
					       end 
		 val (hadS,caq4 as (_,_,q4)) = skipPSmay dtd caq3
	      in let 
		    val (sys,squote,(c5,a5,q5)) = parseSystemLiteral caq4
		    val base = getUri q4
		    val a6 = if hadS then a5 else hookError(a5,(getPos q4,ERR_MISSING_WHITE))
		    val (hadS6,caq6) = skipPSmay dtd (c5,a6,q5)
		 in 
		    (EXTID(SOME(pub,pquote),SOME(base,sys,squote)),hadS6,caq6)
		 end
	      handle NotFound (c,a,q) => (* no system id *)
		 if optSys then (EXTID(SOME(pub,pquote),NONE),hadS,(c,a,q))
		 else let val a1 = hookError(a,(getPos q,ERR_EXPECTED(expLitQuote,[c])))
		      in raise SyntaxError (c,a1,q)
		      end
	      end
	      
	    | [0wx53,0wx59,0wx53,0wx54,0wx45,0wx4d] => (* "SYSTEM" *)
	      let 
		 val (sys,squote,caq3) = parseSystemLiteral caq2
		    handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expLitQuote,[c])
						   val a1 = hookError(a,(getPos q,err))
					       in raise SyntaxError (c,a1,q)
					       end
		 val base = getUri q2
		 val (hadS,caq4) = skipPSmay dtd caq3
	      in 
		 (EXTID(NONE,SOME(base,sys,squote)),hadS,caq4)
	      end

	    | _ => let val (c2,a2,q2) = caq2
		       val a3 = hookError(a2,(getPos q,ERR_EXPECTED(expExtId,name)))
		   in raise SyntaxError (c2,a3,q2)
		   end
      end
   (*--------------------------------------------------------------------*)
   (* parse an external id in an entity definition. Cf. 4.2.2:           *)
   (*                                                                    *) 
   (* print an Error and raise SyntaxState if no external id is found.   *)
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *)
   (*--------------------------------------------------------------------*)
   fun parseExtIdEnt dtd caq = parseExternalId dtd false caq
      handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expLitQuotExt,[c])
				 in raise SyntaxError (c,hookError(a,(getPos q,err)),q)
				 end
   (*--------------------------------------------------------------------*)
   (* parse an external or public id in a notation declaration.          *)
   (*                                                                    *) 
   (* print an Error and raise SyntaxState if neither external nor       *)
   (* public id is found.                                                *)
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *)
   (*--------------------------------------------------------------------*)
   fun parseExtIdNot dtd caq = parseExternalId dtd true caq
      handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expExtId,[c])
				 in raise SyntaxError (c,hookError(a,(getPos q,err)),q)
				 end
   (*--------------------------------------------------------------------*)
   (* parse an external id for the external subset.                      *)
   (*                                                                    *) 
   (* raise NotFound if no external id is found.                         *)
   (*--------------------------------------------------------------------*)
   (* might raise: NotFound SyntaxState                                  *)
   (*--------------------------------------------------------------------*)
   fun parseExtIdSub dtd caq = parseExternalId dtd false caq

   (*--------------------------------------------------------------------*)
   (* parse a parameter entity declaration, starting after the '%'. The  *)
   (* unique entity id of the initial '<' is given as first arg. 4.2:    *)
   (*                                                                    *)
   (*   [72] PEDecl ::= '<!ENTITY' S '%' S Name S PEDef S? '>'           *)
   (*   [74]  PEDef ::= EntityValue | ExternalID                         *)
   (*                                                                    *)
   (* (see also the comments for ParseDtd.parseMarkupDecl).              *)
   (*                                                                    *)
   (* print an error if white space is missing.                          *)
   (* print an error and raise SyntaxState if neither entity value nor   *)
   (* external identifier is found.                                      *)
   (* print an error and raise SyntaxState if the closing '>' is missing.*)
   (* print an error if the '>' is not in the same entity as the '<!'.   *)
   (*                                                                    *)
   (* enter the declared entity into the entity table.                   *)
   (* return the remaining char and state.                               *)
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *)
   (*--------------------------------------------------------------------*)
   fun parseParEntDecl dtd (startEnt,startPos,ext) caq =
      let 
	 val caq1 as (_,_,q1) = skipPS dtd caq 

	 val (name,caq2) = parseName caq1
	    handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expAnEntName,[c]) 
				       in raise SyntaxError (c,hookError(a,(getPos q,err)),q)
				       end
	 val idx = ParEnt2Index dtd name
	 val caq3 = skipPS dtd caq2 

	 val (ent,(c4,a4,q4)) = 
	    let val (ent,caq4) = parseEntityValue dtd PE_INTERN caq3
	        val caq5 = skipPSopt dtd caq4
	    in (ent,caq5)
	    end
	 handle NotFound caq => 
	    let val (extId,_,caq1) = parseExtIdEnt dtd caq
	    in (PE_EXTERN extId,caq1)
	    end

	 val a5 = if useParamEnts() orelse not ext then addParEnt dtd (a4,q1) (idx,ent,ext) else a4
	 val a6 = hookDecl(a5,((startPos,getPos q4),DEC_PAR_ENT(idx,ent,ext)))
      in 
	 if c4<>0wx3E (* #">" *) 
	    then let val a7 = hookError(a6,(getPos q4,ERR_EXPECTED(expGt,[c4])))
		 in raise SyntaxError(c4,a7,q4)
		 end
	 else let val a7 = if not (!O_VALIDATE) orelse getEntId q4=startEnt then a6
			   else hookError(a6,(getPos q4,ERR_DECL_ENT_NESTING LOC_ENT_DECL))
	      in getChar(a7,q4)
	      end
      end

   (*--------------------------------------------------------------------*)
   (* parse a general entity declaration, starting with the name. The    *)
   (* unique entity id of the initial '<' is given as first arg. 4.2:    *)
   (*                                                                    *)
   (*   [71]     GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'         *)
   (*   [73]  EntityDef ::= EntityValue | (ExternalID NDataDecl?)        *)
   (*                                                                    *)
   (*   [76]  NDataDecl ::= S 'NDATA' S Name     [ VC: Notation          *)
   (*                                                  Declared ]        *)
   (*                                                                    *)
   (*   If the NDataDecl is present, this is a general unparsed entity;  *)
   (*   otherwise it is a parsed entity.                                 *)
   (*                                                                    *)
   (*   Validity Constraint: Notation Declared                           *)
   (*   The Name must match the declared name of a notation.             *)
   (*                                                                    *)
   (* (see also the comments for ParseDtd.parseMarkupDecl).              *)
   (*                                                                    *)
   (* print an error if white space is missing.                          *)
   (* print an error and raise SyntaxState if neither entity value nor   *)
   (* external identifier is found.                                      *)
   (* print an error if name other then 'NDATA' is found after ext. id.  *)
   (* print an error and raise SyntaxState if no name is found after the *)
   (* 'NDATA'.                                                           *)
   (* print an error if the notation is not declared.                    *)
   (* print an error and raise SyntaxState if the closing '>' is missing.*)
   (* print an error if the '>' is not in the same entity as the '<!'.   *)
   (*                                                                    *)
   (* enter the declared entity into the entity table.                   *)
   (* return the remaining char and state.                               *)
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *)
   (*--------------------------------------------------------------------*)
   fun parseGenEntDecl dtd (startEnt,startPos,ext) (caq as (_,_,q)) =
      let 
	 val (name,caq1) = parseName caq
	    handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expEntNamePero,[c]) 
				       in raise SyntaxError (c,hookError(a,(getPos q,err)),q)
				       end
	 val idx = GenEnt2Index dtd name
	 val caq2 = skipPS dtd caq1 

	 val (ent,expEnd,(c3,a3,q3)) = 
	    (*-----------------------------------------------------------*)
	    (* Try for an internal entity. Then '>' must follow.         *)
	    (*-----------------------------------------------------------*)
	    let 
	       val (ent,caq3) = parseEntityValue dtd GE_INTERN caq2
	       val caq4 = skipPSopt dtd caq3
	    in 
	       (ent,expGt,caq4)
	    end
	 handle NotFound cq => (* raised by parseEntityValue *)
	    (*-----------------------------------------------------------*)
	    (* Must be external. First parse the external identifier.    *)
	    (*-----------------------------------------------------------*)
	    let 
	       val (extId,hadS,caq1 as (_,_,q1)) = parseExtIdEnt dtd caq2
	    in let 
		  (*-----------------------------------------------------*)
		  (* Does a name follow? Then is must be 'NDATA' and the *)
		  (* notation name follows. Thus the entity is unparsed. *)
		  (* Also, only '>' may come next.                       *)
		  (* NotFound is handled at the end of the let.          *)
		  (*-----------------------------------------------------*)
		  val (key,(c2,a2,q2)) = parseName caq1 
		  val a3 = if hadS then a2 else hookError(a2,(getPos q1,ERR_MISSING_WHITE))
		  val a4 = if key = [0wx4e,0wx44,0wx41,0wx54,0wx41] (* "NDATA" *) then a3
			   else hookError(a3,(getPos q1,ERR_EXPECTED(expGtNdata,key)))
				 
		  val caq5 as (_,_,q5) = skipPS dtd (c2,a4,q2) 
			   
		  val (not,caq6) = parseName caq5
		     handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expANotName,[c])
						    val a1 = hookError(a,(getPos q,err))
						in raise SyntaxError (c,a1,q)
						end
		  val notIdx = AttNot2Index dtd not
		  val caq7 = skipPSopt dtd caq6
	       in 
		  (GE_UNPARSED(extId,notIdx,getPos q5),expGt,caq7)
	       end
	    handle NotFound caq => 
	       (*--------------------------------------------------------*)
	       (* No 'NDATA' present, so it's parsed external entity.    *)
	       (* A 'NDATA' might have followed.                         *)
	       (*--------------------------------------------------------*)
	       (GE_EXTERN extId,expGtNdata,caq)
	    end
		       
	 val a4 = if useParamEnts() orelse not ext then addGenEnt dtd (a3,q) (idx,ent,ext) else a3
	 val a5 = hookDecl(a4,((startPos,getPos q3),DEC_GEN_ENT(idx,ent,ext)))
      in 
	 if c3<>0wx3E (* #">" *) 
	    then let val a6 = hookError(a5,(getPos q3,ERR_EXPECTED(expGt,[c3])))
		 in raise SyntaxError(c3,a6,q3)
		 end
	 else let val a6 = if not (!O_VALIDATE) orelse getEntId q3=startEnt then a5
			   else hookError(a5,(getPos q3,ERR_DECL_ENT_NESTING LOC_ENT_DECL))
	      in getChar(a6,q3)
	      end
      end

   (*--------------------------------------------------------------------*)
   (* parse an entity declaration, the initial '<!ENTITY' already read.  *)
   (* The unique entity id of the initial '<' is given as 1st arg. 4.2:  *)
   (*                                                                    *)
   (*   [70] EntityDecl ::= GEDecl | PEDecl                              *)
   (*   [71]     GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'         *)
   (*   [72]     PEDecl ::= '<!ENTITY' S '%' S Name S PEDef S? '>'       *)
   (*                                                                    *)
   (* (see also the comments for ParseDtd.parseMarkupDecl).              *)
   (*                                                                    *)
   (* raise SyntaxState in case of a syntax error.                       *)
   (* print an error if white space is missing.                          *)
   (*                                                                    *)
   (* print an error for entity end exceptions in subfunctions.          *)
   (* catch syntax errors by recovering to the next possible state.      *)
   (*                                                                    *)
   (* pass control to parseParEntDecl or parseGenEntDecl, depending on   *)
   (* whether the S is followed by a '%'.                                *)
   (* return the remaining char and state.                               *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *)
   (*--------------------------------------------------------------------*)
   fun parseEntityDecl dtd pars caq = 
      let 
	 val (hadPero,caq1) = skipPSdec dtd caq 
      in 
	 if hadPero then parseParEntDecl dtd pars caq1
	 else parseGenEntDecl dtd pars caq1
      end
   handle exn as SyntaxError (c,a,q) => 
      let val a1 = if c=0wx00 then hookError(a,(getPos q,ERR_ENDED_BY_EE LOC_ENT_DECL)) 
		   else a
      in recoverDecl false (c,a1,q)
      end

   (*--------------------------------------------------------------------*)
   (* parse a notation declaration, the initial '<!NOTATION' already     *)
   (* read. The unique entity id of the '<!' is given as first arg. 4.7: *)
   (*                                                                    *)
   (*   [82] NotationDecl ::= '<!NOTATION' S Name S                      *)
   (*                         (ExternalID | PublicID) S? '>'             *)
   (*                                                                    *)
   (* (see also the comments for ParseDtd.parseMarkupDecl).              *)
   (*                                                                    *)
   (* print an error and raise SyntaxState if no notation name, no       *)
   (* external/public identifier or no final '>' is found.               *)
   (* print an error if the '>' is not in the same entity as the '<!'.   *)
   (* print an error if white space is missing.                          *)
   (*                                                                    *)
   (* print an error for entity end exceptions in subfunctions.          *)
   (* catch syntax errors by recovering to the next possible state.      *)
   (*                                                                    *)
   (* enter the declared notation into the notation table.               *)
   (* return the remaining char and state.                               *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *)
   (*--------------------------------------------------------------------*)
   fun parseNotationDecl dtd (startEnt,startPos,ext) caq =
      let 
	 val caq1 as (_,_,q1) = skipPS dtd caq 
	 val (name,caq2) = parseName caq1
	    handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expANotName,[c]) 
				       in raise SyntaxError (c,hookError(a,(getPos q,err)),q)
				       end
	 val idx = AttNot2Index dtd name
	 val caq3 = skipPS dtd caq2 
	       
	 val (extId,_,(c4,a4,q4)) = parseExtIdNot dtd caq3
		       
	 val a5 = if useParamEnts() orelse not ext then addNotation dtd (a4,q1) (idx,extId) else a4
	 val a6 = hookDecl(a5,((startPos,getPos q4),DEC_NOTATION(idx,extId,ext)))
      in 
	 if c4<>0wx3E (* #">" *) 
	    then let val a7 = hookError(a6,(getPos q4,ERR_EXPECTED(expGt,[c4])))
		 in raise SyntaxError (c4,a7,q4)
		 end
	 else let val a7 = if not (!O_VALIDATE) orelse getEntId q4=startEnt then a6
			   else hookError(a6,(getPos q4,ERR_DECL_ENT_NESTING LOC_NOT_DECL))
	      in getChar(a7,q4)
	      end
      end
   handle exn as SyntaxError(c,a,q) => 
      let val a1 = if c=0wx00 then hookError(a,(getPos q,ERR_ENDED_BY_EE LOC_NOT_DECL)) 
		   else a
      in recoverDecl false (c,a1,q)
      end

   (*--------------------------------------------------------------------*)
   (* parse a mixed-content specification, the initial '(', S? and '#'   *)
   (* already read. The unique id of the openening paren's entity is     *)
   (* given as first arg. Cf. 3.2.1/2:                                   *)
   (*                                                                    *)
   (*   Validity Constraint: Proper Group/PE Nesting                     *)
   (*   Parameter-entity replacement text must be properly nested with   *) 
   (*   parenthetized groups. That is to say, if either of the opening   *)
   (*   or closing parentheses in a choice, seq, or Mixed construct is   *)
   (*   contained in the replacement text for a parameter entity, both   *)
   (*   must be contained in the same replacement text.                  *)
   (*   ...                                                              *)
   (*   [51] Mixed ::= '(' S? '#PCDATA'           [ VC: Proper Group/PE  *)
   (*                   (S? '|' S? Name)* S? ')*'       Nesting ]        *)
   (*                | '(' S? '#PCDATA' S? ')'    [ VC: No Duplicate     *)
   (*                                                   Types ]          *)
   (*                                                                    *)
   (* print an error and raise SyntaxState if no name is found first.    *)
   (* print an error if a name other than 'PCDATA' is found.             *)
   (* is found in the first place.                                       *)
   (* print an error if element names are specified but no '*' follows.  *)
   (* print an error if an element name is specified more than once.     *)
   (* print an error and raise SyntaxState if neither '|' nor ')' is     *)
   (* found after the 'PCDATA' or after an element name.                 *)
   (* print an error if the closing parenthesis is not in the same       *)
   (* as the opening one.                                                *)
   (*                                                                    *)
   (* return the mixed-content specification, togther with the next      *)
   (* character and state.                                               *)
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *)
   (*--------------------------------------------------------------------*)
   fun parseMixed dtd lparEnt (caq as (_,_,q)) = 
      let 
	 fun doit is (c,a,q) = 
	    case c 
	      of 0wx29 (* #")" *) => 
		 let val a1 = if not (!O_VALIDATE) orelse getEntId q=lparEnt then a
			      else hookError(a,(getPos q,ERR_GROUP_ENT_NESTING LOC_MIXED))
		 in (rev is,getChar(a1,q))
		 end
	       | 0wx7C (* #"|" *) => 
		 let
		    val caq1 as (_,_,q1) = skipPSopt dtd (getChar(a,q))
			  
		    val (name,(c2,a2,q2)) = parseName caq1
		       handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expAName,[c])
						      val a1 = hookError(a,(getPos q,err))
						  in raise SyntaxError (c,a1,q)
						  end
		    val i = Element2Index dtd name
		    val (newis,a3) = 
		       if not (member i is) then (i::is,a2)
		       else let val a3 = if !O_VALIDATE 
					    then hookError(a2,(getPos q1,ERR_MULT_MIXED name))
					 else a2
			    in (is,a3)
			    end
		    val caq3 = skipPSopt dtd (c2,a3,q2)
		 in doit newis caq3
		 end
	       | _ => let val a1 = hookError(a,(getPos q,ERR_EXPECTED(expBarRpar,[c])))
		      in raise SyntaxError (c,a1,q)
		      end
		    
	 val (name,(c1,a1,q1)) = parseName caq
	    handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expPcdata,[c])
				       in raise SyntaxError (c,hookError(a,(getPos q,err)),q)
				       end
	 val a2 = case name
		    of [0wx50,0wx43,0wx44,0wx41,0wx54,0wx41] (* "PCDATA" *) => a1
		     | _ => hookError(a1,(getPos q,ERR_EXPECTED(expPcdata,name)))
			 
	 val caq2 = skipPSopt dtd (c1,a2,q1)
	 val (is,(c3,a3,q3)) = doit nil caq2
	       
	 val caq4 = if c3=0wx2A (* #"*" *) then getChar(a3,q3)
		    else let val a4 = if null is then a3 
				      else hookError(a3,(getPos q3,ERR_EXPECTED(expRep,[c3])))
			 in (c3,a4,q3)
			 end
      in 
	 (CT_MIXED is,caq4)
      end

   (*--------------------------------------------------------------------*)
   (* parse an optional occurrence indicator afer a content particle or  *)
   (* a content model, given as first argument. Cf. 3.2.1:               *)
   (*                                                                    *)
   (*   [47] children ::= (choice | seq) ('?' | '*' | '+')?              *)
   (*   [48]       cp ::= (Name | choice | seq) ('?' | '*' | '+')?       *)
   (*                                                                    *)
   (* return the (possibly modified) content particle, together with the *)
   (* next char and state.                                               *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *)
   (*--------------------------------------------------------------------*)
   fun parseOcc cm (c,a,q) =
      case c
	of 0wx3F (* #"?" *) => (CM_OPT cm,getChar(a,q))
	 | 0wx2A (* #"*" *) => (CM_REP cm,getChar(a,q))
	 | 0wx2B (* #"+" *) => (CM_PLUS cm,getChar(a,q))
	 | _ => (cm,(c,a,q))

   (*--------------------------------------------------------------------*)
   (* parse a content particle. Cf. 3.2.1:                               *)
   (*                                                                    *)
   (*   Validity Constraint: Proper Group/PE Nesting                     *)
   (*   Parameter-entity replacement text must be properly nested with   *) 
   (*   parenthetized groups. ...                                        *)
   (*                                                                    *)
   (* (see also parseMixed)                                              *)
   (*                                                                    *)
   (*   [48]       cp ::= (Name | choice | seq) ('?' | '*' | '+')?       *)
   (*   [49]   choice ::= '(' S? cp                [ VC: Proper Group/   *)
   (*                     ( S? '|' S? cp )* S? ')'       PE Nesting   ]  *)
   (*   [50]      seq ::= '(' S? cp                [ VC: Proper Group/   *)
   (*                     ( S? ',' S? cp )* S? ')'       PE Nesting   ]  *)
   (*                                                                    *)
   (* print an error and raise SyntaxState if no element name or "(" is  *)
   (* found in the first place.                                          *)
   (*                                                                    *)
   (* return the content particle together with the next char and state. *)
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *)
   (*--------------------------------------------------------------------*)
   fun parseCP dtd (c,a,q) = 
      case c
	of 0wx28 (* #"(" *) => 
	   let 
	      val lparEnt = getEntId q
	      val caq1 = skipPSopt dtd (getChar (a,q))
	   in parseGroup dtd lparEnt caq1
	   end
	 | _ => (* must be an element name *)
	   let 
	      val (name,caq1) = parseName (c,a,q)
		 handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expElemLpar,[c])
						val a1 = hookError(a,(getPos q,err))
					    in raise SyntaxError (c,a1,q)
					    end
	      val idx = Element2Index dtd name
	   in 
	      parseOcc (CM_ELEM idx) caq1
	   end
	
   (*--------------------------------------------------------------------*)
   (* parse a seq/choice, the first content particle and the connector   *)
   (* already parsed; the connector, the type of group and the entity id *)
   (* of the opening parenthesis are given in first arg. Cf. 3.2.1:      *)
   (*                                                                    *)
   (*   Validity Constraint: Proper Group/PE Nesting                     *)
   (*   Parameter-entity replacement text must be properly nested with   *) 
   (*   parenthetized groups. ...                                        *)
   (*                                                                    *)
   (* (see also parseMixed)                                              *)
   (*                                                                    *)
   (*   [49]   choice ::= '(' S? cp                [ VC: Proper Group/   *)
   (*                     ( S? '|' S? cp )* S? ')'       PE Nesting   ]  *)
   (*   [50]      seq ::= '(' S? cp                [ VC: Proper Group/   *)
   (*                     ( S? ',' S? cp )* S? ')'       PE Nesting   ]  *)
   (*                                                                    *)
   (* print an error and raise SyntaxState if something other than the   *)
   (* connector or ')' is found after a content particle.                *)
   (* print an error if the closing parenthesis of a group is not in the *)
   (* same entity as the opening one.                                    *)
   (*                                                                    *)
   (* return the list of content particles parsed, together with the     *)
   (* remaining character and state.                                     *)
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *)
   (*--------------------------------------------------------------------*)
   and parseGroup' dtd (con,loc,lparEnt) caq =
      let fun doit caq =
	 let
	    val caq1 = skipPSopt dtd caq
	    val (cp,caq2) = parseCP dtd caq1
	    val (c3,a3,q3) = skipPSopt dtd caq2 
	 in 
	    if c3=0wx29 (* #")" ( *) 
	       then let val a4 = if not (!O_VALIDATE) orelse getEntId q3=lparEnt then a3
				 else hookError(a3,(getPos q3,ERR_GROUP_ENT_NESTING loc))
		    in ([cp],getChar(a4,q3))
		    end
	    else (if c3=con then let val (cps,caq4) = doit (getChar(a3,q3))
				 in (cp::cps,caq4)
				 end
		  else let val err = ERR_EXPECTED(expConCRpar con,[c3])
		       in raise SyntaxError (c3,hookError(a3,(getPos q3,err)),q3)
		       end)
	 end
      in 
	 doit caq
      end
					
   (*--------------------------------------------------------------------*)
   (* parse a seq/choice, the first content particle parsed; the entity  *)
   (* id of the opening parenthesis are given in first arg. Cf. 3.2.1:   *)
   (*                                                                    *)
   (* (see also parseMixed)                                              *)
   (*                                                                    *)
   (*   [49]   choice ::= '(' S? cp                [ VC: Proper Group/   *)
   (*                     ( S? '|' S? cp )* S? ')'       PE Nesting   ]  *)
   (*   [50]      seq ::= '(' S? cp                [ VC: Proper Group/   *)
   (*                     ( S? ',' S? cp )* S? ')'       PE Nesting   ]  *)
   (*                                                                    *)
   (* print an error and raise SyntaxState if neither '|' nor ',' nor    *)
   (* ')' follows the first content particle in a seq/choice.            *)
   (*                                                                    *)
   (* return the list of as a ContentModel, together with the remaining  *)
   (* character and state.                                               *)
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *)
   (*--------------------------------------------------------------------*)
   and parseGroup dtd lparEnt caq =
      let 
	 val (cp,caq1) = parseCP dtd caq
	 val (c2,a2,q2) = skipPSopt dtd caq1
	 val (group,caq3) = 
	    case c2
	      of 0wx29 (* #")" *) => 
		 let val a3 = if not (!O_VALIDATE) orelse getEntId q2=lparEnt then a2
			      else hookError(a2,(getPos q2,ERR_GROUP_ENT_NESTING LOC_SEQ))
		 in (CM_SEQ[cp],getChar(a3,q2))
		 end
	       | 0wx2C (* #"," *) => 
		 let val (cps,caq3) = parseGroup' dtd (c2,LOC_SEQ,lparEnt) (getChar(a2,q2))
		 in (CM_SEQ(cp::cps),caq3)
		 end
	       | 0wx7C (* #"|" *) => 
		 let val (cps,caq3) = parseGroup' dtd (c2,LOC_CHOICE,lparEnt) (getChar(a2,q2))
		 in (CM_ALT(cp::cps),caq3)
		 end
	       | _ => let val a3 = hookError(a2,(getPos q2,ERR_EXPECTED(expConRpar,[c2])))
		      in raise SyntaxError (c2,a3,q2)
		      end
      in parseOcc group caq3
      end
   
   (*--------------------------------------------------------------------*)
   (* parse a content specification. Cf. 3.2/3.2.1:                      *)
   (*                                                                    *)
   (*   Validity Constraint: Proper Group/PE Nesting                     *)
   (*   Parameter-entity replacement text must be properly nested with   *) 
   (*   parenthetized groups. That is to say, if either of the opening   *)
   (*   or closing parentheses in a choice, seq, or Mixed construct is   *)
   (*   contained in the replacement text for a parameter entity, both   *)
   (*   must be contained in the same replacement text.                  *)
   (*   ...                                                              *)
   (*   [46] contentspec ::= 'EMPTY' | 'ANY' | Mixed | children          *)
   (*                                                                    *)
   (*   [47] children ::= (choice | seq) ('?' | '*' | '+')?              *)
   (*                                                                    *)
   (*   [49] choice ::= '(' S? cp ( S? '|' S? cp )* S? ')' [ VC:Proper   *)
   (*   [50]    seq ::= '(' S? cp ( S? ',' S? cp )* S? ')'      Group/PE *)  
   (*                                                           Nesting ]*) 
   (*                                                                    *)
   (*   [51] Mixed ::= '(' S? '#PCDATA'           [ VC: Proper Group/PE  *)
   (*                   (S? '|' S? Name)* S? ')*'       Nesting ]        *)
   (*                | '(' S? '#PCDATA' S? ')'    [ VC: No Duplicate     *)
   (*                                                   Types ]          *)
   (*                                                                    *)
   (* print an error and raise SyntaxState if no children, Mixed, or     *)
   (* name is found.                                                     *)
   (* print an error and assume ANY if an ambiguous content model is     *)
   (* specified.                                                         *)
   (* print an error and assume ANY if a name other than EMPTY or ANY    *)
   (* is found.                                                          *)
   (* print an error if the closing parenthesis of a Mixed is not in the *)
   (* same entity as the opening one.                                    *)
   (*                                                                    *)
   (* return the parsed content specification, togther with the next     *)
   (* character and state.                                               *)
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *)
   (*--------------------------------------------------------------------*)
   fun parseContentSpec dtd curr (c,a,q) = 
      case c
	of 0wx28 (* #"(" *) => 
	   let 
	      val (c1,a1,q1) = skipPSopt dtd (getChar(a,q))
	      val lparEnt = getEntId q
	   in 
	      if c1=0wx23 (* #"#" *) 
		 then parseMixed dtd lparEnt (getChar(a1,q1)) 
	      else let val (cm,(c2,a2,q2)) = parseGroup dtd lparEnt (c1,a1,q1)
		       val (dfa,a3) = (makeDfa cm,a2) handle Ambiguous(a,n1,n2) 
			  => if !O_COMPATIBILITY 
				then let val err = ERR_AMBIGUOUS(Index2Element dtd a,n1,n2)
					 val a3 = hookError(a2,(getPos q,err))
					 val dfa = makeChoiceDfa cm
				     in (dfa,a3)
				     end
			     else (makeAmbiguous cm,a2) handle DfaTooLarge max
				=> let val a3 = if !O_DFA_WARN_TOO_LARGE 
						   then hookWarning
						      (a2,(getPos q,WARN_DFA_TOO_LARGE(curr,max)))
						else a2
				       val dfa = makeChoiceDfa cm
				   in (dfa,a3)
				   end
		   in (CT_ELEMENT(cm,dfa),(c2,a3,q2))
		   end
	   end
	 | _ => (* must be ANY or EMPTY *)
	   let 
	      val (name,caq1 as (c1,a1,q1)) = parseName (c,a,q)
		 handle NotFound (c,a,q) => 
		    let val err = ERR_EXPECTED(expContSpec,[c])
		    in raise SyntaxError(c,hookError(a,(getPos q,err)),q)
		    end 
	   in case name
		of [0wx41,0wx4e,0wx59]             (* "ANY"   *) => (CT_ANY,caq1)
		 | [0wx45,0wx4d,0wx50,0wx54,0wx59] (* "EMPTY" *) => (CT_EMPTY,caq1)
		 | _ => let val a2 = hookError(a1,(getPos q,ERR_EXPECTED(expContSpec,name)))
			in (CT_ANY,(c1,a2,q1))
			end
	   end

   (*--------------------------------------------------------------------*)
   (* parse an element declaration, the initial '<!ELEMENT' already      *)
   (* read. The unique entity id of the '<!' is given as first arg. 3.2: *)
   (*                                                                    *)
   (*   [45] elementdecl ::= '<!ELEMENT' S Name         [ VC: Unique     *)
   (*                        S contentspec S? '>'         Element Type   *)
   (*                                                     Declaration ]  *)
   (*                                                                    *)
   (* (see also the comments for ParseDtd.parseMarkupDecl).              *)
   (*                                                                    *)
   (* print an error and raise SyntaxState if no element name, no        *)
   (* content specification, or no final '>' is found.                   *)
   (* print an error if the '>' is not in the same entity as the '<!'.   *)
   (* print an error if white space is missing.                          *)
   (*                                                                    *)
   (* print an error for entity end exceptions in subfunctions.          *)
   (* catch syntax errors by recovering to the next possible state.      *)
   (*                                                                    *)
   (* enter the declared element into the notation table.                *)
   (* return the remaining char and state.                               *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *)
   (*--------------------------------------------------------------------*)
   fun parseElementDecl dtd (startEnt,startPos,ext) caq =
      let 
	 val (caq1 as (_,_,q1))= skipPS dtd caq 
	 val (name,(c2,a2,q2)) = parseName caq1
	    handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expAnElemName,[c])
				       in raise SyntaxError(c,hookError(a,(getPos q,err)),q)
				       end
	 val a3 = checkElemName (a2,q1) name
	 val idx = Element2Index dtd name
	 val caq3 = skipPS dtd (c2,a3,q2) 
	       
	 val (contSpec,(c4,a4,q4)) = parseContentSpec dtd name caq3

	 val a5 = if useParamEnts() orelse not ext then addElement dtd (a4,q1) (idx,contSpec,ext) 
		  else a4
	 val a5' = hookDecl(a5,((startPos,getPos q4),DEC_ELEMENT(idx,contSpec,ext)))

	 val (c6,a6,q6) = skipPSopt dtd (c4,a5',q4)
      in  
	 if c6<>0wx3E (* #">" *) 
	    then let val a7 = hookError(a6,(getPos q6,ERR_EXPECTED(expGt,[c6])))
		 in raise SyntaxError(c6,a7,q6)
		 end
	 else let val a7 = if not (!O_VALIDATE) orelse getEntId q6=startEnt then a6
			   else hookError(a6,(getPos q6,ERR_DECL_ENT_NESTING LOC_ELEM_DECL))
	      in getChar(a7,q6)
	      end
      end
   handle exn as SyntaxError (c,a,q) => 
      let val a1 = if c=0wx00 then hookError(a,(getPos q,ERR_ENDED_BY_EE LOC_ELEM_DECL))
		   else a
      in recoverDecl false (c,a1,q)
      end

   (*--------------------------------------------------------------------*)
   (* parse an enumerated attribute type, the '(' already consumed. the  *)
   (* 1st arg is a string describing the attribute (nmtoken or notation),*)
   (* the 2nd arg is a function that parses a single token, the 3rd arg  *)
   (* a function for converting the token to its index. 3.3.1:           *)
   (*                                                                    *)
   (*   [58] NotationType ::= 'NOTATION' S                               *)
   (*                         '(' S? Name (S? '|' S? Name)* S? ')'       *)
   (*   [59]  Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')' *)
   (*                                                                    *)
   (* print an error and raise SyntaxState if no token is found after a  *)
   (* '(' or '|', or if neither '|' nor ')' follows a token.             *)
   (*                                                                    *)
   (* return the (sorted) list of indices of the parsed tokens.          *)
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *)
   (*--------------------------------------------------------------------*)
   fun parseEnumerated dtd (expWhat,parseToken,Token2Index) caq =
      let fun doit idxs caq = 
	 let 
	    val caq1 as (_,_,q1) = skipPSopt dtd caq
	    val (nt,(c2,a2,q2)) = parseToken caq1
	       handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expWhat,[c])
					  in raise SyntaxError(c,hookError(a,(getPos q,err)),q)
					  end
	    val (idx,a3) = Token2Index dtd (a2,q1) nt
	    val (c4,a4,q4) = skipPSopt dtd (c2,a3,q2)
	    val newIdxs = insertInt(idx,idxs)
	 in case c4 
	      of 0wx7C (* #"|" *) => doit newIdxs (getChar(a4,q4))
	       | 0wx29 (* #")" *) => (newIdxs,getChar(a4,q4))
	       | _ => let val a5 = hookError(a4,(getPos q4,ERR_EXPECTED(expBarRpar,[c4])))
		      in raise SyntaxError (c4,a5,q4)
		      end
	 end
      in doit nil caq
      end

   (*--------------------------------------------------------------------*)
   (* Convert a (name) token to its index as an enumerated attribute.    *)
   (* 3.3.1:                                                             *)
   (*                                                                    *)
   (*   Validity Constraint: Notation Attributes                         *)
   (*   ... all notation names in the declaration must be declared.      *)
   (*                                                                    *)
   (* print an error if a notation is not declared.                      *)
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *)
   (*--------------------------------------------------------------------*)
   fun Token2NmtokenIndex dtd (a,_) token = (AttNot2Index dtd token,a)
   fun Token2NotationIndex dtd (a,q) token =
      let 
	 val idx = AttNot2Index dtd token
	 val a1 = if not (!O_VALIDATE) orelse hasNotation dtd idx then a
		  else hookError(a,(getPos q,ERR_UNDECLARED(IT_NOTATION,token,LOC_NONE)))
      in (idx,a1)
      end

   (*--------------------------------------------------------------------*)
   (* parse an attribute type, the 1st arg being the element this decl.  *)
   (* refers to. 3.3.1:                                                  *)
   (*                                                                    *)
   (*   [54] AttType ::= StringType | TokenizedType | EnumeratedType     *)
   (*                                                                    *)
   (*   [55]    StringType ::= 'CDATA'                                   *)
   (*   [56] TokenizedType ::= 'ID'       [VC: One ID per Element Type ] *)
   (*                        | 'IDREF'                                   *)
   (*                        | 'IDREFS'                                  *)
   (*                        | 'ENTITY'                                  *)
   (*                        | 'ENTITIES'                                *)
   (*                        | 'NMTOKEN'                                 *)
   (*                        | 'NMTOKENS'                                *)
   (*                                                                    *)
   (*   Validity Constraint: One ID per Element Type                     *)
   (*   No element type may have more than one ID attribute specified.   *)
   (*                                                                    *)
   (*   Enumerated Attribute Types                                       *)
   (*                                                                    *)
   (*   [57] EnumeratedType ::= NotationType | Enumeration               *)
   (*   [58]   NotationType ::= 'NOTATION' S '(' ...                     *)
   (*   [59]    Enumeration ::= '(' ...                                  *)
   (*                                                                    *)
   (* print an error and raise SyntaxState if no '(', or name is found   *)
   (* in the first place, or the name does not start an attribute type,  *)
   (* or if no '(' follows a 'NOTATION'.                                 *)
   (* print an error and assume NMTOKEN instead of ID if the element     *)
   (* already has an ID attribute.                                       *)
   (*                                                                    *)
   (* return the attribute type together with the next char and state.   *)
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *)
   (*--------------------------------------------------------------------*)
   fun parseAttType dtd elem (c,a,q) = 
      if c=0wx28 (* #"(" *) then 
	 let val (idxs,caq1) = parseEnumerated dtd 
	    (expANameToken,parseNmtoken,Token2NmtokenIndex) (getChar(a,q))
	 in (AT_GROUP idxs,caq1)
	 end
      else let val (name,caq1 as (c1,a1,q1)) = parseName (c,a,q)
	 handle NotFound cq => let val err = ERR_EXPECTED(expAttType,[c])
			       in raise SyntaxError (c,hookError(a,(getPos q,err)),q)
			       end
	   in case name 
		of [0wx43,0wx44,0wx41,0wx54,0wx41] (* "CDATA" *) => 
		   (AT_CDATA,caq1)
		 | [0wx49,0wx44] (* "ID" *) => 
		   (AT_ID,caq1)
		 | [0wx49,0wx44,0wx52,0wx45,0wx46] (* "IDREF" *) => 
		   (AT_IDREF,caq1)
		 | [0wx49,0wx44,0wx52,0wx45,0wx46,0wx53] (* "IDREFS" *) => 
		   (AT_IDREFS,caq1)
		 | [0wx45,0wx4e,0wx54,0wx49,0wx54,0wx59] (* "ENTITY" *) => 
		   (AT_ENTITY,caq1)
		 | [0wx45,0wx4e,0wx54,0wx49,0wx54,0wx49,0wx45,0wx53] (* "ENTITIES" *) => 
		   (AT_ENTITIES,caq1)
		 | [0wx4e,0wx4d,0wx54,0wx4f,0wx4b,0wx45,0wx4e] (* "NMTOKEN" *) => 
		   (AT_NMTOKEN,caq1)
		 | [0wx4e,0wx4d,0wx54,0wx4f,0wx4b,0wx45,0wx4e,0wx53] (* "NMTOKEN" *) => 
		   (AT_NMTOKENS,caq1)
		 | [0wx4e,0wx4f,0wx54,0wx41,0wx54,0wx49,0wx4f,0wx4e] (* "NOTATION" *) => 
		   let val (c2,a2,q2) = skipPSopt dtd caq1
		   in case c2 
			of 0wx28 (* #"(" *) => 
			   let val (idxs,caq3) = parseEnumerated dtd 
			      (expANotName,parseName,Token2NotationIndex) (getChar(a2,q2))
			   in (AT_NOTATION idxs,caq3)
			   end
			 | _ => let val err = ERR_EXPECTED(expLpar,[c2])
				in raise SyntaxError(c2,hookError(a2,(getPos q2,err)),q2)
				end
		   end
		 | _ => let val a2 = hookError(a1,(getPos q,ERR_EXPECTED(expAttType,name))) 
			in raise SyntaxError (c1,a2,q1)
			end
	   end

   (*--------------------------------------------------------------------*)
   (* parse an attribute default, for an attribute whose type is given   *)
   (* the 1st argument. Cf. 3.3.2:                                       *)
   (*                                                                    *)
   (*   [60] DefaultDecl ::= '#REQUIRED' | '#IMPLIED'                    *)
   (*                      | (('#FIXED' S)? AttValue)                    *)
   (*                                                                    *)
   (*   Validity Constraint: Attribute Default Legal                     *)
   (*   The declared default value must meet the lexical constraints of  *)
   (*   the declared attribute type.                                     *)
   (*                                                                    *)
   (* and 3.3.1:                                                         *)
   (*                                                                    *)
   (*   Validity Constraint: ID Attribute Default                        *)
   (*   An ID attribute must have a declared default of #IMPLIED or      *)
   (*   #REQUIRED.                                                       *)
   (*                                                                    *)
   (* print an error and raise SyntaxState if no '#' or literal is found *)
   (* in the first place, or no name or a wrong name is found after the  *)
   (* '#', or if no literal follows the 'FIXED'.                         *)
   (* print an error if white space is missing.                          *)
   (* print an error and assume IMPLIED if the default for an ID attrib. *)
   (* is not IMPLIED or REQUIRED.                                        *)
   (*                                                                    *)
   (* return the default together with the remaining char and state.     *)
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *)
   (*--------------------------------------------------------------------*)
   fun parseDefaultDecl dtd (aidx,attType) (c,a,q) =
      if c=0wx23 (* #"#" *) then  
	 let 
	    val caq0 as (_,_,q0) = (getChar(a,q))
	    val (name,caq1) = parseName caq0
	       handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expAttDefKey,[c])
					  in raise SyntaxError(c,hookError(a,(getPos q,err)),q)
					  end
	 in case name 
	      of [0wx46,0wx49,0wx58,0wx45,0wx44] (* "FIXED" *) => 
		 let 
		    val caq2 as (_,_,q2) = skipPS dtd caq1 
		    val (lit,text,(c3,a3,q3)) = parseAttValue dtd caq2
		       handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expLitQuote,[c])
						      val a1 = hookError(a,(getPos q,err))
						  in raise SyntaxError (c,a1,q)
						  end
		 in 
		    if !O_VALIDATE andalso isIdType attType 
		       then let val a4 = hookError(a3,(getPos q,ERR_ID_DEFAULT))
			    in (AD_IMPLIED,(c3,a4,q3))
			    end
		    else 
		       let val (cv,(av,a4)) = makeAttValue dtd (a3,q2) 
			  (aidx,attType,false,true,text)
		       in (AD_FIXED((lit,cv,av),(getPos q2,ref false)),(c3,a4,q3))
		       end
		      handle AttValue a => (AD_IMPLIED,(c3,a,q3))
		 end

	       | [0wx49,0wx4d,0wx50,0wx4c,0wx49,0wx45,0wx44] (* "IMPLIED" *) => 
		 (AD_IMPLIED,caq1)
	       | [0wx52,0wx45,0wx51,0wx55,0wx49,0wx52,0wx45,0wx44] (* "REQUIRED" *) => 
		 (AD_REQUIRED,caq1)
	       | _ => let val (c1,a1,q1) = caq1
			  val a2 = hookError(a1,(getPos q0,ERR_EXPECTED(expAttDefKey,name)))
		      in raise SyntaxError (c1,a2,q1)
		      end
	 end
      else let 
	      val (lit,text,(c1,a1,q1)) = parseAttValue dtd (c,a,q)
		 handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expQuoteRni,[c])
						val a1 = hookError(a,(getPos q,err))
					    in raise SyntaxError(c,a1,q)
					    end
	   in 
	      if !O_VALIDATE andalso isIdType attType 
		 then let val a2 = hookError(a1,(getPos q,ERR_ID_DEFAULT))
		      in (AD_IMPLIED,(c1,a2,q1))
		      end
	      else let val (cv,(av,a2)) = makeAttValue dtd (a1,q) (aidx,attType,false,true,text)
		   in (AD_DEFAULT((lit,cv,av),(getPos q,ref false)),(c1,a2,q1))
		   end
		handle AttValue a => (AD_IMPLIED,(c1,a,q1))
	   end

   (*--------------------------------------------------------------------*)
   (* parse an attribute definition, the referred element given as 1st   *)
   (* argument. 3.3:                                                     *)
   (*                                                                    *)
   (*   [53]      AttDef ::= S Name S AttType S DefaultDecl              *)
   (*                                                                    *)
   (* raise NotFound if no name is found (and thus no attribute def.)    *)
   (* print an error if white space is missing.                          *)
   (*                                                                    *)
   (* enter the attribute definition into the element table.             *)
   (* return the next character and the remaining state.                 *)
   (*--------------------------------------------------------------------*)
   (* might raise: NotFound SyntaxState                                  *)
   (*--------------------------------------------------------------------*)
   fun parseAttDef dtd (elem,ext) caq =
      let 
	 val (hadS,caq1 as (_,_,q1)) = skipPSmay dtd caq
	    
	 val (name,(c2,a2,q2)) = parseName caq1 (* NotFound falls through to the next level *)
	 val a3 = if hadS then a2 else hookError(a2,(getPos q1,ERR_MISSING_WHITE))
	 val a4 = checkAttName (a3,q1) name
	 val idx = AttNot2Index dtd name
	    
	 val caq5 = skipPS dtd (c2,a4,q2) 
	 val (attType,caq6) = parseAttType dtd elem caq5
	 val caq7 = skipPS dtd caq6 
	       
	 val (attDef,(c8,a8,q8)) = parseDefaultDecl dtd (idx,attType) caq7
	    
	 val a9 = if useParamEnts() orelse not ext 
		     then addAttribute dtd (a8,q1) (elem,(idx,attType,attDef,ext)) else a8
      in 
	 ((idx,attType,attDef),(c8,a9,q8))
      end
   
   (*--------------------------------------------------------------------*)
   (* parse an attribute-list declaration, the initial '<!ATTLIST'       *)
   (* already read. The unique entity id of the '<!' is given as first   *)
   (* arg. Cf. 3.3:                                                      *)
   (*                                                                    *)
   (*   [52] AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'           *)
   (*                                                                    *)
   (* (see also the comments for ParseDtd.parseMarkupDecl).              *)
   (*                                                                    *)
   (* check whether the element already had an attlist declaration. (cf. *)
   (* DtdElements.enterAttDecl)                                          *)
   (*                                                                    *)
   (* print an error and raise SyntaxState if no element name, or no     *)
   (* final '>' is found.                                                *)
   (* print an error if the '>' is not in the same entity as the '<!'.   *)
   (* print an error if white space is missing.                          *)
   (*                                                                    *)
   (* print an error for entity end exceptions in subfunctions.          *)
   (* catch syntax errors by recovering to the next possible state.      *)
   (*                                                                    *)
   (* enter the declared attributes into the element table.              *)
   (* return the remaining char and state.                               *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *)
   (*--------------------------------------------------------------------*)
   fun parseAttListDecl dtd (startEnt,startPos,ext) caq = 
      let 
	 val caq1 as (_,_,q1) = skipPS dtd caq 
	 val (name,(c2,a2,q2)) = parseName caq1
	    handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expAnElemName,[c])
				       in raise SyntaxError (c,hookError(a,(getPos q,err)),q)
				       end
	 val a3 = checkElemName (a2,q1) name
	 val idx = Element2Index dtd name

	 val a4 = if !O_VALIDATE orelse not ext then enterAttList dtd (a3,q1) idx else a3
	       
	 fun doit attDefs caq = 
	    let val (attDef,caq1) = parseAttDef dtd (idx,ext) caq
	       handle NotFound (c,a,q) => raise NotFound 
		  (c,hookDecl(a,((startPos,getPos q),DEC_ATTLIST(idx,rev attDefs,ext))),q)
		    | SyntaxError (c,a,q) => raise SyntaxError
		  (c,hookDecl(a,((startPos,getPos q),DEC_ATTLIST(idx,rev attDefs,ext))),q)
	    in doit (attDef::attDefs) caq1
	    end
			   
	 val (c5,a5,q5) = doit nil (c2,a4,q2) handle NotFound caq => caq
      in 
	 if c5 <> 0wx3E (* #">" *) 
	    then let val a6 = hookError(a5,(getPos q5,ERR_EXPECTED(expAttNameGt,[c5])))
		 in raise SyntaxError (c5,a6,q5)
		 end
	 else let val a6 = if not (!O_VALIDATE) orelse getEntId q5=startEnt then a5
			   else hookError(a5,(getPos q5,ERR_DECL_ENT_NESTING LOC_ATT_DECL))
	      in getChar(a6,q5)
	      end
      end
   handle exn as SyntaxError (c,a,q) => 
      let val a1 = if c=0wx00 then hookError(a,(getPos q,ERR_ENDED_BY_EE LOC_ATT_DECL))
		   else a
      in recoverDecl false (c,a,q)
      end
end

signature ParseDtd = 
   sig
      (*----------------------------------------------------------------------
      include ParseBase

      val parseName    : UniChar.Char * AppData * State 
	 -> UniChar.Data * (UniChar.Char * AppData * State)

      val openExtern   : int * Uri.Uri -> AppData * State 
	 -> Encoding.Encoding * HookData.TextDecl option * (UniChar.Char * AppData * State)
      val openDocument : Uri.Uri option -> AppData 
	 -> Encoding.Encoding * HookData.XmlDecl option * (UniChar.Char * AppData * State)

      val skipCharRef     : AppData * State -> (UniChar.Char *  AppData * State)
      val skipReference   : UniChar.Char * AppData * State -> (UniChar.Char *  AppData * State)
      val parseGenRef     : Dtd -> UniChar.Char * AppData * State 
	 -> (int * Base.GenEntity) * (AppData * State)
      val parseCharRefLit : UniChar.Data -> AppData * State 
	 -> UniChar.Data * (UniChar.Char * AppData * State)

      val parseComment   : Errors.Position -> AppData * State -> (UniChar.Char * AppData * State)
      val parseProcInstr : Errors.Position -> AppData * State -> (UniChar.Char * AppData * State)

      val skipTag   : Errors.Location -> AppData * State -> (UniChar.Char * AppData * State)
      val parseETag : Dtd -> AppData * State 
	 -> int * UniChar.Data * Errors.Position * (UniChar.Char * AppData * State)
      val parseSTag : Dtd -> Errors.Position -> UniChar.Char * AppData * State 
	 -> (HookData.StartTagInfo * Base.ElemInfo) * (UniChar.Char * AppData * State)

      val skipDecl : bool -> UniChar.Char * AppData * State -> UniChar.Char * AppData * State
      ----------------------------------------------------------------------*)
      include ParseDecl
      
      val parseDocTypeDecl : Dtd -> (UniChar.Char * AppData * State) 
	 -> int option * (UniChar.Char * AppData * State)
   end

(*--------------------------------------------------------------------------*)
(* Structure: ParseDtd                                                      *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   parseDocTypeDecl : none                                                *)
(*--------------------------------------------------------------------------*)
functor ParseDtd (structure ParseBase : ParseBase) 
   : ParseDtd =
struct 
   structure ParseDecl = ParseDecl (structure ParseBase = ParseBase)

   open 
      Base UniChar Errors UniClasses
      ParseDecl
      
   (*--------------------------------------------------------------------*)
   (* parse a markup declaration other than a processing instruction,    *)
   (* "<!" already consumed. The unique entity id of the initial '<!' is *)
   (* given as first arg. Cf. 2.8:                                       *)
   (*                                                                    *)
   (*   [29]  markupdecl ::= elementdecl | AttlistDecl | EntityDecl      *)
   (*                      | NotationDecl | PI | Comment                 *)
   (*   ...                                                              *)
   (*   Validity Constraint: Proper Declaration/PE Nesting               *)
   (*   Parameter-entity replacement text must be properly nested with   *)
   (*   markup declarations. That is to say, if either the first         *)
   (*   character or the last character of a markup declaration          *)
   (*   (markupdecl above) is contained in the replacement text for a    *)
   (*   parameter-entity reference, both must be contained in the same   *)
   (*   replacement text.                                                *)
   (*                                                                    *)
   (* and 3.2,3.3,4.2,4.7:                                               *)
   (*                                                                    *)
   (*   [45]  elementdecl ::= '<!ELEMENT' ...                            *)
   (*   [52]  AttlistDecl ::= '<!ATTLIST' ...                            *)
   (*   [70]   EntityDecl ::= GEDecl | PEDecl                            *)
   (*   [71]       GEDecl ::= '<!ENTITY' ...                             *)
   (*   [72]       PEDecl ::= '<!ENTITY' ...                             *)
   (*   [82] NotationDecl ::= '<!NOTATION' ...                           *)
   (*                                                                    *)
   (* print an error an recover if something other than "--", "ELEMENT", *)
   (* "ENTITY", "ATTLIST", or"NOTATION" is found.                        *)
   (*                                                                    *)
   (* return the remaining character and state.                          *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *)
   (*--------------------------------------------------------------------*)
   fun parseMarkupDecl dtd (startEnt,startPos) (c,a,q) =
      case c 
	of 0wx2D => (* #"-" *)
	   let val (c1,a1,q1) = getChar (a,q)
	   in if c1<>0wx2D (* #"-" *) 
		 then let val a2 = hookError(a1,(getPos q1,ERR_EXPECTED(expDash,[c1])))
		      in recoverDecl false (c1,a2,q1)
		      end
	      else parseComment startPos (a1,q1)
	   end
	 | _ => let 
		   val (name,caq1) = parseName (c,a,q)
		      handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expStartMarkup,[c])
						     val a1 = hookError(a,(getPos q,err))
						 in raise SyntaxError (c,a1,q)
						 end
		   val ext = hasExternal dtd
		in case name 
		     of [0wx45,0wx4c,0wx45,0wx4d,0wx45,0wx4e,0wx54] (* "ELEMENT" *) => 
			parseElementDecl dtd (startEnt,startPos,ext) caq1
		      | [0wx41,0wx54,0wx54,0wx4c,0wx49,0wx53,0wx54] (* "ATTLIST" *) => 
			parseAttListDecl dtd (startEnt,startPos,ext) caq1
		      | [0wx4e,0wx4f,0wx54,0wx41,0wx54,0wx49,0wx4f,0wx4e] (* "NOTATION" *) =>
			parseNotationDecl dtd (startEnt,startPos,ext) caq1
		      | [0wx45,0wx4e,0wx54,0wx49,0wx54,0wx59] (* "ENTITY" *) =>
			parseEntityDecl dtd (startEnt,startPos,ext) caq1
		      | _ => let val (c1,a1,q1) = caq1
				 val err = ERR_EXPECTED(expStartMarkup,name)
				 val a2 = hookError(a1,(getPos q,err))
			     in recoverDecl false (c1,a2,q1)
			     end
		end

   (*--------------------------------------------------------------------*)
   (* skip an ignored section, starting after the '<![IGNORE[', consume  *)
   (* the finishing "]]>". 3.4:                                          *)
   (*                                                                    *)
   (*   [63]         ignoreSect ::= '<![' S? 'IGNORE' S? '['             *)
   (*                               ignoreSectContents* ']]>'            *)
   (*   [64] ignoreSectContents ::= Ignore ('<![' ignoreSectContents     *)
   (*                               ']]>' Ignore)*                       *)
   (*   [65]             Ignore ::= Char* - (Char* ('<!['|']]>') Char* ) *)
   (*                                                                    *)
   (*   ... If the keyword of the conditional section is IGNORE, then    *)
   (*   the contents of the conditional section are not logically part   *)
   (*   of the DTD. Note that for reliable parsing, the contents of even *)
   (*   ignored conditional sections must be read in order to detect     *)
   (*   nested conditional sections and ensure that the end of the       *)
   (*   outermost (ignored) conditional section is properly detected.    *)
   (*   If a conditional section with a keyword of INCLUDE occurs within *)
   (*   a larger conditional section with a keyword of IGNORE, both the  *)
   (*   outer and the inner conditional sections are ignored.            *)
   (*                                                                    *)
   (* print an error an finish if an entity end is encountered.          *)
   (*                                                                    *)
   (* return the next char and state.                                    *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *)
   (*--------------------------------------------------------------------*)
   fun skipIgnored caq = 
      let 
	 (*--------------------------------------------------------------*)
	 (* level counts the nesting of conditional sections.            *)
	 (* if the second char after a "<" ("]") is not a "[" ("]"), it  *)
	 (* can nevertheless start another delimiter and is therefore    *)
	 (* fed into a recursive call of doit.                           *)
	 (*--------------------------------------------------------------*)
	 fun doit level (c,a,q) = 
	    case c
	      of 0wx00 => (c,hookError(a,(getPos q,ERR_ENDED_BY_EE LOC_IGNORED)),q) 
	       | 0wx3C (* #"<" *) =>  
		 let val (c1,a1,q1) = getChar (a,q)
		 in if c1=0wx21 (* #"!" *) 
		       then let val (c2,a2,q2) = (getChar(a1,q1))
			    in if c2=0wx5B (* #"[" *) then doit (level+1) (getChar(a2,q2))
			       else doit level (c2,a2,q2)
			    end
		    else doit level (c1,a1,q1)
		 end
	       | 0wx5D (* #"]" *) => 
		 let val (c1,a1,q1) = getChar (a,q)
		 in if c1=0wx5D (* #"]" *) then doit' level (getChar (a1,q1))
		    else doit level (c1,a1,q1)
		 end
	       | _ => doit level (getChar (a,q))
	 (*--------------------------------------------------------------*)
	 (* if the second "]" is followed by a "]", then this might be   *)
	 (* the real second "]". Therefore doit' loops as long as it     *)
	 (* finds "]"'s.                                                 *)
	 (*--------------------------------------------------------------*)
	 and doit' level (c,a,q) = 
	    case c
	      of 0wx3E (* #">" *) =>  if level>0 then doit (level-1) (getChar (a,q))
				      else getChar (a,q)
	       | 0wx5D (* #"]" *) => doit' level (getChar (a,q))
	       | _ => doit level (c,a,q)
      in 
	 doit 0 caq
      end
      
   (*--------------------------------------------------------------------*)
   (* parse the internal or external subset of the dtd. handle included  *)
   (* sections by counting their nesting level. Cf 2.8:                  *)
   (*                                                                    *)
   (*   Validity Constraint: Proper Declaration/PE Nesting               *)
   (*   Parameter-entity replacement text must be properly nested with   *)
   (*   markup declarations. That is to say, if either the first         *)
   (*   character or the last character of a markup declaration          *)
   (*   (markupdecl above) is contained in the replacement text for a    *)
   (*   parameter-entity reference, both must be contained in the same   *)
   (*   replacement text.                                                *)
   (*   ...                                                              *)
   (*   [28] doctypedecl ::= '<!DOCTYPE'[Image] S Name (S ExternalID)?   *)
   (*               S? ('[' (markupdecl | PEReference | S)* ']' S?)? '>' *)
   (*   [29]  markupdecl ::= elementdecl | AttlistDecl | EntityDecl      *)
   (*                      | NotationDecl | PI | Comment                 *)
   (*   [30]     extSubset ::= TextDecl? extSubsetDecl                   *)
   (*   [31] extSubsetDecl ::= ( markupdecl | conditionalSect            *)
   (*                            | PEReference | S )*                    *)
   (* and 3.4:                                                           *)
   (*                                                                    *)
   (*   [61]    conditionalSect ::= includeSect | ignoreSect             *)
   (*   [62]        includeSect ::= '<![' S? 'INCLUDE' S?                *)
   (*                               '[' extSubsetDecl ']]>'              *)
   (*   [63]         ignoreSect ::= '<![' S? 'IGNORE' S?                 *)
   (*                               '[' ignoreSectContents* ']]>'        *)
   (*                                                                    *)
   (* print an error and finish if the end of document is encountered in *)
   (* the internal subset.                                               *)
   (* print an error and raise SyntaxState if a "<" is not followed by a *)
   (* "!" or a "?".                                                      *)
   (* print an error and raise SyntaxState if a "]" is not followed by   *)
   (* "]>".                                                              *)
   (* print an error if a "<![" is found in the internal subset.         *)
   (* print an error if a "]" is found outside the internal subset.      *)
   (* print an error if a "]]>" is found outside an included section.    *)
   (* print an error an raise SyntaxState if something other than a      *)
   (* markup declaration, parameter entity reference, white space or     *)
   (* a conditional section is encountered.                              *)
   (* print an error and raise SyntaxState if a "<![" is not followed by *)
   (* "INCLUDE" or "IGNORE", or if the second "[" is missing.            *)
   (*                                                                    *)
   (* catch entity end exceptions in subfunctions by printing an error   *)
   (* and recovering.                                                    *)
   (*                                                                    *)
   (* return the remaining state and char.                               *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *)
   (*--------------------------------------------------------------------*)
   fun parseSubset dtd caq = 
      let 
	 datatype CondStatus = IGNORE | INCLUDE 

	 fun do_data caq =
	    let fun doit hadError ws (c,a,q) =
	       case c
		 of 0wx00 => (ws,(c,a,q))
		  | 0wx09 => doit false (c::ws) (getChar(a,q))
		  | 0wx0A => doit false (c::ws) (getChar(a,q))
		  | 0wx20 => doit false (c::ws) (getChar(a,q))
		  | 0wx25 => (ws,(c,a,q))
		  | 0wx3C => (ws,(c,a,q))
		  | 0wx5D => (ws,(c,a,q))
		  | _ => if hadError then doit true ws (getChar(a,q))
			 else let val err = ERR_FORBIDDEN_HERE(IT_DATA nil,LOC_SUBSET)
				  val a1 = hookError (a,(getPos q,err))
			      in doit true ws (getChar(a1,q))
			      end

		val (ws,(c1,a1,q1)) = doit false nil caq
		val a2 = if null ws then a1
			 else hookWhite(a1,Data2Vector (rev ws))
	    in (c1,a2,q1)
	    end

	 fun doit cond (c,a,q) = 
	    case c
	      of 0wx00 => 
		 if isSpecial q 
		    (*---------------------------------------------------*)
		    (* the external subset ends at and of special entity.*)
		    (* so does the internal subset, but with error.      *) 
		    (*---------------------------------------------------*)
		    then 
		       let val a1 = 
			  if inDocEntity q 
			     then hookError(a,(getPos q,ERR_ENDED_BY_EE LOC_INT_SUBSET))
			  else if cond=0 then a
			       else hookError(a,(getPos q,ERR_ENDED_BY_EE LOC_INCLUDED))
		       in (c,a1,q)
		       end
		 else let val a1 = hookEntEnd (a,getPos q)
		      in doit cond (getChar(a1,q))
		      end
		       
	       (* ignore errors in parameter references -----------------*)
	       | 0wx25 (* #"%" *) =>  
		      let 
			 val caq2 = 
			    let val ((id,ent),(a1,q1)) = parseParRef dtd (getChar(a,q))
			    in if !O_VALIDATE orelse !O_INCLUDE_PARAM_ENTS then 
			       case ent
				 of PE_NULL => getChar(a1,q1)
				  | PE_INTERN(_,rep) => 
				    let 
				       val q2 = pushIntern(q1,id,true,rep)
				       val a2 = hookParRef(a1,((getPos q,getPos q1),id,ent,true))
				    in getChar(a2,q2)
				    end
				  | PE_EXTERN extId => 
				    let 
				       val a2 = hookParRef(a1,((getPos q,getPos q1),id,ent,true))
				       val caq3 = 
					  #3(openExtern (id,true,resolveExtId extId) (a2,q1))
					  handle CantOpenFile(fmsg,a) 
					  => let val err = ERR_NO_SUCH_FILE fmsg
						 val a1 = hookError(a,(getPos q1,err))
						 val a2 = hookEntEnd (a1,getPos q1)
					     in (getChar(a2,q1))
					     end
				    in caq3
				    end
                               (* changed 080600: setExternal is already called by parseParRef *)
			       else let val a2 = hookParRef(a1,((getPos q,getPos q1),id,ent,false))
				    in getChar(a2,q1)
				    end
			    end
			 handle SyntaxError caq => caq
			      | NoSuchEntity aq => getChar aq
		      in doit cond caq2
		      end 

	       | 0wx3C (* #"<" *) =>  
		      let val (c1,a1,q1) = getChar(a,q)
		      in case c1
			   of 0wx3F => (* #"?" *)
			      let val caq2 = parseProcInstr (getPos q) (a1,q1)
			      in doit cond caq2
			      end
			    | 0wx21 => (* #"!" *) 
			      let val (c2,a2,q2) = (getChar(a1,q1))
			      in if c2=0wx5B (* #"[" *) 
				    then do_cond cond q (a2,q2)
				 else 
				    let val caq3 = parseMarkupDecl dtd 
				       (getEntId q,getPos q) (c2,a2,q2)
				    in doit cond caq3
				    end
			      end
			    | _ => let val err = ERR_EXPECTED(expExclQuest,[c1])
				       val a2 = hookError(a1,(getPos q1,err))
				       val caq3 = recoverDecl false (c1,a2,q1)
				   in doit cond caq3
				   end
		      end

	       | 0wx5D (* #"]" *) => do_brack cond q (getChar(a,q))
	       | _ => let val caq1 = do_data (c,a,q)
		      in doit cond caq1
		      end
		      
	 and do_brack cond q0 (c,a,q) =
	    if inDocEntity q then (c,a,q)
	    else if c=0wx5D (* #"]" *) 
		    then let val (c1,a1,q1) = getChar(a,q)
			 in if c1=0wx3E (* #">" *)
			       (* ignore wrong "]]>"'s ------------------*)
			       then if cond=0 
				       then let val err = ERR_FORBIDDEN_HERE(IT_DATA [c,c,c1],
									     LOC_OUT_COND)
						val a2 = hookError(a1,(getPos q0,err))
					    in doit cond (getChar(a2,q1))
					    end
				    else doit (cond-1) (getChar(a1,q1))
			    (* the second "]" may start another "]]>" ---*) 
			    else let val a2 = hookError(a1,(getPos q1,ERR_EXPECTED(expGt,[c1])))
				 in do_brack cond q (c1,a2,q1)
				 end
			 end
		 else let val a1 = hookError(a,(getPos q,ERR_EXPECTED(expRbrack,[c])))
		      in doit cond (c,a1,q)
		      end

	 and do_cond cond q0 (a,q) =
	    let 
	       (* marked sections are forbidden in the internal subset. -*)
	       val inInt = inDocEntity q
	       val a1 = if inInt then hookError (a,(getPos q0,ERR_FORBIDDEN_HERE
						    (IT_COND,LOC_INT_SUBSET)))
			else a

	       val caq2 as (_,_,q2) = skipPSopt dtd (getChar(a1,q))

	       val (status,caq3) = 
		  let 
		     val (name,(c3,a3,q3)) = parseName caq2 
		     (* ignore sections with bad status keyword ---------*)
		     val (status,a4) = 
			case name 
			  of [0wx49,0wx47,0wx4e,0wx4f,0wx52,0wx45] => (IGNORE,a3)
			   | [0wx49,0wx4e,0wx43,0wx4c,0wx55,0wx44,0wx45] => (INCLUDE,a3)
			   | _ => let val err = ERR_EXPECTED(expCondStatus,name)
				      val a4 = hookError(a3,(getPos q2,err))
				  in (IGNORE,a4)
				  end
		     val (c5,a5,q5) = skipPSopt dtd (c3,a4,q3)
		  in (* ignore sections without "[" after keyword -------*)
		     if c5=0wx5B then (status,getChar(a5,q5)) 
		     else let val a6 = hookError(a5,(getPos q5,ERR_EXPECTED(expLbrack,[c5])))
			  in (IGNORE,(c5,a6,q5))
			  end
		  end
	       handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expCondStatus,[c])
					      val a1 = hookError(a,(getPos q,err))
					  in (IGNORE,(c,a1,q))
					  end
	    in 
	       (* ignore sections in the internal subset ----------------*)
	       case (status,inInt) 
		 of (INCLUDE,_) => doit (cond+1) caq3
		  | (_,_)       => doit cond (skipIgnored caq3)
	    end
      in 
	 doit 0 caq
      end

   (*--------------------------------------------------------------------*)
   (* parse the internal subset of the dtd. Cf 2.8:                      *)
   (*                                                                    *)
   (* return the remaining character and state.                          *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *)
   (*--------------------------------------------------------------------*)
   fun parseInternalSubset dtd (a,q) = 
      let val a1 = hookSubset (a,getPos q)
      in parseSubset dtd (getChar(a1,q))
      end

   (*--------------------------------------------------------------------*)
   (* parse the external subset of the dtd, the filename given as first  *)
   (* argument. handle included sections by counting their nesting level.*)
   (* the file is opened on its own stack, and closed at the end.        *)
   (* Cf 2.8:                                                            *)
   (*                                                                    *)
   (* print an error and do nothing if the file cannot be opened.        *)
   (*                                                                    *)
   (* return nothing.                                                    *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *)
   (*--------------------------------------------------------------------*)
   fun parseExternalSubset dtd (a,q) extId = 
      let 
	 val uri = resolveExtId extId
	 val (enc,textDecl,(c1,a1,q1)) = openSubset uri a
	 val a2 = hookExtSubset (a1,(uri,enc,textDecl))
	 val (_,a3,q3) =
	   let
	     val version = case textDecl 
	       of (SOME(SOME ver,_)) => ver
	     | _ => "1.0"
	     val getCharOld = !getCharRef
	     val isNmsOld = !isNmsRef
             val isNameOld = !isNameRef
	     val isXmlOld = !isXmlRef
	     val _ = if version="1.0" then ()
		     else 
		       let
			 val _ = getCharRef := getChar11
			 val _ = isNmsRef := isNms11
			 val _ = isNameRef := isName11
			 val _ = isXmlRef := isXml11
		       in
			 ()
		       end
	     val r = parseSubset dtd (c1,a2,q1)
	     val _ = getCharRef := getCharOld
	     val _ = isNmsRef := isNmsOld
	     val _ = isNameRef := isNameOld
	     val _ = isXmlRef := isXmlOld
	   in
	     r
	   end	  
	 val _ = closeAll q3
      in a3
      end
   handle CantOpenFile(fmsg,a) => hookError(a,(getPos q,ERR_NO_SUCH_FILE fmsg))

   (*--------------------------------------------------------------------*)
   (* Parse the document type declaration, the <!DOCTYPE already read.   *)
   (* Cf. 2.8:                                                           *)
   (*                                                                    *)
   (*   [28] doctypedecl ::= '<!DOCTYPE'[Image] S Name (S ExternalID)?   *)
   (*               S? ('[' (markupdecl | PEReference | S)* ']' S?)? '>' *)
   (*                                                                    *)
   (* print an error and raise SyntaxState if no name is found.          *)
   (* print an error and raise SyntaxState if no final ">" is found.     *)
   (* external identifier is found.                                      *)
   (* print an error if white space is missing.                          *)
   (*                                                                    *)
   (* return nothing.                                                    *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *)
   (*--------------------------------------------------------------------*)
   fun parseDocTypeDecl dtd caq = 
      let 
	 val _ = setHasDtd dtd
	 val caq1 = skipS caq 
		  
	 val (doc,caq2) = parseName caq1 
	    handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expAName,[c])
				       in raise SyntaxError (c,hookError(a,(getPos q,err)),q)
				       end
	 val idx = Element2Index dtd doc

	 val (hadS,caq3 as (_,_,q3)) = skipSmay caq2
	 val (ext,(c4,a4,q4)) = let val (extId,_,(c4,a4,q4)) = parseExtIdSub dtd caq3
				    val a5 = if hadS then a4 
					     else hookError(a4,(getPos q3,ERR_MISSING_WHITE))
				in (SOME extId,(c4,a5,q4))
				end
			     handle NotFound caq => (NONE,caq)
					  
	 val a4' = hookDocType(a4,(idx,ext))
	 val (c5,a5,q5) = case c4
			    of 0wx5B (* #"[" *) => 
			       let val caq5 = parseInternalSubset dtd (a4',q4)
			       in skipSopt caq5
			       end
			     | _ => (c4,a4',q4)
				       
	 val a6 = case ext 
		    of NONE => a5
		     | SOME extId => let val _ = setExternal dtd
				     in if !O_VALIDATE orelse !O_INCLUDE_PARAM_ENTS 
                                           then parseExternalSubset dtd (a5,q5) extId
					else a5
				     end
				       
	 val a7 = checkMultEnum dtd (a6,q5)
	 val a7'= checkPreDefined dtd (a7,q5)
	 val a8 = checkUnparsed dtd a7'
				       
	 val (c9,a9,q9) = if c5=0wx3E (* #">" *) then getChar(a8,q5)
			  else let val err = expectedOrEnded(expGt,LOC_DOC_DECL) c5
				   val a9 = hookError(a8,(getPos q5,err)) 
			       in recoverDecl false (c5,a9,q5)
			       end
      in 
	 (SOME idx,(c9,hookEndDtd(a9,getPos q9),q9))
      end
   handle exn as SyntaxError(c,a,q) => 
      let val a1 = if c=0wx00 then hookError(a,(getPos q,ERR_ENDED_BY_EE LOC_DOC_DECL))
		   else a
	  val (c2,a2,q2) = recoverDecl true (c,a1,q)
      in (NONE,(c2,hookEndDtd(a2,getPos q2),q2))
      end
end

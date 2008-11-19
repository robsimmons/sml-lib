signature ParseContent =
   sig
      (*----------------------------------------------------------------------
      include ParseBase
      
      val parseName    : UniChar.Char * AppData * State 
	 -> UniChar.Data * (UniChar.Char * AppData * State)

      val openDocument : Uri.Uri option -> AppData 
	 -> Encoding.Encoding * HookData.XmlDecl option * (UniChar.Char * AppData * State)

      val skipCharRef   : AppData * State -> (UniChar.Char *  AppData * State)
      val skipReference : UniChar.Char * AppData * State -> (UniChar.Char *  AppData * State)

      val parseComment   : Errors.Position -> AppData * State -> (UniChar.Char * AppData * State)
      val parseProcInstr : Errors.Position -> AppData * State -> (UniChar.Char * AppData * State)

      val skipTag   : Errors.Location -> AppData * State -> (UniChar.Char * AppData * State)
      val parseSTag : Dtd -> Errors.Position -> UniChar.Char * AppData * State 
	 -> (HookData.StartTagInfo * Base.ElemInfo) * (UniChar.Char * AppData * State)

      val skipDecl : bool -> UniChar.Char * AppData * State -> UniChar.Char * AppData * State

      val parseDocTypeDecl : Dtd -> (UniChar.Char * AppData * State) 
	 -> int option * (UniChar.Char * AppData * State)
      ----------------------------------------------------------------------*)
      include ParseDtd

      val skipBadSection : UniChar.Char * AppData * State -> (UniChar.Char * AppData * State)

      val parseElement : Dtd * int list * State * (HookData.StartTagInfo * Base.ElemInfo) 
	 * (UniChar.Char * AppData * State)
	 -> (int * UniChar.Data * Errors.Position * Errors.Position) option 
	 * (UniChar.Char * AppData * State)
   end

(*--------------------------------------------------------------------------*)
(* Structure: ParseContent                                                  *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   skipBadSection : none                                                  *)
(*   parseElement   : none                                                  *)
(*--------------------------------------------------------------------------*)
functor ParseContent (structure ParseBase : ParseBase) 
   : ParseContent =
struct
   structure ParseDtd = ParseDtd (structure ParseBase = ParseBase)

   open
      Base Errors UniChar UniClasses UtilList
      ParseDtd
      
   val THIS_MODULE = "ParseContent"
   val DATA_BUFSIZE = 1024
   val dataBuffer = Array.array(DATA_BUFSIZE,0w0:UniChar.Char)

   (*--------------------------------------------------------------------*)
   (* skip a cdata section, the initial "<![" already consumed. The first*)
   (* arg is the type of section to be skipped. cf. 2.5:                 *)
   (*                                                                    *)
   (*   [18]  CDSect ::= CDStart CData CDEnd                             *)
   (*   [19] CDStart ::= '<![CDATA['                                     *)
   (*   [20]   CData ::= (Char* - (Char* ']]>' Char* ))               [[ *)
   (*   [21]   CDEnd ::= ']]>'                                           *)
   (*                                                                    *)
   (* don't care abeout whether "CDATA[" is present. just skip until the *)
   (* next "]]>" or entity end.                                          *)
   (*                                                                    *)
   (* return the remaining char and state.                               *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *)
   (*--------------------------------------------------------------------*)
   fun skipBadSection caq =
      let(*--------------------------------------------------------------*)
	 (* for a sequence of "]"s, check whether the last two are       *)
	 (* followed by a ">"                                            *)
	 (*--------------------------------------------------------------*)
	 fun checkEnd aq = 
	    let val (c1,a1,q1) = getChar aq
	    in case c1
		 of 0wx3E (* #">" *) => getChar(a1,q1)
		  | 0wx5D (* #"]" *) => checkEnd(a1,q1)
		  | _ => doit(c1,a1,q1)
	    end
	 and doit (c,a,q) = 
	    case c 
	      of 0wx00 => let val a1 = hookError(a,(getPos q,ERR_ENDED_BY_EE LOC_CDATA))
			  in (c,a1,q)
			  end
	       | 0wx5D (* #"]" *) => let val (c1,a1,q1) = getChar(a,q)
				     in if c1=0wx5D (* #"]" *) then checkEnd(a1,q1)
					else doit (c1,a1,q1)
				  end
	       | _ => doit (getChar(a,q))
      in doit caq
      end
   
   (*--------------------------------------------------------------------*)
   (* parse a cdata section, the initial "<![CDATA[" already consumed.   *)
   (* cf. 2.5:                                                           *)
   (*                                                                    *)
   (*   [18]  CDSect ::= CDStart CData CDEnd                             *)
   (*   [19] CDStart ::= '<![CDATA['                                     *)
   (*   [20]   CData ::= (Char* - (Char* ']]>' Char* ))               [[ *)
   (*   [21]   CDEnd ::= ']]>'                                           *)
   (*                                                                    *)
   (* print an error and finish if an entity end is found.               *)
   (*                                                                    *)
   (* return the data as a Vector option and the next char & state.      *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *)
   (*--------------------------------------------------------------------*)
   fun parseCDataSection' (aq as (_,q)) =
      let 
         (*--------------------------------------------------------------*)
         (* for a sequence of "]"s, check whether the last two are       *)
         (* followed by a ">"                                            *)
         (*--------------------------------------------------------------*)
         fun doEnd (text,q0,q1) (a2,q2) = 
            let val (c3,a3,q3) = getChar (a2,q2)
            in case c3
                 of 0wx00 => 
                    let val a4 = hookError(a3,(getPos q3,ERR_ENDED_BY_EE LOC_CDATA))
                    in (0wx5D::text,getPos q2,(c3,a4,q3))
                    end
                  | 0wx3E => (* #">" *) (text,getPos q0,getChar(a3,q3))
                  | 0wx5D => doEnd (0wx5D::text,q1,q2) (a3,q3)
                  | _ => doit (c3::0wx5D::0wx5D::text) (a3,q3)
            end
         and doBrack (text,q0) (a1,q1) =
            let val (c2,a2,q2) = getChar(a1,q1)
            in case c2
                 of 0wx00 => 
                    let val a3 = hookError(a2,(getPos q2,ERR_ENDED_BY_EE LOC_CDATA))
                    in (0wx5D::text,getPos q1,(c2,a3,q2))
                    end
                  | 0wx5D (* #"]" *) => doEnd (text,q0,q1) (a2,q2)
                  | _ => doit (c2::0wx5D::text) (a2,q2)
            end
         and doit text (a,q) =
            let val (c1,a1,q1) = getChar(a,q)
            in case c1 
                 of 0wx00 => 
                    let val a2 = hookError(a1,(getPos q1,ERR_ENDED_BY_EE LOC_CDATA))
                    in (text,getPos q,(c1,a2,q1))
                    end
                  | 0wx5D (* #"]" *) => doBrack (text,q) (a1,q1)
                  | _ => doit (c1::text) (a1,q1)
            end
         val (c1,a1,q1) = getChar aq
         val startPos = getPos q1
         val (cs,endPos,(c2,a2,q2)) = 
            case c1 
              of 0wx00 => 
                 let val a2 = hookError(a1,(getPos q1,ERR_ENDED_BY_EE LOC_CDATA))
                 in (nil,getPos q,(c1,a2,q1))
                 end
               | 0wx5D (* #"]" *) => doBrack (nil,q) (a1,q1)
               | _ => doit [c1] (a1,q1)
         val text = Data2Vector(rev cs)
         val a3 = hookCData(a1,((startPos,endPos),text))
      in (c2,a3,q2)
      end
   (*--------------------------------------------------------------------*)
   (* parse a cdata section, the initial "<![" already consumed.         *)
   (* cf. 2.5:                                                           *)
   (*                                                                    *)
   (*   [18]  CDSect ::= CDStart CData CDEnd                             *)
   (*   [19] CDStart ::= '<![CDATA['                                     *)
   (*   [20]   CData ::= (Char* - (Char* ']]>' Char* ))               [[ *)
   (*   [21]   CDEnd ::= ']]>'                                           *)
   (*                                                                    *)
   (* print an error and skip the section if no name or a name other     *)
   (* than CDATA comes first, or no '[' follows the name.                *)
   (*                                                                    *)
   (* return the text of the section together with the remaining state.  *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *)
   (*--------------------------------------------------------------------*)
   fun parseCDataSection startPos aq =
      let 
	 val caq0 as (_,_,q0) = (getChar aq) 
	 val (name,(c1,a1,q1)) = parseName caq0
	    handle NotFound (c,a,q) => let val err = expectedOrEnded(expCdata,LOC_CDATA) c
				       in raise SyntaxError(c,hookError(a,(getPos q,err)),q)
				       end
				    
	 val _ = if name = [0wx43,0wx44,0wx41,0wx54,0wx41] (* "CDATA" *) then ()
		 else let val err = ERR_EXPECTED(expCdata,name)
		      in raise SyntaxError(c1,hookError(a1,(getPos q0,err)),q1)
		      end
		   
	 val _ = if c1=0wx5B (* #"[" *) then ()
		 else let val err = expectedOrEnded(expLbrack,LOC_CDATA) c1
		      in raise SyntaxError(c1,hookError(a1,(getPos q1,err)),q1)
		      end
      in 
	 parseCDataSection'(a1,q1)
      end      
   handle SyntaxError caq => skipBadSection caq
	 
   (*--------------------------------------------------------------------*)
   (* parse element or empty content. The second arg holds the unique    *)
   (* number of the element's first characters's entity, the index of    *)
   (* the current element, and the dfa for its content. Cf. 3:           *)
   (*                                                                    *)
   (*   [39] element ::= EmptyElemTag                                    *)
   (*                  | STag content ETag                               *)
   (*   ...                                                              *)
   (*   Well-Formedness Constraint: Element Type Match                   *)
   (*   The Name in an element's end-tag must match the element type in  *)
   (*   the start-tag.                                                   *)
   (*                                                                    *)
   (*   Validity Constraint: Element Valid                               *)
   (*   An element is valid if there is a declaration matching           *)
   (*   elementdecl where the Name matches the element type, and one of  *)
   (*   the following holds:                                             *)
   (*                                                                    *)
   (*   1. The declaration matches EMPTY and the element has no content. *)
   (*   2. The declaration matches children and the sequence of child    *)
   (*      elements belongs to the language generated by the regular     *)
   (*      expression in the content model, with optional white space    *)
   (*      (characters matching the nonterminal S) between each pair of  *)
   (*      child elements.                                               *)
   (*                                                                    *)
   (* and 3.1:                                                           *)
   (*                                                                    *)
   (*   [43] content ::= (element | CharData | Reference | CDSect | PI   *)
   (*                     | Comment)*                                    *)
   (* 2.4:                                                               *)
   (*   The ampersand character (&) and the left angle bracket (<) may   *)
   (*   appear in their literal form only when used as markup delimiters,*)
   (*   or within a comment, a processing instruction, or a CDATA        *)
   (*   section... If they are needed elsewhere, they must be escaped    *)
   (*   using either numeric character references or the strings "&amp;" *)
   (*   and "&lt;" respectively...                                       *)
   (*                                                                    *)
   (* consume the content of the element, accumulating it via the user   *)
   (* data functions (parameter a in subfunctions). trace the content    *)
   (* model of the element with a dfa transitions on a dfa state (para-  *)
   (* meter p in subfunctions). finish at the first end-tag, whether     *)
   (* matching or not, or at the document end.                           *)
   (*                                                                    *)
   (* handle all syntax and other recoverable errors from subfunctions   *)
   (* and try to continue.                                               *)
   (*                                                                    *)
   (* return the accumulated user data and the next char and state.      *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *)
   (*--------------------------------------------------------------------*)
   fun parseElementContent dtd (openElems,startEnt,curr,dfa,ext,mt) caq = 
      let       
	 (*--------------------------------------------------------------*)
	 (* check whether the dfa allows a transition/an end tag here.   *)
	 (* print an error if not. After a transition return the new     *)
	 (* dfa state.                                                   *)
	 (*--------------------------------------------------------------*)
	 fun fin_elem (a,pos,dfa,p) = 
	    if dfaFinal(dfa,p) then a
	    else hookError(a,(pos,ERR_ENDED_EARLY(Index2Element dtd curr)))
	 fun trans_elem (a,q,dfa,p,el) = 
	    let val p1 = dfaTrans(dfa,p,el)
	    in if p1<>dfaError then (p1,a)
	       else let val err = ERR_BAD_ELEM(Index2Element dtd curr,Index2Element dtd el)
		    in (p1,hookError(a,(getPos q,err)))
		    end 
	    end

	 (*--------------------------------------------------------------*)
	 (* consume all white space and skip all data until the next "<" *)
	 (* or "&". print an error for each sequence of data encountered.*)
	 (*                                                              *)
	 (* add the white space as data to the user data.                *)
	 (* return the next char and state.                              *)
	 (*--------------------------------------------------------------*)
	 fun do_char_elem (c0,a0,q0) = 
	    let 
	       (*--------------------------------------------------------------*)
	       (* read data characters until the next "<", "&" or entity end.  *)
	       (* add the data to the user data when an error occurs or no     *)
	       (* more data follows.                                           *)
	       (*                                                              *)
	       (* return the modified user data with the next char and state.  *)
	       (*--------------------------------------------------------------*)
	       fun data_hook(a,q,cs) = 
		  if null cs then a 
		  else hookData(a,((getPos q0,getPos q),Data2Vector(rev cs),true))
               fun after_error (caq as (c,a,q)) =
                  case c
                    of 0wx00            => caq
                     | 0wx26 (* #"&" *) => caq
                     | 0wx3C (* #"<" *) => caq
                     | _ => after_error(getChar(a,q))
	       fun do_data (yet,aq as (_,q)) = 
		  let val (c1,a1,q1) = getChar aq
		  in case c1
		       of 0wx00            => (c1,data_hook(a1,q,yet),q1)
			| 0wx26 (* #"&" *) => (c1,data_hook(a1,q,yet),q1)
			| 0wx3C (* #"<" *) => (c1,data_hook(a1,q,yet),q1)
			| _ => 
			  if isS c1 then do_data (c1::yet,(a1,q1))
			  else let val a2 = data_hook(a1,q,yet)
                                   val err = ERR_ELEM_CONTENT(IT_DATA nil)
                                   val a3 = hookError(a2,(getPos q1,err))
                               in after_error (getChar(a3,q1))
                               end
		  end
	    in 
	       if isS c0 then 
		  let val a1 = if not (ext andalso standsAlone dtd) then a0
			       else let val err = ERR_STANDALONE_ELEM(Index2Element dtd curr)
					val _ = setStandAlone dtd (not (!O_ERROR_MINIMIZE))
				    in hookError(a0,(getPos q0,err))
				    end
		  in do_data ([c0],(a1,q0))
		  end
	       else let val a1 = hookError(a0,(getPos q0,ERR_ELEM_CONTENT(IT_DATA nil)))
		    in after_error(getChar(a1,q0))
		    end
	    end
	 (*--------------------------------------------------------------*)
	 (* consume a reference, handling errors by ignoring them.       *)
	 (*--------------------------------------------------------------*)
	 fun do_ref (q,(c1,a1,q1)) = 
	    if c1=0wx23 (* #"#" *)
	       (*------------------------------------------------------*)
	       (* it's a character reference.                          *)
	       (*------------------------------------------------------*)
	       then let val err = ERR_ELEM_CONTENT IT_CHAR_REF
		        val a2 = hookError(a1,(getPos q,err))
		    in skipCharRef(a2,q1)
		    end
	    (*---------------------------------------------------------*)
	    (* it's a general entity reference.                        *)
	    (*---------------------------------------------------------*)
	    else let val ((id,ent),(a2,q2)) = parseGenRef dtd (c1,a1,q1)
		 in case ent
		      of GE_NULL => 
			 let val a3 = hookGenRef(a2,((getPos q,getPos q2),id,ent,false))
			 in (getChar(a3,q2))
			 end
		       | GE_INTERN(_,rep) => 
			 let 
			    val q3 = pushIntern(q2,id,false,rep)
			    val a3 = hookGenRef(a2,((getPos q,getPos q2),id,ent,true))
			 in (getChar(a3,q3))
			 end
		       | GE_EXTERN ext => 
			 if !O_VALIDATE orelse !O_INCLUDE_EXT_PARSED 
			    then 
			       let 
				  val a3 = hookGenRef(a2,((getPos q,getPos q2),id,ent,true))
				  val caq4 = #3(openExtern (id,false,resolveExtId ext) (a3,q2))
				     handle CantOpenFile(fmsg,a) 
				     => let val err = ERR_NO_SUCH_FILE fmsg
					    val a2 = hookError(a,(getPos q2,err))
					    val a3 = hookEntEnd(a2,getPos q2)
					in (getChar(a3,q2))
					end
			       in caq4
			       end
			 else let val a3 = hookGenRef(a2,((getPos q,getPos q2),id,ent,false))
			      in getChar(a3,q2)
			      end
		       | GE_UNPARSED _ => 
			      raise InternalError
				 (THIS_MODULE,"parseElementContent",
				  "parseGenRef returned GE_UNPARSED")
		 end
	     (*-------------------------------------------------------*)
	     (* handle any errors in references by ignoring them.     *)
	     (*-------------------------------------------------------*)
	  handle SyntaxError caq => caq
	       | NoSuchEntity aq => getChar aq
		      		    
	 (*--------------------------------------------------------------*)
	 (* handle an end-tag. finish the element in the user data and   *)
	 (* return.                                                      *)
	 (*                                                              *)
	 (* print an error if the element's content is not yet finished. *)
	 (* print an error if the end-tag is for another element.        *)
	 (* print an error if the element's first character was not in   *)
	 (* the same entity.                                             *)
	 (*--------------------------------------------------------------*)
	 and do_etag (p,etag as (elem,space,startPos,endPos),(c,a,q)) = 
	    let 
	       fun checkNesting a = 
		  if getEntId q=startEnt then a
		  else hookError(a,(startPos,ERR_ELEM_ENT_NESTING(Index2Element dtd curr)))
	    in 
	       if elem=curr then let val a1 = fin_elem (a,startPos,dfa,p)
				     val a2 = checkNesting a1
				     val a3 = hookEndTag
					(a2,((startPos,endPos),curr,SOME(elem,space)))
				 in (NONE,(c,a3,q))
				 end
	       else if member elem openElems 
		       then let val err = ERR_OMITTED_END_TAG(Index2Element dtd curr)
				val a1 = hookError(a,(startPos,err))
				val a2 = fin_elem (a1,startPos,dfa,p)
				val a3 = hookEndTag(a2,((startPos,endPos),curr,NONE))
			    in (SOME etag,(c,a3,q))
			    end
	       else if dfaFinal(dfa,p)
		       then let val err = ERR_ELEM_TYPE_MATCH(Index2Element dtd curr,
							      Index2Element dtd elem)
				val a1 = hookError(a,(startPos,err))
				val a2 = checkNesting a1
				val a3 = hookEndTag(a2,((startPos,endPos),curr,SOME(elem,space)))
			    in (NONE,(c,a3,q))
			    end
		    else let val err = ERR_IGNORED_END_TAG(Index2Element dtd curr,
							   Index2Element dtd elem)
			     val a1 = hookError(a,(startPos,err))
			 in do_elem(p,(c,a1,q))
			 end
	    end

	 (*--------------------------------------------------------------*)
	 (* handle a declaration, proc. instr or tag.                    *)
	 (*--------------------------------------------------------------*)
	 and do_lt (p,q,(c1,a1,q1)) =
	    case c1
	      of 0wx21 (* #"!" *) => 
	         (*------------------------------------------------------*)
		 (* its a declaration, cdata section or comment.         *)
                 (* Only comments are valid.                             *)
	         (*------------------------------------------------------*)
		 let val (c2,a2,q2) = getChar(a1,q1)
		     val caq3 =  
			case c2
			  of 0wx2D (* #"-" *) => 
			     let val (c3,a3,q3) = getChar(a2,q2)
			     in if c3=0wx2D then parseComment (getPos q) (a3,q3)
				else let val err = ERR_EXPECTED(expDash,[c3])
					 val a4 = hookError(a3,(getPos q3,err))
				     in recoverDecl false (c3,a4,q3)
				     end
			     end
			   | 0wx5B (* #"[" *) => 
			     let val a3 = hookError(a2,(getPos q2,ERR_ELEM_CONTENT IT_CDATA))
			     in skipBadSection (getChar(a3,q2))
			     end
			   | _ => (c2,hookError(a2,(getPos q2,ERR_EXPECTED(expDash,[c2]))),q2)
		 in do_elem(p,caq3)
		 end
	       | 0wx2F (* #"/" *) => 
		 (let val (elem,space,endPos,caq2) = parseETag dtd (a1,q1)
		  in do_etag (p,(elem,space,getPos q,endPos),caq2)
		  end
		     handle SyntaxError caq => do_elem(p,caq))
	       | 0wx3F (* #"?" *) => do_elem (p,parseProcInstr (getPos q) (a1,q1))
	       | _ => 
		 (*------------------------------------------------------*)
		 (* it's a start tag. the recursive call to parseElement *)
		 (* might return an end-tag that has to be consumed.     *)
		 (*------------------------------------------------------*)
		 if isNms c1 then 
		    let val (p1,(opt,caq2)) = 
		       (let val (stag as ((_,elem,_,_,_),_),(c2,a2,q2)) = 
			   parseSTag dtd (getPos q) (c1,a1,q1)
			    val (p1,a3) = trans_elem (a2,q1,dfa,p,elem)
			in (p1,parseElement (dtd,curr::openElems,q,stag,(c2,a3,q2)))
			end)
			   handle SyntaxError caq => (p,(NONE,caq))
		    in case opt
			 of NONE => do_elem (p1,caq2)
			  | SOME etag => do_etag (p1,etag,caq2)
		    end
		 else let val err = ERR_FORBIDDEN_HERE(IT_CHAR 0wx3C,LOC_CONTENT)
			  val a2 = hookError(a1,(getPos q,err))
		      in do_elem (p,(c1,a2,q1)) 
		      end

	 (*--------------------------------------------------------------*)
	 (* do element content. handle the document end by printing an   *)
	 (* error and finishing like with an end-tag.                    *)
	 (*--------------------------------------------------------------*)
	 and do_elem (p,(c,a,q)) = 
	    case c 
	      of 0wx00 => if isSpecial q 
			     then let val err = ERR_OMITTED_END_TAG(Index2Element dtd curr) 
				      val a1 = hookError(a,(getPos q,err))
				      val pos = getPos q
				      val a2 = fin_elem (a1,pos,dfa,p)
				      val a3 = hookEndTag(a2,((pos,pos),curr,NONE))
				  in (NONE,(c,a3,q))
				  end
			  else let val a1 = hookEntEnd(a,getPos q)
			       in do_elem (p,getChar(a1,q))
			       end
	       | 0wx26 (* #"&" *) => do_elem (p,do_ref (q,getChar(a,q)))
	       | 0wx3C (* #"<" *) => do_lt (p,q,getChar(a,q))
	       | _ => do_elem (p,do_char_elem (c,a,q))
			 		      
	 (*--------------------------------------------------------------*)
	 (* do empty content. if the first thing to come is the current  *)
	 (* element's end-tag, finish it. Otherwise print an error and   *)
	 (* continue as for element content.                             *) 
	 (*--------------------------------------------------------------*)
	 and do_empty (c,a,q) = 
	    if c<>0wx3C (* #"<" *) 
	       then let val a1 = hookError(a,(getPos q,ERR_NONEMPTY(Index2Element dtd curr)))
		    in do_elem (dfaInitial,(c,a1,q))
		    end
	    else 
	       let val (c1,a1,q1) = getChar(a,q)
	       in if c1<>0wx2F (* #"/" *) 
		     then let val err = ERR_NONEMPTY(Index2Element dtd curr)
			      val a2 = hookError(a1,(getPos q,err))
			  in do_lt (dfaInitial,q,(c1,a2,q1))
			  end
		  else let val (elem,space,endPos,caq2) = parseETag dtd (a1,q1)
		       in do_etag (dfaInitial,(elem,space,getPos q,endPos),caq2)
		       end 
		    handle SyntaxError caq => do_elem (dfaInitial,caq)
	       end
	       
      in if mt then do_empty caq 
	 else do_elem (dfaInitial,caq)
      end

   (*--------------------------------------------------------------------*)
   (* parse mixed or any content. The second arg holds the unique number *)
   (* of the element's first characters's entity, the idx of the current *)
   (* element, and a function for validating child elements. Cf. 3:      *)
   (*                                                                    *)
   (*   [39] element ::= EmptyElemTag                                    *)
   (*                  | STag content ETag                               *)
   (*   ...                                                              *)
   (*   Well-Formedness Constraint: Element Type Match                   *)
   (*   The Name in an element's end-tag must match the element type in  *)
   (*   the start-tag.                                                   *)
   (*                                                                    *)
   (*   Validity Constraint: Element Valid                               *)
   (*   An element is valid if there is a declaration matching           *)
   (*   elementdecl where the Name matches the element type, and one of  *)
   (*   the following holds:                                             *)
   (*   ...                                                              *)
   (*   3. The declaration matches Mixed and the content consists of     *)
   (*      character data and child elements whose types match names in  *)
   (*      the content model.                                            *)
   (*   4. The declaration matches ANY, and the types of any child       *)
   (*      elements have been declared.                                  *)
   (*                                                                    *)
   (* 3.1:                                                               *)
   (*                                                                    *)
   (*   [43] content ::= (element | CharData | Reference | CDSect | PI   *)
   (*                     | Comment)*                                    *)
   (* 2.4:                                                               *)
   (*   The ampersand character (&) and the left angle bracket (<) may   *)
   (*   appear in their literal form only when used as markup delimiters,*)
   (*   or within a comment, a processing instruction, or a CDATA        *)
   (*   section... If they are needed elsewhere, they must be escaped    *)
   (*   using either numeric character references or the strings "&amp;" *)
   (*   and "&lt;" respectively. The right angle bracket (>) may be      *)
   (*   represented using the string "&gt;", and must, for compatibility,*)
   (*   be escaped using "&gt;" or a character reference when it appears *)
   (*   in the string "]]>" in content, when that string is not marking  *)
   (*   the end of a CDATA section.                                      *)
   (*                                                                    *)
   (* consume the content of the element, accumulating it via the user   *)
   (* data functions (parameter a in subfunctions). for each child,      *)
   (* check whether it was specified in the element's Mixed content      *)
   (* specification (validate). finish at the first end-tag, whether     *)
   (* matching or not, or at the document end.                           *)
   (*                                                                    *)
   (* handle all syntax and other recoverable errors from subfunctions   *)
   (* and try to continue.                                               *)
   (*                                                                    *)
   (* return the accumulated user data and the next char and state.      *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *)
   (*--------------------------------------------------------------------*)
   and parseMixedContent dtd (openElems,startEnt,curr,validate) caq =
      let 
	 (*--------------------------------------------------------------*)
	 (* read data characters until the next "<", "&" or entity end.  *)
	 (* add the data to the user data when an error occurs or no     *)
	 (* more data follows.                                           *)
	 (*                                                              *)
	 (* return the modified user data with the next char and state.  *)
	 (*--------------------------------------------------------------*)
	 fun do_data (br,(c0,a0,q0)) =
	    let 
	       val pos0 = ref (getPos q0)
	       val _ = Array.update(dataBuffer,0,c0)

	       fun data_hook (i,(a,q)) = 
		  hookData(a,((!pos0,getPos q),ArraySlice.vector(ArraySlice.slice(dataBuffer,0,SOME i)),false))
	       fun takeOne (c,qE,i,aq as (a,q)) = 
		  if i<DATA_BUFSIZE then (i+1,aq) before Array.update(dataBuffer,i,c)
		  else let val a1 = data_hook(i,(a,qE))
			   val _ = pos0 := getPos q
			   val _ =  Array.update(dataBuffer,0,c)
		       in (1,(a1,q))
		       end
	       fun do_br (n,(i,aq as (_,q))) = 
		  let val (c1,a1,q1) = getChar aq 
		  in case c1
		       of 0wx00            => (c1,data_hook(i,(a1,q)),q1)
			| 0wx26 (* #"&" *) => (c1,data_hook(i,(a1,q)),q1)
			| 0wx3C (* #"<" *) => (c1,data_hook(i,(a1,q)),q1)
			| 0wx5D (* #"]" *) => do_br (n+1,takeOne(c1,q,i,(a1,q1)))
			| 0wx3E (* #">" *) => 
			  let val a2 = if n=1 then a1 
				       else hookError(a1,(getPos q1,ERR_MUST_ESCAPE c1)) 
			  in doit (takeOne(c1,q,i,(a2,q1)))
			  end
			| _ => doit (takeOne(c1,q,i,(a1,q1)))
		  end
	       and doit (i,aq as (_,q)) = 
		  let val (c1,a1,q1) = getChar aq
		  in case c1
		       of 0wx00            => (c1,data_hook(i,(a1,q)),q1)
			| 0wx26 (* #"&" *) => (c1,data_hook(i,(a1,q)),q1)
			| 0wx3C (* #"<" *) => (c1,data_hook(i,(a1,q)),q1)
			| 0wx5D (* #"]" *) => if !O_COMPATIBILITY 
						 then do_br (1,takeOne(c1,q,i,(a1,q1)))
					      else doit (takeOne(c1,q,i,(a1,q1)))
			| _ => doit (takeOne(c1,q,i,(a1,q1)))
		  end
	    in 
	       if br then do_br (1,(1,(a0,q0)))
	       else doit (1,(a0,q0))
	    end
	 (*
         fun do_data (br,(c0,a0,q0)) =
	    let 
	       fun data_hook (yet,(a,q)) = 
		  hookData(a,((getPos q0,getPos q),Data2Vector(rev yet),false))
	       fun do_br (n,yet,aq as (_,q)) = 
		  let val (c1,a1,q1) = getChar aq 
		  in case c1
		       of 0wx00            => (c1,data_hook(yet,(a1,q)),q1)
			| 0wx26 (* #"&" *) => (c1,data_hook(yet,(a1,q)),q1)
			| 0wx3C (* #"<" *) => (c1,data_hook(yet,(a1,q)),q1)
			| 0wx5D (* #"]" *) => do_br (n+1,c1::yet,(a1,q1))
			| 0wx3E (* #">" *) => 
			  let val a2 = if n=1 then a1 
				       else hookError(a1,(getPos q1,ERR_MUST_ESCAPE c1)) 
			  in doit (c1::yet,(a2,q1))
			  end
			| _ => doit (c1::yet,(a1,q1))
		  end
	       and doit (yet,aq as (_,q)) = 
		  let val (c1,a1,q1) = getChar aq
		  in case c1
		       of 0wx00            => (c1,data_hook(yet,(a1,q)),q1)
			| 0wx26 (* #"&" *) => (c1,data_hook(yet,(a1,q)),q1)
			| 0wx3C (* #"<" *) => (c1,data_hook(yet,(a1,q)),q1)
			| 0wx5D (* #"]" *) => if !O_COMPATIBILITY 
						 then do_br (1,c1::yet,(a1,q1))
					      else doit (c1::yet,(a1,q1))
			| _ => doit (c1::yet,(a1,q1))
		  end
	    in 
	       if br then do_br (1,[0wx5D],(a0,q0))
	       else doit ([c0],(a0,q0))
	    end
	 *)

	 (*--------------------------------------------------------------*)
	 (* consume a reference, handling errors by ignoring them.       *)
	 (*--------------------------------------------------------------*)
	 fun do_ref (q0,(c,a,q)) = 
	    if c=0wx23 (* #"#" *)
	       (*------------------------------------------------------*)
	       (* it's a character reference.                          *)
	       (*------------------------------------------------------*)
	       then let val (cs,(ch,a1,q1)) = parseCharRefLit [0wx23,0wx26] (a,q)
			val cv = Data2Vector(rev cs)
			val a2 = hookCharRef(a1,((getPos q0,getPos q1),ch,cv))
		    in getChar(a2,q1)
		    end
		 handle SyntaxError caq => caq
		      | NoSuchChar aq => getChar aq 
	    (*---------------------------------------------------------*)
	    (* it's a general entity reference.                        *)
	    (*---------------------------------------------------------*)
	    else let val ((id,ent),(a1,q1)) = parseGenRef dtd (c,a,q)
		 in case ent
		      of GE_NULL => 
			 let val a2 = hookGenRef(a1,((getPos q0,getPos q1),id,ent,false))
			 in getChar(a2,q1)
			 end
		       | GE_INTERN(_,rep) => 
			 let 
			    val q2 = pushIntern(q1,id,false,rep)
			    val a2 = hookGenRef(a1,((getPos q0,getPos q1),id,ent,true))
			 in getChar(a2,q2)
			 end
		       | GE_EXTERN ext => 
			 if !O_VALIDATE orelse !O_INCLUDE_EXT_PARSED 
			    then 
			       let 
				  val a2 = hookGenRef(a1,((getPos q0,getPos q1),id,ent,true))
				  val caq3 = #3(openExtern (id,false,resolveExtId ext) (a2,q1))
				     handle CantOpenFile(fmsg,a) 
				     => let val err = ERR_NO_SUCH_FILE fmsg
					    val a1 = hookError(a,(getPos q1,err))
					    val a2 = hookEntEnd(a1,getPos q1)
					in (getChar(a2,q1))
					end
			       in caq3
			       end
			 else let val a2 = hookGenRef(a1,((getPos q0,getPos q1),id,ent,false))
			      in getChar(a2,q1)
			      end
		       | GE_UNPARSED _ => 
			      raise InternalError
				 ("THIS_MODULE","parseMixedContent",
				  "parseGenRef returned GE_UNPARSED")
		 end
	     (*-------------------------------------------------------*)
	     (* handle any errors in references by ignoring them.     *)
	     (*-------------------------------------------------------*)
	  handle SyntaxError caq => caq
	       | NoSuchEntity aq => getChar aq 
		      		    
	 (*--------------------------------------------------------------*)
	 (* handle an end-tag. finish the element in the user data and   *)
	 (* return.                                                      *)
	 (*                                                              *)
	 (* print an error if the element's content is not yet finished. *)
	 (* print an error if the end-tag is for another element.        *)
	 (* print an error if the element's first character was not in   *)
	 (* the same entity.                                             *)
	 (*--------------------------------------------------------------*)
	 and do_etag (etag as (elem,space,startPos,endPos),(c,a,q)) = 
	    let 
	       fun checkNesting a = 
		  if getEntId q=startEnt then a
		  else hookError(a,(startPos,ERR_ELEM_ENT_NESTING(Index2Element dtd curr)))
	    in 
	       if elem=curr then let val a1 = checkNesting a
				     val a2 = hookEndTag
					(a1,((startPos,endPos),curr,SOME(elem,space)))
				 in (NONE,(c,a2,q))
				 end
	       else if member elem openElems 
		       then let val err = ERR_OMITTED_END_TAG(Index2Element dtd curr)
				val a1 = hookError(a,(startPos,err))
				val a2 = hookEndTag(a1,((startPos,endPos),curr,NONE))
			    in (SOME etag,(c,a2,q))
			    end
		    else let val err = ERR_ELEM_TYPE_MATCH(Index2Element dtd curr,
							   Index2Element dtd elem)
			     val a1 = hookError(a,(startPos,err))
			     val a2 = checkNesting a1
			     val a3 = hookEndTag(a2,((startPos,endPos),curr,SOME(elem,space)))
			 in (NONE,(c,a3,q))
			 end
	    end

	 (*--------------------------------------------------------------*)
	 (* handle a declaration, proc. instr or tag. If it is an end-   *)
	 (* tag, finish the element in the user data and return.         *)
	 (*                                                              *)
	 (* print an error if the element's content is not yet finished. *)
	 (* print an error if the end-tag is for another element.        *)
	 (* print an error if the element's first character was not in   *)
	 (* the same entity.                                             *)
	 (*--------------------------------------------------------------*)
	 and do_lt (q,(c1,a1,q1)) =
	    case c1
	      of 0wx21 (* #"!" *) => 
	         (*------------------------------------------------------*)
		 (* its a declaration, cdata section or comment.         *)
                 (* Only comments and cdata sections are valid.          *)
	         (*------------------------------------------------------*)
		 let val (c2,a2,q2) = getChar(a1,q1)
		     val caq3 =  
			case c2
			  of 0wx2D (* #"-" *) => 
			     let val (c3,a3,q3) = getChar(a2,q2)
			     in if c3=0wx2D then parseComment (getPos q) (a3,q3)
				else let val err = ERR_EXPECTED(expDash,[c3])
					 val a4 = hookError(a3,(getPos q3,err))
				     in recoverDecl false (c3,a4,q3)
				     end
			     end
			   | 0wx5B (* #"[" *) => parseCDataSection (getPos q) (a2,q2)
			   | _ => 
			     (c2,hookError(a2,(getPos q2,ERR_EXPECTED(expDashLbrack,[c2]))),q2)
		 in do_mixed caq3
		 end
	       | 0wx2F (* #"/" *) => 
		 (let val (elem,space,endPos,caq2) = parseETag dtd (a1,q1)
		  in do_etag ((elem,space,getPos q,endPos),caq2)
		  end
		     handle SyntaxError caq => do_mixed caq)
	       | 0wx3F (* #"?" *) => do_mixed (parseProcInstr (getPos q) (a1,q1))
	       | _ => 
		 (*------------------------------------------------------*)
		 (* it's a start tag. the recursive call to parseElement *)
		 (* might return an end-tag that has to be consumed.     *)
		 (*------------------------------------------------------*)
		 if isNms c1 then 
		    let val (opt,caq2) = 
		       (let val (stag as ((_,elem,_,_,_),_),(c2,a2,q2)) = 
			   parseSTag dtd (getPos q) (c1,a1,q1)
			    val a3 = validate (a2,q1) elem
			in parseElement (dtd,curr::openElems,q,stag,(c2,a3,q2))
			end
			   handle SyntaxError caq => (NONE,caq))
		    in case opt
			 of NONE => do_mixed caq2
			  | SOME etag => do_etag (etag,caq2)
		    end
		 else let val err = ERR_FORBIDDEN_HERE(IT_CHAR 0wx3C,LOC_CONTENT)
			  val a2 = hookError(a1,(getPos q,err))
		      in do_mixed (c1,a2,q1)
		      end

	 (*--------------------------------------------------------------*)
	 (* do mixed content. handle the document end by printing an     *)
	 (* error and finishing like with an end-tag.                    *)
	 (*--------------------------------------------------------------*)
	 and do_mixed (c,a,q) = 
	    case c 
	      of 0wx00 => if isSpecial q 
			     then let val err = ERR_OMITTED_END_TAG(Index2Element dtd curr)
				      val a1 = hookError(a,(getPos q,err))
				      val pos = getPos q
				      val a2 = hookEndTag(a1,((pos,pos),curr,NONE))
				  in (NONE,(c,a2,q))
				  end
			  else let val a1 = hookEntEnd(a,getPos q)
			       in do_mixed (getChar(a1,q))
			       end
	       | 0wx26 (* #"&" *) => do_mixed (do_ref (q,getChar(a,q)))
	       | 0wx3C (* #"<" *) => do_lt (q,getChar(a,q))
	       | 0wx5D => do_mixed (do_data (!O_COMPATIBILITY,(c,a,q)))
	       | _ => do_mixed (do_data (false,(c,a,q)))
      in 
	 do_mixed caq
      end

   (*--------------------------------------------------------------------*)
   (* parse an element, the start tag already read. the second arg holds *)
   (* the number of the entity of the start-tag's first char, and the    *)
   (* start-tag information. The 1st arg is the start value for the user *)
   (* data. 3:                                                           *)
   (*                                                                    *)
   (*   [39] element ::= EmptyElemTag                                    *)
   (*                  | STag content ETag                               *)
   (* and 3.1:                                                           *)
   (*                                                                    *)
   (*   Empty-element tags may be used for any element which has no      *)
   (*   content, whether or not it is declared using the keyword EMPTY.  *)
   (*   For interoperability, the empty-element tag must be used, and    *)
   (*   can only be used, for elements which are declared EMPTY.         *)
   (*--------------------------------------------------------------------*)
   and parseElement (dtd,openElems,q0,(stag as (_,curr,_,_,mt),elemInfo),(c,a,q)) =
      let 
	 (*--------------------------------------------------------------*)
	 (* validate whether an element is allowed in mixed/any content. *) 
	 (*--------------------------------------------------------------*)
	 fun trans_any (a,_) _ = a
	 fun trans_mixed is (a,q) i = 
	    if member i is then a
	    else let val err = ERR_BAD_ELEM(Index2Element dtd curr,Index2Element dtd i)
		 in hookError(a,(getPos q,err))
		 end
      in 
	 (*-----------------------------------------------------------*)
	 (* For empty-element tags, verify that the element's declar. *)
	 (* allows empty content.                                     *)
	 (*-----------------------------------------------------------*)
	 if mt then 
	    let val a1 = 
	       if not (!O_VALIDATE andalso hasDtd dtd) then a
	       else 
		  case #decl elemInfo
		    of (SOME(CT_EMPTY,_)) => a
		     | (SOME(CT_ELEMENT(_,dfa),_)) => 
		       if not (dfaFinal(dfa,dfaInitial)) 
			  then hookError(a,(getPos q0,ERR_EMPTY_TAG(Index2Element dtd curr)))
		       else if not (!O_INTEROPERABILITY) then a
			    else hookError
			       (a,(getPos q0,ERR_EMPTY_TAG_INTER (Index2Element dtd curr)))
		     | _ => if not (!O_INTEROPERABILITY) then a
			    else hookError(a,(getPos q0,ERR_EMPTY_TAG_INTER
					      (Index2Element dtd curr)))
	    in (NONE,(c,hookStartTag(a1,stag),q))
	    end
	 (*-----------------------------------------------------------*)
	 (* for normal start-tags, check whether the element's decl.  *)
	 (* requires an empty-element tag, or empty content, then     *)
	 (* call the appropriate function that parses the content.    *)
	 (*-----------------------------------------------------------*)
	 else 
	    let val startEnt = getEntId q0
	    in if !O_VALIDATE then  
	       case getOpt(#decl elemInfo,(CT_ANY,false))
		 of (CT_ANY,_) => parseMixedContent dtd  
		    (openElems,startEnt,curr,trans_any) (c,hookStartTag(a,stag),q)
		  | (CT_MIXED is,_) => parseMixedContent dtd 
		    (openElems,startEnt,curr,trans_mixed is) (c,hookStartTag(a,stag),q) 
		  | (CT_ELEMENT(_,dfa),ext) => parseElementContent dtd 
		    (openElems,startEnt,curr,dfa,ext,false) 
		    (c,hookStartTag(a,stag),q) 
		  | (CT_EMPTY,_) => 
		    let val a1 = if not (!O_INTEROPERABILITY) then a
				 else let val err = ERR_MUST_BE_EMPTY(Index2Element dtd curr)
				      in hookError(a,(getPos q0,err))
				      end
			val a2 = hookStartTag(a1,stag)
		    in parseElementContent dtd 
		       (openElems,startEnt,curr,emptyDfa,false,true) (c,a2,q)
		    end
	       else parseMixedContent dtd 
		  (openElems,startEnt,curr,trans_any) (c,hookStartTag(a,stag),q)
	    end
      end
end

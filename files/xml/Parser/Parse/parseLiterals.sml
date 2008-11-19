signature ParseLiterals = 
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
      val parseSopt : UniChar.Data -> UniChar.Char * AppData * State 
	 -> UniChar.Data * (UniChar.Char * AppData * State)
      val parseSmay : UniChar.Data -> UniChar.Char * AppData * State 
	 -> bool * (UniChar.Data * (UniChar.Char * AppData * State))
      val parseEq : UniChar.Char * AppData * State 
	 -> UniChar.Data * (UniChar.Char * AppData * State)

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
      ----------------------------------------------------------------------*)
      include ParseRefs

      val parseSystemLiteral : UniChar.Char * AppData * State 
	 -> Uri.Uri * UniChar.Char * (UniChar.Char * AppData * State)
      val parsePubidLiteral  : UniChar.Char * AppData * State 
	 -> string * UniChar.Char * (UniChar.Char * AppData * State)

      val parseAttValue : Dtd -> UniChar.Char * AppData * State 
	 -> UniChar.Vector * UniChar.Data * (UniChar.Char * AppData * State)
      val parseEntityValue : Dtd -> (UniChar.Vector * UniChar.Vector -> 'a) 
	 -> UniChar.Char * AppData * State 
	 -> 'a * (UniChar.Char * AppData * State)
   end

(*--------------------------------------------------------------------------*)
(* Structure: ParseLiterals                                                 *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   parseSystemLiteral : NotFound                                          *)
(*   parsePubidLiteral  : NotFound                                          *)
(*   parseAttValue      : NotFound                                          *)
(*   parseEntityValue   : NotFound                                          *)
(*--------------------------------------------------------------------------*)
functor ParseLiterals (structure ParseBase : ParseBase) 
   : ParseLiterals =   
struct
   structure ParseRefs  = ParseRefs  (structure ParseBase = ParseBase)

   open
      Base UniChar Errors UniClasses Uri
      ParseRefs 

   val THIS_MODULE = "ParseLiterals"
      
      (*--------------------------------------------------------------------*)
      (* parse a system literal, the quote character ("'" or '"') already --*)
      (* read and passed as first argument.  cf. 2.3:                       *)
      (*                                                                    *)
      (*   ... Note that a SystemLiteral can be parsed without scanning     *)
      (*   for markup.                                                      *)
      (*                                                                    *)
      (*   [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")         *)
      (*                                                                    *)
      (* print an error and end the literal if an entity end is found.      *)
      (*                                                                    *)
      (* return the literal as a string together with the next character    *)
      (* and remaining state.                                               *)
      (*--------------------------------------------------------------------*)
      (* might raise: none                                                  *) 
      (*--------------------------------------------------------------------*)
      fun parseSystemLiteral' quote aq =
	 let 
	    fun doit text (c,a,q) = 
	       if c=quote then (text,getChar (a,q))
	       else if c=0wx0 
		       then let val a1 = hookError(a,(getPos q,ERR_ENDED_BY_EE LOC_SYS_LIT))
			    in (text,(c,a1,q))
			    end
	       else if c>0wx7F andalso !O_WARN_NON_ASCII_URI 
		       then let val a1 = hookWarning(a,(getPos q,WARN_NON_ASCII_URI c))
			    in doit (c::text) (getChar(a1,q))
			    end
		    else doit (c::text) (getChar(a,q))
	    
	    val (text,caq1) = doit nil (getChar aq)
	 in 
	    (Data2Uri(rev text),quote,caq1)
	 end
      (*--------------------------------------------------------------------*)
      (* parse a system literal.                                            *)
      (*                                                                    *)
      (*   [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")         *)
      (*                                                                    *)
      (* raise NotFound if neither '"' nor "'" comes first.                 *)   
      (*                                                                    *)
      (* return the literal as a string together with the next character    *)
      (* and remaining state.                                               *)
      (*--------------------------------------------------------------------*)
      (* might raise: NotFound                                              *) 
      (*--------------------------------------------------------------------*)
      fun parseSystemLiteral (c,a,q) = 
	 if c=0wx22 (* "'" *) orelse 
	    c=0wx27 (* '"' *) 
	    then parseSystemLiteral' c (a,q)
	 else raise NotFound (c,a,q)
		     
      (*--------------------------------------------------------------------*)
      (* parse a pubid literal, the quote character ("'" or '"') already ---*)
      (* read and passed as first argument.  cf. 2.3:                       *)
      (*                                                                    *)
      (*   [12]  PubidLiteral ::= '"' PubidChar* '"'                        *)
      (*                        | "'" (PubidChar - "'")* "'"                *)
      (*                                                                    *)
      (* print an error and end the literal if an entity end is found.      *)
      (* print an error if a non-pubid character is found.                  *)
      (*                                                                    *)
      (* return the literal as a string together with the next character    *)
      (* and remaining state.                                               *)
      (*--------------------------------------------------------------------*)
      (* might raise: none                                                  *) 
      (*--------------------------------------------------------------------*)
      fun parsePubidLiteral' quote aq =
	 let 
	    fun doit (hadSpace,atStart,text) aq = 
	       let val (c1,a1,q1) = getChar aq
	       in case c1 
		    of 0wx00 => let val a2 = hookError(a1,(getPos q1,ERR_ENDED_BY_EE LOC_PUB_LIT))
				in (text,(c1,a2,q1)) 
				end
		     | 0wx0A => doit (true,atStart,text) (a1,q1)
		     | 0wx20 => doit (true,atStart,text) (a1,q1)
		     | _ => 
		       if c1=quote then (text,getChar (a1,q1))
		       else if not (isPubid c1) 
			       then let val err = ERR_FORBIDDEN_HERE(IT_CHAR c1,LOC_PUB_LIT)
					val a2 = hookError(a1,(getPos q1,err))
				    in doit (hadSpace,atStart,text) (a2,q1)
				    end
		       else if hadSpace andalso not atStart 
			       then doit (false,false,c1::0wx20::text) (a1,q1)
			    else doit (false,false,c1::text) (a1,q1)
	       end 
	    val (text,caq1) = doit (false,true,nil) aq
	 in 
	    (Latin2String(rev text),quote,caq1)
	 end
      (*--------------------------------------------------------------------*)
      (* parse a pubid literal.                                             *)
      (*                                                                    *)
      (*   [12]  PubidLiteral ::= '"' PubidChar* '"'                        *)
      (*                        | "'" (PubidChar - "'")* "'"                *)
      (*                                                                    *)
      (* raise NotFound if neither '"' nor "'" comes first.                 *)   
      (*                                                                    *)
      (* return the literal as a string together with the next character    *)
      (* and remaining state.                                               *)
      (*--------------------------------------------------------------------*)
      (* might raise: NotFound                                              *) 
      (*--------------------------------------------------------------------*)
      fun parsePubidLiteral (c,a,q) = 
	 if c=0wx22 (* "'" *) orelse 
	    c=0wx27 (* '"' *) 
	    then parsePubidLiteral' c (a,q)
	 else raise NotFound (c,a,q)

      (*--------------------------------------------------------------------*)
      (* parse an entity value and the quote character ("'" or '"') passed  *)
      (* as first argument. Cf. 2.3:                                        *)
      (*                                                                    *)
      (*   [9]   EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"'*)
      (*                      |  "'" ([^%&'] | PEReference | Reference)* "'"*)
      (* See also 4.4.5:                                                    *)
      (*                                                                    *)
      (*   When ... a parameter entity reference appears in a literal       *)
      (*   entity value, its replacement text is processed in place of the  *)
      (*   reference itself as though it were part of the document at the   *)
      (*   location the reference was recognized, except that a single or   *)
      (*   double quote character in the replacement text is always treated *)
      (*   as a normal data character and will not terminate the literal.   *)
      (*                                                                    *)
      (* and 4.4.7:                                                         *)
      (*                                                                    *)
      (*   When a general entity reference appears in the EntityValue in an *)
      (*   entity declaration, it is bypassed and left as is.               *)
      (*                                                                    *)
      (* A bypassed entity ref must, however, be checked for syntactic      *)
      (* validity, as opposed to SGML, where it is not even recognized.     *)
      (*                                                                    *)
      (* print an error and end the literal if an entity end is found at    *)
      (* the toplevel.                                                      *)
      (* print an error if a general entity reference is ill-formed.        *)
      (*                                                                    *)
      (* handle any errors in references by ignoring them syntactically.    *)
      (*                                                                    *)
      (* return argument con applied to the entity value as a char buffer,  *)
      (* and the remaining char and state.                                  *)
      (*--------------------------------------------------------------------*)
      (* might raise: none                                                  *) 
      (*--------------------------------------------------------------------*)
      fun parseEntityValue' dtd (quote,con) aq =
	 let fun doit (level,hadCr,lit,text) (c1,a1,q1) = 
	    case c1
	      of 0wx00 => if level=0 then let val err = ERR_ENDED_BY_EE LOC_ENT_VALUE
					      val a2 = hookError(a1,(getPos q1,err))
					  in (lit,text,(c1,a2,q1))
					  end
			  else doit (level-1,false,lit,text) (getChar (a1,q1))
	       | 0wx25 => (* #"%" *)
		 let val (level1,lit1,caq2) = 
		    if inDocEntity q1 
		       then let val err = ERR_FORBIDDEN_HERE(IT_PAR_REF,LOC_INT_DECL)
				val a2 = hookError(a1,(getPos q1,err))
			    in (level,lit,skipReference (getChar(a2,q1)))
			    end
		    else
		       let val (lit1,((id,ent),(a2,q2))) = 
			  if level=0 then parseParRefLit dtd (c1::lit) (getChar(a1,q1))
			  else (lit,parseParRef dtd (getChar(a1,q1)))
		       in case ent
			    of PE_NULL => (level,lit1,getChar(a2,q2))
			     | PE_INTERN(_,rep) => 
			       let val q3 = pushIntern(q2,id,true,rep)
			       in (level+1,lit1,getChar(a2,q3))
			       end
			     | PE_EXTERN extId => 
			       let 
				  val fname = resolveExtId extId
				  val caq3 = #3(openExtern (id,true,fname) (a2,q2))
			       in (level+1,lit1,caq3)
			       end handle CantOpenFile(fmsg,a) 
			       => let val err = ERR_NO_SUCH_FILE fmsg
				      val a1 = hookError(a,(getPos q1,err))
				  in (level,lit1,getChar(a1,q1))
				  end
		       end (* ignore syntax errors in references *)
		    handle SyntaxError caq => (level,lit,caq) 
			 | NoSuchEntity aq => (level,lit,getChar aq)
		 in doit (level1,false,lit1,text) caq2
		 end
	       | 0wx26 => (* #"&" *)
		 let val (c2,a2,q2) = getChar (a1,q1)
		 in (if c2=0wx23 (* #"#" *)
			(*--------------------------------------------------*)
			(* it's a character reference.                      *)
			(*--------------------------------------------------*)
			then (if level=0
				 then 
				    let val (lit3,(ch,a3,q3)) = 
				       parseCharRefLit (c2::c1::lit) (a2,q2)
				    in doit (level,false,lit3,ch::text) (getChar(a3,q3))
				    end
			      else let val (ch,a3,q3) = parseCharRef (a2,q2)
				   in doit (level,false,lit,ch::text) (getChar(a3,q3))
				   end)
			   (* ignore errors in char references *)
			   handle SyntaxError caq => doit (level,false,lit,text) caq
				| NoSuchChar aq => doit (level,false,lit,text) (getChar aq)
		     (*-----------------------------------------------------*)
		     (* it's a general entity reference.                    *)
		     (*-----------------------------------------------------*)
		     else let 
			     val (fnd,lit3,text3,(c3,a3,q3)) = 
				parseEntName (c1::lit,c1::text) (c2,a2,q2)
			     val (lit4,text4,caq4) = 
				if not fnd then (lit,text,(c3,a3,q3))
				else if c3=0wx3B (* #";" *) 
					then (c3::lit3,c3::text3,(getChar(a3,q3)))
				     else let val err = ERR_EXPECTED(expSemi,[c3])
					      val a4 = hookError(a3,(getPos q3,err))
					  in (lit,text,(c3,a4,q3))
					  end
			  in doit (level,false,lit4,text4) caq4
			  end
		       )
		 end
	       | 0wx0A => doit (level,false,if level=0 then c1::lit else lit,
				if hadCr then text else c1::text) (getChar (a1,q1))
	       | 0wx0D => doit (level,true,if level=0 then c1::lit else lit,0wx0A::text) 
			  (getChar (a1,q1))
	       | _ => if c1=quote andalso level=0 then (lit,text,getChar(a1,q1))
		      else doit (level,false,if level=0 then c1::lit else lit,c1::text) 
			 (getChar (a1,q1))

	    val (lit,text,caq1) = doit (0,false,nil,nil) (getChar aq)
	    val literal = Data2Vector(quote::rev(quote::lit))
	    val repText = Data2Vector(rev text)
	 in 
	    (con(literal,repText),caq1)
	 end
      (*--------------------------------------------------------------------*)
      (* parse an entity value.                                             *)
      (*                                                                    *)
      (*   [9]   EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"'*)
      (*                      |  "'" ([^%&'] | PEReference | Reference)* "'"*)
      (*                                                                    *)
      (* raise NotFound if neither '"' nor "'" comes first.                 *)   
      (*                                                                    *)
      (* return the entity value as a char buffer, and the remaining char   *) 
      (* and state.                                                         *)
      (*--------------------------------------------------------------------*)
      (* might raise: NotFound                                              *) 
      (*--------------------------------------------------------------------*)
      fun parseEntityValue dtd con (c,a,q) = 
	 if c=0wx22 (* "'" *) orelse 
	    c=0wx27 (* '"' *) 
	    then parseEntityValue' dtd (c,con) (a,q)
	 else raise NotFound (c,a,q)

      (*--------------------------------------------------------------------*)
      (* parse and normalize an attribute value, consume the final quote    *)
      (* character ("'" or '""') passed in the argument. Cf. 2.3:           *)
      (*                                                                    *)
      (*   [10]      AttValue ::= '"' ([^<&""] | Reference)* '"'            *)
      (*                       |  "'" ([^<&'] | Reference)* "'"             *)
      (* See also 4.4.5:                                                    *)
      (*                                                                    *)
      (*   When an entity reference appears in an attribute value ...,      *)
      (*   its replacement text is processed in place of the reference      *)
      (*   itself as though it were part of the document at the location    *)
      (*   the reference was recognized, except that a single or double     *)
      (*   quote character in the replacement text is always treated as a   *)
      (*   normal data character and will not terminate the literal.        *)
      (*                                                                    *)
      (* and 3.3.3:                                                         *)
      (*                                                                    *)
      (*   Before the value of an attribute is passed to the application    *)
      (*   or checked for validity, the XML processor must normalize it as  *)
      (*   follows:                                                         *)
      (*                                                                    *)
      (*   * a character reference is processed by appending the referenced *)
      (*     character to the attribute value                               *)
      (*   * an entity reference is processed by recursively processing the *)
      (*     replacement text of the entity                                 *)
      (*   * a whitespace character (#x20, #xD, #xA, #x9) is processed by   *)
      (*     appending #x20 to the normalized value, except that only a     *)
      (*     single #x20 is appended for a "#xD#xA" sequence that is part   *)
      (*     of an external parsed entity or the literal entity value of    *)
      (*     an internal parsed entity                                      *)
      (*   * other characters are processed by appending them to the        *)
      (*     normalized value                                               *)
      (*                                                                    *)
      (* since #xD#xA are normalized by the parseEntityValue (internal) and *)
      (* getChar (external entities), we don't need to care about that.     *)
      (*--------------------------------------------------------------------*)
      (* print an error and end the literal if an entity end is found.      *)
      (* print an error if a general entity reference is ill-formed.        *) 
      (* print an error if a reference to an external or unparsed entity is *)
      (* found.                                                             *)
      (* print an error if character '<' appears literally.                 *)
      (*                                                                    *)
      (* handle any errors in references by ignoring them syntactically.    *)
      (* raise NotFound if neither '"' nor "'" comes first.                 *)   
      (*                                                                    *)
      (* return the list of chars in the value, and the next char and state *) 
      (*--------------------------------------------------------------------*)
      (* might raise: NotFound                                              *) 
      (*--------------------------------------------------------------------*)
      fun parseAttValue dtd (quote,a,q) =
	 let fun doit (lhlt as (level,lit,text)) (c1,a1,q1) = 
	    case c1
	      of 0wx00 => if level=0 then let val err = ERR_ENDED_BY_EE LOC_ATT_VALUE
					      val a2 = hookError(a1,(getPos q1,err))
					  in (lit,text,(c1,a2,q1))
					  end
			  else doit (level-1,lit,text) (getChar (a1,q1))
	       | 0wx26 => (* #"&" *)
		 let 
		    val (c2,a2,q2) = getChar (a1,q1)
		    val ((level1,lit1,text1),caq3) = 
		       (if c2=0wx23 (* #"#" *)
			   (*--------------------------------------------------*)
			   (* it's a character reference.                      *)
			   (*--------------------------------------------------*)
			   then if level=0
				   then 
				      let val (lit3,(ch,a3,q3)) = 
					 parseCharRefLit (c2::c1::lit) (a2,q2)
				      in ((level,lit3,ch::text),getChar(a3,q3))
				      end
				else let val (ch,a3,q3) = parseCharRef (a2,q2)
				     in ((level,lit,ch::text),getChar (a3,q3))
				     end
			(*-----------------------------------------------------*)
			(* it's a general entity reference.                    *)
			(*-----------------------------------------------------*)
			else 
			   let val (lit3,((id,ent),(a3,q3))) = 
			      if level=0 then parseGenRefLit dtd (c1::lit) (c2,a2,q2)
			      else (nil,parseGenRef dtd (c2,a2,q2))
			   in case ent
				of GE_NULL => ((level,lit3,text),getChar(a3,q3))
				 | GE_INTERN(_,rep) => 
				   let val q4 = pushIntern(q3,id,false,rep)
				   in ((level+1,lit3,text),getChar (a3,q4))
				   end
				 | GE_EXTERN _ => 
				   let val err = ERR_ILLEGAL_ENTITY
				      (ENT_EXTERNAL,Index2GenEnt dtd id,LOC_ATT_VALUE)
				       val a4 = hookError(a3,(getPos q2,err))
				   in ((level,lit,text),getChar (a4,q3))
				   end
				   | GE_UNPARSED _ => raise InternalError
				   (THIS_MODULE,"parseAttValue'",
				    "parseGenRef returned GE_UNPARSED")
			   end)
			   (*------------------------------------------------------*)
			   (* handle any errors in references by ignoring them.    *)
			   (*------------------------------------------------------*)
			   handle SyntaxError caq => ((level,lit,text),caq)
				| NoSuchEntity aq => ((level,lit,text),getChar aq)
				| NoSuchChar aq => ((level,lit,text),getChar aq)
		 in doit (level1,lit1,text1) caq3
		 end
	       | 0wx3C => let val err = ERR_FORBIDDEN_HERE(IT_CHAR c1,LOC_ATT_VALUE)
			      val a2 = hookError(a1,(getPos q1,err))
			      val lit1 = if level=0 then c1::lit else lit
			  in doit (level,lit1,c1::text) (getChar (a2,q1))
			  end
	       | _ => if isS c1 then doit (level,if level=0 then c1::lit else lit,0wx20::text) 
			  (getChar (a1,q1))
		      else (if c1=quote andalso level=0 then (lit,text,getChar (a1,q1))
			    else doit (level,if level=0 then c1::lit else lit,c1::text) 
			       (getChar (a1,q1)))
	     

	     val _ = if quote=0wx22 orelse quote=0wx27 (* "'",'"' *) then () 
		     else raise NotFound (quote,a,q)
	     val (lit,text,caq1) = doit (0,nil,nil) (getChar(a,q))
	 in 
	    (Data2Vector(quote::rev(quote::lit)),rev text,caq1)
	 end
   end

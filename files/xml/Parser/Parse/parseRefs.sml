signature ParseRefs =
   sig
      (*----------------------------------------------------------------------
      include ParseBase

      val parseName    : UniChar.Char * AppData * State 
	 -> UniChar.Data * (UniChar.Char * AppData * State)
      val parseNmtoken : UniChar.Char * AppData * State 
         -> UniChar.Data * (UniChar.Char * AppData * State)
      val parseEntName : UniChar.Data * UniChar.Data -> UniChar.Char * AppData * State 
         -> bool * UniChar.Data * UniChar.Data * (UniChar.Char * AppData * State)

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
      ----------------------------------------------------------------------*)
      include ParseXml

      val parseCharRef : AppData * State -> UniChar.Char *  AppData * State
      val parseGenRef  : Dtd -> UniChar.Char * AppData * State 
	 -> (int * Base.GenEntity) * (AppData * State)
      val parseParRef  : Dtd -> UniChar.Char * AppData * State 
	 -> (int * Base.ParEntity) * (AppData * State)

      val parseCharRefLit : UniChar.Data -> AppData * State 
	 -> UniChar.Data * (UniChar.Char * AppData * State)
      val parseGenRefLit  : Dtd -> UniChar.Data -> UniChar.Char *  AppData * State 
	 -> UniChar.Data * ((int * Base.GenEntity) *  (AppData * State))
      val parseParRefLit  : Dtd -> UniChar.Data -> UniChar.Char *  AppData * State 
	 -> UniChar.Data * ((int * Base.ParEntity) *  (AppData * State))

      val skipCharRef   : AppData * State -> (UniChar.Char *  AppData * State)
      val skipReference : UniChar.Char * AppData * State -> (UniChar.Char *  AppData * State)

      val skipPS    : Dtd -> UniChar.Char * AppData * State 
	 -> UniChar.Char * AppData * State
      val skipPSopt : Dtd -> UniChar.Char * AppData * State 
	 -> UniChar.Char * AppData * State
      val skipPSmay : Dtd -> UniChar.Char * AppData * State 
	 -> bool * (UniChar.Char * AppData * State)
      val skipPSdec : Dtd -> UniChar.Char * AppData * State 
	 -> bool * (UniChar.Char * AppData * State)
    end

(*--------------------------------------------------------------------------*)
(* Structure: ParseRefs                                                     *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   parseCharRef  : NoSuchChar SyntaxError                                 *)
(*   parseGenRef   : NoSuchEntity SyntaxState                               *)
(*   parseParRef   : NoSuchEntity SyntaxState                               *)
(*   skipCharRef   : none                                                   *)
(*   skipPS        : none                                                   *)
(*   skipPSdec     : none                                                   *)
(*   skipPSmay     : none                                                   *)
(*   skipPSopt     : none                                                   *)
(*   skipReference : none                                                   *)
(*--------------------------------------------------------------------------*)
functor ParseRefs (structure ParseBase : ParseBase)
   : ParseRefs = 
struct
   structure ParseXml   = ParseXml   (structure ParseBase = ParseBase)

   open
      Base Errors UniClasses
      ParseXml

   (*--------------------------------------------------------------------*)
   (* parse a character reference, the "&#" already read. See 4.1:       *)
   (*                                                                    *)
   (*   [66] CharRef ::= '&#'  [0-9]+ ';'                                *)
   (*                  | '&#x' [0-9a-fA-F]+ ';' [ WFC: Legal Character ] *)
   (*                                                                    *)
   (*   Well-Formedness Constraint: Legal Character                      *)
   (*   Characters referred to using character references must match the *)
   (*   production for Char.                                             *)
   (*                                                                    *)
   (*   If the character reference begins with "&#x", the digits and     *)
   (*   letters up to the terminating ; provide a hexadecimal            *)
   (*   representation of the character's code point in ISO/IEC 10646.   *)
   (*   If it begins just with "&#", the digits up to the terminating ;  *)
   (*   provide a decimal representation of the character's code point.  *)
   (*                                                                    *)
   (* raise SyntaxError if no number or x plus hexnum is found, or if no *)
   (* semicolon follows it.                                              *)
   (* raise NoSuchChar if the reference is to a non-XML character.       *)
   (*                                                                    *)
   (* return the character referred to, and the remaining state.         *)
   (*--------------------------------------------------------------------*)
   fun parseCharRef aq =
      let 
	 (*--------------------------------------------------------------*)
	 (* parse a (hexa)decimal number, accumulating the value in the  *)
	 (* first parameter.                                             *)
	 (*                                                              *)
	 (* return the numbers value as a Char.                          *)
	 (*--------------------------------------------------------------*)
	 fun do_hex_n yet (c,a,q) = 
	    case hexValue c
	      of NONE => (yet,(c,a,q))
	       | SOME v => do_hex_n (0wx10*yet+v) (getChar (a,q))
	 fun do_dec_n yet (c,a,q) = 
	    case decValue c
	      of NONE => (yet,(c,a,q))
	       | SOME v => do_dec_n (0wx0A*yet+v) (getChar (a,q))
	 (*--------------------------------------------------------------*)
	 (* Parse a (hexa)decimal number of at least one digit.          *)
	 (*                                                              *)
	 (* raise SyntaxError if no hexdigit is found first.             *)
	 (*                                                              *)
	 (* return the numbers value as a Char.                          *)
	 (*--------------------------------------------------------------*)
	 fun do_hex_1 (c,a,q) =
	    case hexValue c 
	      of SOME v => do_hex_n v (getChar (a,q))
	       | NONE => let val a1 = hookError(a,(getPos q,ERR_EXPECTED(expHexDigit,[c])))
			 in raise SyntaxError(c,a1,q)
			 end
	 (*--------------------------------------------------------------*)
	 (* Parse a decimal number of at least one digit, or a hexnumber *)
	 (* if the first character is 'x'.                               *)
	 (*                                                              *)
	 (* raise SyntaxError if neither 'x' nor digit is found first.   *)
	 (*                                                              *)
	 (* return the number's value as a Char.                         *)
	 (*--------------------------------------------------------------*)
	 fun do_dec_1 (c,a,q) =
	    case decValue c 
	      of SOME v => do_dec_n v (getChar (a,q))
	       | NONE => if c=0wx78 (* #"x" *) 
			    then do_hex_1 (getChar (a,q))
			 else let val a1 = hookError(a,(getPos q,ERR_EXPECTED(expDigitX,[c])))
			      in raise SyntaxError(c,a1,q)
			      end
			       
	 val (ch,(c1,a1,q1)) = do_dec_1 (getChar aq)

	 val _ = if c1=0wx3B then () 
		 else let val a2 = hookError(a1,(getPos q1,ERR_EXPECTED(expSemi,[c1])))
		      in raise SyntaxError(c1,a2,q1)
		      end

	 val _ = if isXml ch then ()
		 else let val a2 = hookError(a1,(getPos q1,ERR_NON_XML_CHARREF ch))
		      in raise NoSuchChar (a2,q1)
		      end
      in (ch,a1,q1)
      end
   fun parseCharRefLit cs aq =
      let 
	 (*--------------------------------------------------------------*)
	 (* parse a (hexa)decimal number, accumulating the value in the  *)
	 (* first parameter.                                             *)
	 (*                                                              *)
	 (* return the numbers value as a Char.                          *)
	 (*--------------------------------------------------------------*)
	 fun do_hex_n (cs,yet) (c,a,q) = 
	    case hexValue c
	      of NONE => (cs,yet,(c,a,q))
	       | SOME v => do_hex_n (c::cs,0wx10*yet+v) (getChar (a,q))
	 fun do_dec_n (cs,yet) (c,a,q) = 
	    case decValue c
	      of NONE => (cs,yet,(c,a,q))
	       | SOME v => do_dec_n (c::cs,0wx0A*yet+v) (getChar (a,q))
	 (*--------------------------------------------------------------*)
	 (* Parse a (hexa)decimal number of at least one digit.          *)
	 (*                                                              *)
	 (* raise SyntaxError if no hexdigit is found first.             *)
	 (*                                                              *)
	 (* return the numbers value as a Char.                          *)
	 (*--------------------------------------------------------------*)
	 fun do_hex_1 cs (c,a,q) =
	    case hexValue c 
	      of SOME v => do_hex_n (c::cs,v) (getChar (a,q))
	       | NONE => let val a1 = hookError(a,(getPos q,ERR_EXPECTED(expHexDigit,[c])))
			 in raise SyntaxError(c,a1,q)
			 end
	 (*--------------------------------------------------------------*)
	 (* Parse a decimal number of at least one digit, or a hexnumber *)
	 (* if the first character is 'x'.                               *)
	 (*                                                              *)
	 (* raise SyntaxError if neither 'x' nor digit is found first.   *)
	 (*                                                              *)
	 (* return the number's value as a Char.                         *)
	 (*--------------------------------------------------------------*)
	 fun do_dec_1 cs (c,a,q) =
	    case decValue c 
	      of SOME v => do_dec_n (c::cs,v) (getChar (a,q))
	       | NONE => if c=0wx78 (* #"x" *) 
			    then do_hex_1 (c::cs) (getChar (a,q))
			 else let val a1 = hookError(a,(getPos q,ERR_EXPECTED(expDigitX,[c])))
			      in raise SyntaxError(c,a1,q)
			      end
			       
	 val (cs1,ch,(c1,a1,q1)) = do_dec_1 cs (getChar aq)

	 val _ = if c1=0wx3B then () 
		 else let val a2 = hookError(a1,(getPos q1,ERR_EXPECTED(expSemi,[c1])))
		      in raise SyntaxError(c1,a2,q1)
		      end

	 val _ = if isXml ch then ()
		 else let val a2 = hookError(a1,(getPos q1,ERR_NON_XML_CHARREF ch))
		      in raise NoSuchChar (a2,q1)
		      end
      in (c1::cs1,(ch,a1,q1))
      end

   (*--------------------------------------------------------------------*)
   (* parse a general entity reference, the "&" already read. See 4.1:   *)
   (*                                                                    *)
   (*   [68]   EntityRef ::= '&' Name ';'       [ WFC: Entity Declared ] *)
   (*                                           [ VC: Entity Declared ]  *)
   (*                                           [ WFC: Parsed Entity ]   *)
   (*                                           [ WFC: No Recursion ]    *)
   (*                                                                    *)
   (*   Well-Formedness Constraint: Entity Declared                      *)
   (*   In a document without any DTD, a document with only an internal  *)
   (*   DTD subset which contains no parameter entity references, or a   *)
   (*   document with "standalone='yes'", the Name given in the entity   *)
   (*   reference must match that in an entity declaration, ...          *)
   (*   ... the declaration of a general entity must precede any         *)
   (*   reference to it which appears in a default value in an           *)
   (*   attribute-list declaration.                                      *)
   (*                                                                    *)
   (*   Validity Constraint: Entity Declared                             *)
   (*   In a document with an external subset or external parameter      *)
   (*   entities with "standalone='no'", the Name given in the entity    *)
   (*   reference must match that in an entity declaration. ...          *)
   (*   ... the declaration of a general entity must precede any         *)
   (*   reference to it which appears in a default value in an           *)
   (*   attribute-list declaration.                                      *)
   (*                                                                    *)
   (* Thus: in both cases it is an error if the entity is not declared.  *)
   (* The only difference is the impact on well-formednes/validity.      *)
   (*                                                                    *)
   (* There are three contexts in which a general entity reference can   *)
   (* appear: in content, in attribute value, in entity value. This      *)
   (* passage states that it need not be declared prior to a reference   *)
   (* in an entity value. But in this context, it is bypassed and not    *)
   (* included, i.e., it need not be recognized.                         *)
   (*                                                                    *)
   (* Well-Formedness Constraint: Parsed Entity                          *)
   (* An entity reference must not contain the name of an unparsed       *)
   (* entity. Unparsed entities may be referred to only in attribute     *)
   (* values ...                                                         *)
   (*                                                                    *)
   (*   Well-Formedness Constraint: No Recursion                         *)
   (*   A parsed entity must not contain a recursive reference to        *)
   (*   itself, either directly or indirectly.                           *)
   (*                                                                    *)
   (* print an error and raise SyntaxState if no name is found, or if no *)
   (* semicolon follows it.                                              *)
   (* print an error and return GE_NULL if the reference is to an        *)
   (* undeclared, unparsed or open entity.                               *)
   (*                                                                    *)
   (* return the entity referred to, and the remaining state.            *)
   (*--------------------------------------------------------------------*)
   fun parseGenRef dtd (caq as (_,_,q)) = 
      let 
	 val (name,(c1,a1,q1)) = parseName caq
	    handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expAnEntName,[c])
					   val a1 = hookError(a,(getPos q,err))
				       in raise SyntaxError(c,a1,q)
				       end
	 val _ = if c1=0wx3B then () 
		 else let val a2 = hookError(a1,(getPos q1,ERR_EXPECTED(expSemi,[c1])))
		      in raise SyntaxError(c1,a2,q1)
		      end
						    
	 val idx = GenEnt2Index dtd name
	 val (ent,ext) = getGenEnt dtd idx 
	       
	 val _ = (* check whether entity is undeclared/unparsed/open *)
	    case ent
	      of GE_NULL => 
		 if entitiesWellformed dtd
		    then let val err = ERR_UNDEC_ENTITY(ENT_GENERAL,name)
			     val a2 = hookError(a1,(getPos q,err))
			 in raise NoSuchEntity (a2,q1)
			 end
		 else if useParamEnts()
			 then let val err = ERR_UNDECLARED(IT_GEN_ENT,name,LOC_NONE)
				  val a2 = hookError(a1,(getPos q,err))
			      in raise NoSuchEntity (a2,q1)
			      end
		      else ()
	       | GE_UNPARSED _ => let val err = ERR_ILLEGAL_ENTITY(ENT_UNPARSED,name,LOC_NONE)
				      val a2 = hookError(a1,(getPos q,err))
				  in raise NoSuchEntity (a2,q1)
				  end
	       | _ => if isOpen(idx,false,q1) 
			 then let val err = ERR_RECURSIVE_ENTITY(ENT_GENERAL,name)
				  val a2 = hookError(a1,(getPos q,err))
			      in raise NoSuchEntity (a2,q1)
			      end
		      else ()
			    
	 val a2 = 
	    if ext andalso !O_VALIDATE andalso standsAlone dtd andalso inDocEntity q1
	       then let val _ = if !O_ERROR_MINIMIZE then setStandAlone dtd false else ()
		    in hookError(a1,(getPos q,ERR_STANDALONE_ENT(ENT_GENERAL,name)))
		    end
	    else a1

      in ((idx,ent),(a2,q1))	    
      end
   fun parseGenRefLit dtd cs (caq as (_,_,q)) = 
      let 
	 val (cs1,name,(c1,a1,q1)) = parseNameLit cs caq
	    handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expAnEntName,[c])
					   val a1 = hookError(a,(getPos q,err))
				       in raise SyntaxError(c,a1,q)
				       end
	 val _ = if c1=0wx3B then () 
		 else let val a2 = hookError(a1,(getPos q1,ERR_EXPECTED(expSemi,[c1])))
		      in raise SyntaxError(c1,a2,q1)
		      end
						    
	 val idx = GenEnt2Index dtd name
	 val (ent,ext) = getGenEnt dtd idx 
	       
	 val _ = (* check whether entity is undeclared/unparsed/open *)
	    case ent
	      of GE_NULL => 
		 if entitiesWellformed dtd 
		    then let val err = ERR_UNDEC_ENTITY(ENT_GENERAL,name)
			     val a2 = hookError(a1,(getPos q,err))
			 in raise NoSuchEntity (a2,q1)
			 end
		 else if useParamEnts()
			 then let val err = ERR_UNDECLARED(IT_GEN_ENT,name,LOC_NONE)
				  val a2 = hookError(a1,(getPos q,err))
			      in raise NoSuchEntity (a2,q1)
			      end
		      else ()
	       | GE_UNPARSED _ => let val err = ERR_ILLEGAL_ENTITY(ENT_UNPARSED,name,LOC_NONE)
				      val a2 = hookError(a1,(getPos q,err))
				  in raise NoSuchEntity (a2,q1)
				  end
	       | _ => if isOpen(idx,false,q1) 
			 then let val err = ERR_RECURSIVE_ENTITY(ENT_GENERAL,name)
				  val a2 = hookError(a1,(getPos q,err))
			      in raise NoSuchEntity (a2,q1)
			      end
		      else ()
			    
	 val a2 = 
	    if ext andalso !O_VALIDATE andalso standsAlone dtd andalso inDocEntity q1
	       then let val _ = if !O_ERROR_MINIMIZE then setStandAlone dtd false else ()
		    in hookError(a1,(getPos q,ERR_STANDALONE_ENT(ENT_GENERAL,name)))
		    end
	    else a1

      in (c1::cs1,((idx,ent),(a2,q1)))
      end

   (*--------------------------------------------------------------------*)
   (* parse a parameter entity reference, the "%" already read. See 4.1: *)
   (*                                                                    *)
   (*   [69] PEReference ::= '%' Name ';'        [ VC: Entity Declared ] *)
   (*                                            [ WFC: No Recursion ]   *)
   (*                                            [ WFC: In DTD ]         *)
   (*                                                                    *)
   (*   Well-Formedness Constraint: Entity Declared                      *)
   (*   In a document without any DTD, a document with only an internal  *)
   (*   DTD subset which contains no parameter entity references, or a   *)
   (*   document with "standalone='yes'", the Name given in the entity   *)
   (*   reference must match that in an entity declaration, ...          *)
   (*   The declaration of a parameter entity must precede any reference *)
   (*   to it...                                                         *)
   (*                                                                    *)
   (*   Validity Constraint: Entity Declared                             *)
   (*   In a document with an external subset or external parameter      *)
   (*   entities with "standalone='no'", the Name given in the entity    *)
   (*   reference must match that in an entity declaration. ...          *)
   (*   The declaration of a parameter entity must precede any reference *)
   (*   to it...                                                         *)
   (*                                                                    *)
   (* Thus: in both cases it is an error if the entity is not declared.  *)
   (* The only difference is the impact on well-formednes/validity.      *)
   (* Because the thing to be parsed is a parameter entity reference,    *)
   (* this DTD has references, and thus an undeclared entity is probably *)
   (* a validity and not a well-formedness error. Thus setExternal must  *)
   (* be called before determining a possible error!                     *)
   (*                                                                    *)
   (*   Well-Formedness Constraint: No Recursion                         *)
   (*   A parsed entity must not contain a recursive reference to        *)
   (*   itself, either directly or indirectly.                           *)
   (*                                                                    *)
   (* print an error and raise SyntaxError if no name is found, or if no *)
   (* semicolon follows it.                                              *)
   (* print an error and return PE_NULL if the reference is to an        *)
   (* undeclared or open entity.                                         *) 
   (*                                                                    *)
   (* return the entity referred to, and the remaining state.            *)
   (*--------------------------------------------------------------------*)
   fun parseParRef dtd (caq as (_,_,q)) = 
      let 
	 val (name,(c1,a1,q1)) = parseName caq 
	    handle NotFound(c,a,q) => let val err = ERR_EXPECTED(expAnEntName,[c])
					  val a1 = hookError(a,(getPos q,err))
				      in raise SyntaxError(c,a1,q)
				      end
		  
	 val _ = if c1=0wx3B then () 
		 else let val err = ERR_EXPECTED(expSemi,[c1])
			  val a2 = hookError(a1,(getPos q1,err))
		      in raise SyntaxError(c1,a2,q1)
		      end
		       
         val _ = setExternal dtd;
	 val idx = ParEnt2Index dtd name
	 val (ent,ext) = getParEnt dtd idx 
	       
	 val _ = (* check whether entity is declared *)
	    case ent 
	      of PE_NULL =>  
		 if entitiesWellformed dtd
		    then let val err = ERR_UNDEC_ENTITY(ENT_PARAMETER,name)
			     val a2 = hookError(a1,(getPos q,err))
			 in raise NoSuchEntity (a2,q1)
			 end
		 else if useParamEnts()
			 then let val err = ERR_UNDECLARED(IT_PAR_ENT,name,LOC_NONE)
				  val a2 = hookError(a1,(getPos q,err))
			      in raise NoSuchEntity (a2,q1)
			      end
		      else ()
	       (* check whether the entity is already open *)
	       | _ => if isOpen(idx,true,q1) 
			 then let val err = ERR_RECURSIVE_ENTITY(ENT_PARAMETER,name)
				  val a2 = hookError(a1,(getPos q,err))
			      in raise NoSuchEntity (a2,q1)
			      end
		      else ()
      in ((idx,ent),(a1,q1))	 
      end
   fun parseParRefLit dtd cs (caq as (_,_,q)) = 
      let 
	 val (cs1,name,(c1,a1,q1)) = parseNameLit cs caq 
	    handle NotFound(c,a,q) => let val err = ERR_EXPECTED(expAnEntName,[c])
					  val a1 = hookError(a,(getPos q,err))
				      in raise SyntaxError(c,a1,q)
				      end
		  
	 val _ = if c1=0wx3B then () 
		 else let val err = ERR_EXPECTED(expSemi,[c1])
			  val a2 = hookError(a1,(getPos q1,err))
		      in raise SyntaxError(c1,a2,q1)
		      end
		       
         val _ = setExternal dtd;
	 val idx = ParEnt2Index dtd name
	 val (ent,ext) = getParEnt dtd idx 
	       
	 val _ = (* check whether entity is declared *)
	    case ent 
	      of PE_NULL =>  
		 if entitiesWellformed dtd 
		    then let val err = ERR_UNDEC_ENTITY(ENT_PARAMETER,name)
			     val a2 = hookError(a1,(getPos q,err))
			 in raise NoSuchEntity (a2,q1)
			 end
		 else if useParamEnts()
			 then let val err = ERR_UNDECLARED(IT_PAR_ENT,name,LOC_NONE)
				  val a2 = hookError(a1,(getPos q,err))
			      in raise NoSuchEntity (a2,q1)
			      end
		      else ()
	       (* check whether the entity is already open *)
	       | _ => if isOpen(idx,true,q1) 
			 then let val err = ERR_RECURSIVE_ENTITY(ENT_PARAMETER,name)
				  val a2 = hookError(a1,(getPos q,err))
			      in raise NoSuchEntity (a2,q1)
			      end
		      else ()
      in (c1::cs1,((idx,ent),(a1,q1)))
      end

   (*--------------------------------------------------------------------*)
   (* skip a general/parameter entity reference, the "&/%" already read. *)
   (*                                                                    *)
   (* print an error if no name is found, or if no semicolon follows it. *)
   (*                                                                    *)
   (* handle any SyntaxState by returning its char and state.            *)
   (*                                                                    *)
   (* return the remaining state.                                        *)
   (*--------------------------------------------------------------------*)
   fun skipReference caq = 
      let val (_,(c1,a1,q1)) = parseName caq 
      in if c1=0wx3B then getChar (a1,q1) 
	 else let val err = ERR_EXPECTED(expSemi,[c1])
		  val a2 = hookError(a1,(getPos q1,err))
	      in (c1,a2,q1)    
	      end
      end
   handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expAnEntName,[c])
				  val a1 = hookError(a,(getPos q,err))
			      in (c,a1,q)	    
			      end

   (*--------------------------------------------------------------------*)
   (* skip a character reference, the "&#" already read. See 4.1:        *)
   (*                                                                    *)
   (* print an error if no number or x plus hexnum is found, or if no    *)
   (* semicolon follows it.                                              *)
   (*                                                                    *)
   (* handle any SyntaxState by returning its char and state.            *)
   (*                                                                    *)
   (* return the remaining char and state.                               *)
   (*--------------------------------------------------------------------*)
   fun skipCharRef aq =
      let 
	 (*--------------------------------------------------------------*)
	 (* skip a (hexa)decimal number.                                 *)
	 (*--------------------------------------------------------------*)
	 fun skip_ximal isX (c,a,q) = 
	    if isX c then skip_ximal isX (getChar (a,q)) else (c,a,q)
			       
	 val (c1,a1,q1) = getChar aq
	 val (c2,a2,q2) = 
	    if isDec c1 then skip_ximal isDec (getChar (a1,q1))
	    else if c1=0wx78 (* #"x" *) 
		    then let val (c2,a2,q2) = getChar (a1,q1)
			 in if isHex c2 then skip_ximal isHex (getChar (a2,q2))
			    else let val err = ERR_EXPECTED(expHexDigit,[c2])
				     val a3 = hookError(a2,(getPos q2,err))
				 in raise SyntaxError(c2,a3,q2)
				 end
			 end
		 else let val a2 = hookError(a1,(getPos q1,ERR_EXPECTED(expDigitX,[c1])))
		      in raise SyntaxError (c1,a2,q1)
		      end
				   
      in if c2=0wx3B then getChar (a2,q2) 
	 else (c2,hookError(a2,(getPos q2,ERR_EXPECTED(expSemi,[c2]))),q2)
      end
   handle SyntaxError caq => caq

   (*--------------------------------------------------------------------*)
   (* parse a sequence of white space in markup declarations. Cf. 2.3:   *)
   (*                                                                    *)
   (*   [3] S ::= (#x20 | #x9 | #xD | #xA)+                              *)
   (*                                                                    *)
   (* and 2.8 states:                                                    *)
   (*                                                                    *)
   (*   The markup declarations may be made up in whole or in part of    *)
   (*   the replacement text of parameter entities. The productions      *)
   (*   later in this specification for individual nonterminals          *)
   (*   (elementdecl, AttlistDecl, and so on) describe the declarations  *)
   (*   after all the parameter entities have been included.             *)
   (*                                                                    *)
   (* in markup declarations, we thus have to include entity references  *)
   (* and skip entity ends, except for the document end.                 *)
   (*                                                                    *)
   (*   Well-Formedness Constraint: PEs in Internal Subset               *)
   (*   In the internal DTD subset, parameter-entity references can      *)
   (*   occur only where markup declarations can occur, not within       *)
   (*   markup declarations. (This does not apply to references that     *)
   (*   occur in external parameter entities or to the external subset.) *)
   (*                                                                    *)
   (* we therefore always check whether we are in the internal subset    *)
   (* before including a parameter entity.                               *)
   (*--------------------------------------------------------------------*)
   (* handle a parameter entity reference                                *)
   (*--------------------------------------------------------------------*)
   fun doParRef dtd (caq as (c,a,q)) =
      if inDocEntity q 
         then let val err = ERR_FORBIDDEN_HERE(IT_PAR_REF,LOC_INT_DECL)
                  val a1 = hookError(a,(getPos q,err))
              in skipReference (c,a1,q)
              end
      else let val ((id,ent),(a1,q1)) = parseParRef dtd caq
           in case ent
                of PE_NULL => getChar (a1,q1)
                 | PE_INTERN (_,rep) => getChar(a1,(pushIntern(q1,id,true,rep)))
                 | PE_EXTERN extId => #3(openExtern(id,true,resolveExtId extId) (a1,q1))
                   handle CantOpenFile(fmsg,a) 
                   => let val err = ERR_NO_SUCH_FILE fmsg
                          val a1 = hookError(a,(getPos q1,err))
                      in (getChar(a1,q1))
                      end
           end
        handle SyntaxError caq => caq
             | NoSuchEntity aq => getChar aq
   (*--------------------------------------------------------------------*)
   (* parse optional white space.                                        *)
   (*                                                                    *)
   (* catch SyntaxState exceptions from parameter refs.                  *)
   (*                                                                    *)
   (* print an error if a parameter entity reference or an entity end is *)
   (* found inside the internal subset.                                  *)
   (*                                                                    *)
   (* return the following character and the remaining state.            *)
   (*--------------------------------------------------------------------*)
   fun skipPSopt dtd caq =
      let fun doit (c,a,q) = 
	 case c
	   of 0wx00 => 
	      if isSpecial q then (c,a,q) 
	      else let val a1 = if !O_VALIDATE andalso inDocEntity q 
				   then hookError(a,(getPos q,ERR_EE_INT_SUBSET))
				else a
		   in doit (getChar (a1,q))
		   end
	    | 0wx09 => doit (getChar (a,q))
	    | 0wx0A => doit (getChar (a,q))
	    | 0wx20 => doit (getChar (a,q))
	    | 0wx25 (* #"%" *) => doit (doParRef dtd (getChar (a,q)))
	 | _ => (c,a,q)
      in doit caq
      end
   (*--------------------------------------------------------------------*)
   (* parse optional white space.                                        *)
   (*                                                                    *)
   (* catch SyntaxState exceptions from parameter refs.                  *)
   (*                                                                    *)
   (* print an error if a parameter entity reference or an entity end is *)
   (* found inside the internal subset.                                  *)
   (*                                                                    *)
   (* return a boolean whether white space was actually found, and the   *)
   (* following character with the remaining state.                      *)
   (*--------------------------------------------------------------------*)
   fun skipPSmay dtd (c,a,q) =
      case c 
	of 0wx00 => 
	   if isSpecial q then (false,(c,a,q)) 
	   else let val a1 = if !O_VALIDATE andalso inDocEntity q 
				then hookError(a,(getPos q,ERR_EE_INT_SUBSET))
			     else a
		in (true,skipPSopt dtd (getChar (a1,q)))
		end
	 | 0wx09 => (true,skipPSopt dtd (getChar (a,q)))
	 | 0wx0A => (true,skipPSopt dtd (getChar (a,q)))
	 | 0wx20 => (true,skipPSopt dtd (getChar (a,q)))
         | 0wx25 (* #"%" *) => (true,skipPSopt dtd (doParRef dtd (getChar (a,q))))
	 | _ => (false,(c,a,q))
   (*--------------------------------------------------------------------*)
   (* parse required white space.                                        *)
   (*                                                                    *)
   (* catch SyntaxState exceptions from parameter refs.                  *)
   (*                                                                    *)
   (* print an error and return if no white space character is found.    *)
   (* print an error if a parameter entity reference or an entity end is *)
   (* found inside the internal subset.                                  *)
   (*                                                                    *)
   (* return the following character and the remaining state.            *)
   (*--------------------------------------------------------------------*)
   fun skipPS dtd (c,a,q) = 
      case c
	of 0wx00 => 
	   if isSpecial q then (c,hookError(a,(getPos q,ERR_MISSING_WHITE)),q)
	   else let val a1 = if !O_VALIDATE andalso inDocEntity q 
				then hookError(a,(getPos q,ERR_EE_INT_SUBSET))
			     else a
		in skipPSopt dtd (getChar (a1,q))
		end
	 | 0wx09 => skipPSopt dtd (getChar (a,q))
	 | 0wx0A => skipPSopt dtd (getChar (a,q))
	 | 0wx20 => skipPSopt dtd (getChar (a,q))
         | 0wx25 (* #"%" *) => skipPSopt dtd (doParRef dtd (getChar (a,q)))
	 | _ => (c,hookError(a,(getPos q,ERR_MISSING_WHITE)),q)
   (*--------------------------------------------------------------------*)
   (* parse required white space, taking care of a single '%' character. *)
   (* this is only needed before the entity name in an entity decl.      *)
   (*                                                                    *)
   (* catch SyntaxState exceptions from parameter refs.                  *)
   (*                                                                    *)
   (* print an error if no white space character is found.               *)
   (* print an error if a parameter entity reference or an entity end is *)
   (* found inside the internal subset.                                  *)
   (*                                                                    *)
   (* return a boolean whether a '%' was found,  the following character *)
   (* and the remaining state.                                           *)
   (*--------------------------------------------------------------------*)
   fun skipPSdec dtd caq = 
      let fun doit req (c,a,q) = 
	 case c
	   of 0wx00 => 
	      if isSpecial q then (false,(c,a,q)) 
	      else let val a1 = if !O_VALIDATE andalso inDocEntity q 
				   then hookError(a,(getPos q,ERR_EE_INT_SUBSET))
				else a
		   in doit false (getChar (a1,q))
		   end
	    | 0wx09 => doit false (getChar (a,q))
	    | 0wx0A => doit false (getChar (a,q))
	    | 0wx20 => doit false (getChar (a,q))
	    | 0wx25 => (* #"%" *)
		   let val (c1,a1,q1) = getChar (a,q)
		   in if isNms c1 then doit false (doParRef dtd (c1,a1,q1))
		      else let val a2 = if req then hookError(a1,(getPos q,ERR_MISSING_WHITE))
					else a1
			   in (true,(c1,a2,q1))
			   end
		   end
	    | _ => let val a1 = if req then hookError(a,(getPos q,ERR_MISSING_WHITE))
				else a
		   in (false,(c,a1,q))
		   end
      in 
	 doit true caq 
      end
end

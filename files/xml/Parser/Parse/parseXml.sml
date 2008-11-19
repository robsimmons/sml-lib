signature ParseXml =
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
      ----------------------------------------------------------------------*)
      include ParseMisc

      val openDocument : Uri.Uri option -> AppData 
	 -> Encoding.Encoding * HookData.XmlDecl option * (UniChar.Char * AppData * State)
      val openSubset   : Uri.Uri -> AppData 
	 -> Encoding.Encoding * HookData.TextDecl option * (UniChar.Char * AppData * State)
      val openExtern   : int * bool * Uri.Uri -> AppData * State 
	 -> Encoding.Encoding * HookData.TextDecl option * (UniChar.Char * AppData * State)
   end

(*--------------------------------------------------------------------------*)
(* Structure: ParseXml                                                      *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   openDocument : NoSuchFile                                              *)
(*   openExtern   : none                                                    *)
(*   openSubset   : NoSuchFile                                              *)
(*--------------------------------------------------------------------------*)
functor ParseXml (structure ParseBase : ParseBase) 
   : ParseXml =
struct
   structure ParseMisc  = ParseMisc  (structure ParseBase = ParseBase)

   open 
      Errors UniChar UniClasses UtilString
      ParseMisc
	 
   fun checkVersionNum (a,q) version =
      if not (!O_CHECK_VERSION) orelse version="1.0" orelse version ="1.1" then a  (* XML 1.1 also allowed *)
      else hookError(a,(getPos q,ERR_VERSION version))

   (*--------------------------------------------------------------------*)
   (* parse a version number, the quote character ("'" or '"') passed as *)
   (* first argument.  cf. 2.8:                                          *)
   (*                                                                    *)
   (*   [24] VersionInfo ::= S 'version' Eq (' VersionNum '              *)
   (*                                       | " VersionNum ")            *)
   (*   [26]  VersionNum ::= ([a-zA-Z0-9_.:] | '-')+                     *)
   (*                                                                    *)
   (* print an error and end the literal if an entity end is found.      *)
   (* print an error if a disallowed character is found.                 *)
   (*                                                                    *)
   (* return the version number as a string option, together with the    *)
   (* next character and state.                                          *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *) 
   (*--------------------------------------------------------------------*)
   fun parseVersionNum quote aq =
      let 
	 fun doit text (c,a,q) = 
	    if c=quote then (text,getChar (a,q))
	    else if isVers c then doit (c::text) (getChar (a,q))
	    else if c=0wx0
		    then let val a1 = hookError(a,(getPos q,ERR_ENDED_BY_EE LOC_VERSION))
			 in (text,(c,a1,q))
			 end
		 else let val err = ERR_FORBIDDEN_HERE(IT_CHAR c,LOC_VERSION)
			  val a1 = hookError(a,(getPos q,err))
		      in doit text (getChar (a1,q))
		      end

	 val (c1,a1,q1) = getChar aq

	 val (text,(c2,a2,q2)) = 
	    if isVers c1 then doit [c1] (getChar (a1,q1))
	    else if c1=quote 
		    then let val a2 = hookError(a1,(getPos q1,ERR_EMPTY LOC_VERSION))
			 in (nil,getChar (a2,q1))
			 end
	    else if c1=0wx00 
		    then let val a2 = hookError(a1,(getPos q1,ERR_ENDED_BY_EE LOC_VERSION))
			     val a3 = hookError(a2,(getPos q1,ERR_EMPTY LOC_VERSION))
			 in (nil,(c1,a3,q1)) 
			 end
		 else let val err = ERR_FORBIDDEN_HERE(IT_CHAR c1,LOC_VERSION)
			  val a2 = hookError(a1,(getPos q1,err))
		      in doit nil (getChar (a2,q1))
		      end
	 val version = Latin2String (rev text)
	 val a3 = checkVersionNum (a2,q1) version
      in 
	 (SOME version,(c2,a3,q2))
      end
   (*--------------------------------------------------------------------*)
   (* parse a version info starting after 'version'. Cf. 2.8:            *)
   (*                                                                    *)
   (*   [24] VersionInfo ::= S 'version' Eq (' VersionNum '              *)
   (*                                       | " VersionNum ")            *)
   (*                                                                    *)
   (* print an error and raise SyntaxState if no '=' is found.           *)
   (* print an error and raise SyntaxState if no quote sign is found.    *)
   (*                                                                    *)
   (* return the version number as a string option, together with the    *)
   (* next char and the remaining state.                                 *) 
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *) 
   (*--------------------------------------------------------------------*)
   fun parseVersionInfo caq =
      let val (c1,a1,q1) = skipEq caq
      in case c1
	   of 0wx22 (* '""' *) => parseVersionNum c1 (a1,q1) 
	    | 0wx27 (* "'" *)  => parseVersionNum c1 (a1,q1) 
	    | _ => let val a2 = hookError(a1,(getPos q1,ERR_EXPECTED(expLitQuote,[c1])))
		   in raise SyntaxError(c1,a2,q1)	
		   end
      end   

   (*--------------------------------------------------------------------*)
   (* parse an encoding name, the quote character ("'" or '"') passed as *)
   (* first argument.  cf. 4.3.3:                                        *)
   (*                                                                    *)
   (*   [80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"'           *)
   (*                                         |"'" EncName  "'")         *)
   (*                                                                    *) 
   (*   [81] EncName ::= [A-Za-z]                /* Encoding name        *)
   (*                    ([A-Za-z0-9._] | '-')*     contains only Latin  *)  
   (*                                               characters */        *) 
   (*                                                                    *)
   (* print an error and end the literal if an entity end is found.      *)
   (* print an error if a disallowed character is found.                 *)
   (*                                                                    *)
   (* return the encoding name as a string option, together with the     *)
   (* next character and state.                                          *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *) 
   (*--------------------------------------------------------------------*)
   fun parseEncName quote aq =
      let 
	 fun doit text (c,a,q) = 
	    if c=quote then (text,getChar (a,q))
	    else if isEnc c then doit (c::text) (getChar (a,q))
	    else if c=0wx00
		    then let val a1 = hookError(a,(getPos q,ERR_ENDED_BY_EE LOC_ENCODING))
			 in (text,(c,a1,q))
			 end
		 else let val err = ERR_FORBIDDEN_HERE(IT_CHAR c,LOC_ENCODING)
			  val a1 = hookError(a,(getPos q,err))
		      in doit text (getChar (a,q))
		      end

	 val (c1,a1,q1) = getChar aq

	 val (text,caq2) = 
	    if isEncS c1 then doit [c1] (getChar (a1,q1))
	    else if c1=quote 
		    then let val a2 = hookError(a1,(getPos q1,ERR_EMPTY LOC_ENCODING))
			 in (nil,getChar (a2,q1)) 
			 end
	    else if c1=0wx00 
		    then let val a2 = hookError(a1,(getPos q1,ERR_ENDED_BY_EE LOC_ENCODING))
			     val a3 = hookError(a2,(getPos q1,ERR_EMPTY LOC_ENCODING))
			 in (nil,(c1,a3,q1))
			 end
		 else let val a2 = hookError(a1,(getPos q1,ERR_EXPECTED(expStartEnc,[c1])))
		      in doit nil (getChar (a2,q1))
		      end

	 val enc = toUpperString (Latin2String (rev text))
      in 
	 (enc,caq2)
      end
   (*--------------------------------------------------------------------*)
   (* parse an encoding decl starting after 'encoding'. Cf. 4.3.3:       *)
   (*                                                                    *)
   (*                                                                    *)
   (*   [80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"'           *)
   (*                                         |"'" EncName  "'")         *)
   (*                                                                    *) 
   (* print an error and raise SyntaxState if no '=' is found.           *)
   (* print an error and raise SyntaxState if no quote sign is found.    *)
   (*                                                                    *)
   (* return the encoding name as a string option, together with the     *)
   (* next char and the remaining state.                                 *) 
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *) 
   (*--------------------------------------------------------------------*)
   fun parseEncodingDecl caq =
      let val (c1,a1,q1) = skipEq caq 
      in case c1
	   of 0wx22 (* '""' *) => parseEncName c1 (a1,q1) 
	    | 0wx27 (* "'" *)  => parseEncName c1 (a1,q1) 
	    | _ => let val a2 = hookError(a1,(getPos q1,ERR_EXPECTED(expLitQuote,[c1])))
		   in raise SyntaxError(c1,a2,q1)	
		   end
      end

   (*--------------------------------------------------------------------*)
   (* parse a standalone declaration starting after 'standalone'.        *)
   (* Cf. 2.9:                                                           *)
   (*                                                                    *)
   (*   [32] SDDecl ::= S 'standalone' Eq            [ VC: Standalone    *)
   (*                   ( ("'" ('yes' | 'no') "'")         Document      *)
   (*                   | ('"' ('yes' | 'no') '"'))        Declaration ] *)
   (*                                                                    *) 
   (* print an error and raise SyntaxState if no '=' is found.           *)
   (* print an error and raise SyntaxState if no literal is found.       *)
   (* print an error and end the literal if an entity end is found.      *)
   (* print an error if the literal is neither 'yes' nor 'no'.           *)
   (*                                                                    *)
   (* return the standalone status as a boolean option, together with    *)
   (* the next character and the remaining state.                        *) 
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *) 
   (*--------------------------------------------------------------------*)
   fun parseStandaloneDecl caq0 =
      let 
	 val (quote,a,q) = skipEq caq0

	 fun doit text (c,a,q) = 
	    if c=quote then (text,getChar (a,q))
	    else if c<>0wx0 then doit (c::text) (getChar (a,q))
		 else let val a1 = hookError(a,(getPos q,ERR_ENDED_BY_EE LOC_LITERAL))
		      in (text,(c,a1,q)) 
		      end

	 val caq1 as (_,_,q1) = 
	    case quote
	      of 0wx22 (* '""' *) => (getChar (a,q))
	       | 0wx27 (* "'" *)  => (getChar (a,q))
	       | _ => let val a1 = hookError(a,(getPos q,ERR_EXPECTED(expLitQuote,[quote])))
		      in raise SyntaxError(quote,a1,q)	
		      end
	 val (text,caq2) = doit nil caq1
      in 
	 case text
	   of [0wx73,0wx65,0wx79] (* reversed "yes" *) => (SOME true,caq2)
	    | [0wx6f,0wx6e]       (* reversed "no"  *) => (SOME false,caq2)
	    | revd => let val (c2,a2,q2) = caq2 
			  val a3 = hookError(a2,(getPos q1,ERR_EXPECTED(expNoYes,revd)))
		      in (NONE,(c2,a3,q2))
		      end   
      end

   (*--------------------------------------------------------------------*)
   (* parse an xml declaration starting after 'xml ' (i.e. the first     *)
   (* white space character is already consumed). Cf. 2.8:               *)
   (*                                                                    *) 
   (*   [23] XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S?'?>'*)
   (*                                                                    *) 
   (*   [24] VersionInfo ::= S 'version' Eq (' VersionNum '              *)
   (*                                       | " VersionNum ")            *)
   (*                                                                    *) 
   (*   [32] SDDecl ::= S 'standalone' Eq             [ VC: Standalone   *)
   (*                   ( ("'" ('yes' | 'no') "'")         Document      *)
   (*                   | ('"' ('yes' | 'no') '"'))        Declaration ] *)
   (*                                                                    *) 
   (*   [80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"'           *)
   (*                                         |"'" EncName  "'")         *)
   (*                                                                    *) 
   (* default version, encoding and standalone status to NONE.           *)
   (*                                                                    *) 
   (* print an error if no leading white space is found.                 *)
   (* print an error whenever a wrong name is encountered.               *)
   (* print an Error if no VersionInfo is found.                         *)
   (* print an Error if no '?>' is found at the end.                     *)
   (* print an error and raise SyntaxState if no '=' or no literal is    *)
   (* found in VersionInfo, EncodingDecl or SDDecl.                      *)
   (* print an error if a literal does not have a correct value.         *)
   (*                                                                    *)
   (* return the corresponding XmlDecl option and the next char & state. *)
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *) 
   (*--------------------------------------------------------------------*)
   fun parseXmlDecl auto caq =
      let 
	 (*-----------------------------------------------------------------*)
	 (* skip the '?>' at the end of the xml declaration.                *)
	 (*                                                                 *) 
	 (* print an error and raise SyntaxState if no '?>' is found.       *) 
	 (*                                                                 *) 
	 (* return the info passed as first arg, and the next char & state. *)
	 (*-----------------------------------------------------------------*)
	 (* might raise: SyntaxState                                        *) 
	 (*-----------------------------------------------------------------*)
	 fun skipXmlDeclEnd enc res (c,a,q) = 
	    if c=0wx3F (* "#?" *) 
	       then let val (c1,a1,q1) = getChar (a,q)
		    in if c1=0wx3E (* #">" *) then (enc,SOME res,getChar (a1,q1))
		       else let val a2 = hookError(a1,(getPos q1,ERR_EXPECTED(expGt,[c1]))) 
			    in raise SyntaxError (c1,a2,q1)
			    end
		    end
	    else let val a1 = hookError(a,(getPos q,ERR_EXPECTED(expProcEnd,[c]))) 
		 in raise SyntaxError (c,a1,q)
		 end
	 (*-----------------------------------------------------------------*)
	 (* parse the remainder after the keyword 'standalone', the version *)
	 (* and encoding already parsed and given in the first arg.         *) 
	 (*                                                                 *) 
	 (* pass the version,encoding and sd status to skipXmlDeclEnd       *)
	 (*-----------------------------------------------------------------*)
	 (* might raise: SyntaxState                                        *) 
	 (*-----------------------------------------------------------------*)
	 fun parseXmlDeclAfterS enc (v,e) caq = 
	    let 
	       val (alone,caq1) = parseStandaloneDecl caq
	       val caq2 = skipSopt caq1
	    in skipXmlDeclEnd enc (v,e,alone) caq2
	    end
	 (*-----------------------------------------------------------------*)
	 (* parse the remainder after the encoding declaration, the version *)
	 (* and encoding already parsed and given in the first arg.         *) 
	 (*                                                                 *) 
	 (* print an error if a name other than 'standalone' is found.      *) 
	 (*                                                                 *) 
	 (* pass the version and encoding to parseXmlDeclAfterS.            *)
	 (*-----------------------------------------------------------------*)
	 (* might raise: SyntaxState                                        *) 
	 (*-----------------------------------------------------------------*)
	 fun parseXmlDeclBeforeS enc (v,e) caq = 
	    let
	       val (hadS,caq1 as (_,_,q1)) = skipSmay caq
	       val (name,(c2,a2,q2)) = parseName caq1 (* NotFound handled below *)
	       val a3 = if hadS then a2 
			else hookError(a2,(getPos q1,ERR_MISSING_WHITE))
	    in case name 
		 of [0wx73,0wx74,0wx61,0wx6e,0wx64,0wx61,0wx6c,0wx6f,0wx6e,0wx65] => 
		  (* "standalone" *) parseXmlDeclAfterS enc (v,e) (c2,a3,q2)
		  | _ => let val a4 = hookError(a3,(getPos q1,ERR_EXPECTED(expStandOpt,name)))
			 in parseXmlDeclAfterS enc (v,e) (c2,a4,q2)
			 end
	    end
	 handle NotFound caq => (* exception raised by parseName *) 
	    skipXmlDeclEnd enc (v,e,NONE) caq 
	 (*-----------------------------------------------------------------*)
	 (* parse the remainder after the keyword 'encoding', the version   *)
	 (* already parsed and given in the first arg.                      *) 
	 (*                                                                 *) 
	 (* pass the version and encoding and to parseXmlDeclBeforeS        *)
	 (*-----------------------------------------------------------------*)
	 (* might raise: SyntaxState                                        *) 
	 (*-----------------------------------------------------------------*)
	 fun parseXmlDeclAfterE ver caq = 
	    let 
	       val (enc,(c1,a1,q1)) = parseEncodingDecl caq
	       val (a2,q2,enc1) = changeAuto(a1,q1,enc)
	    in 
	       parseXmlDeclBeforeS enc1 (ver,SOME enc) (c1,a2,q2)
	    end
	 (*-----------------------------------------------------------------*)
	 (* parse the remainder after the version info, the version already *)
	 (* parsed and given in the first arg.                              *) 
	 (*                                                                 *) 
	 (* print an error if a name other than 'encoding' or 'standalone'  *)
	 (* is found.                                                       *) 
	 (*                                                                 *) 
	 (* pass obtained/default values to parseXmlDeclAfter[E|S] or to    *)
	 (* skipXmlDeclEnd.                                                 *)
	 (*-----------------------------------------------------------------*)
	 (* might raise: SyntaxState                                        *) 
	 (*-----------------------------------------------------------------*)
	 fun parseXmlDeclBeforeE ver caq = 
	    let
	       val (hadS,caq1 as (_,_,q1)) = skipSmay caq
	       val (name,(c2,a2,q2)) = parseName caq1 (* NotFound handled below *)
	       val a3 = if hadS then a2 
			else hookError(a2,(getPos q1,ERR_MISSING_WHITE))
	    in 
	       case name 
		 of [0wx65,0wx6e,0wx63,0wx6f,0wx64,0wx69,0wx6e,0wx67] => 
		  (* "encoding" *) parseXmlDeclAfterE ver (c2,a3,q2)
		  | [0wx73,0wx74,0wx61,0wx6e,0wx64,0wx61,0wx6c,0wx6f,0wx6e,0wx65] => 
		  (* "standalone" *) parseXmlDeclAfterS auto (ver,NONE) (c2,a3,q2)
		  | _ => let val a4 = hookError(a3,(getPos q1,ERR_EXPECTED(expEncStand,name)))
			 in parseXmlDeclAfterE ver (c2,a4,q2)
			 end
	    end
	 handle NotFound caq => (* exception raised by parseName *) 
	    skipXmlDeclEnd auto (ver,NONE,NONE) caq 

	 (*-----------------------------------------------------------------*)
	 (* do the main work. if the first name is not 'version' then it    *)
	 (* might be 'encoding' or 'standalone'. Then take the default      *) 
	 (* NONE for version and - if needed - encoding and call the        *)
	 (* appropriate function. otherwise assume a typo and parse the     *)
	 (* version number, then call parseXmlDeclBeforeE. if no name is    *) 
	 (* found at all, proceed with skipXmlDeclEnd.                      *)
	 (*                                                                 *)
	 (* print an error and raise SyntaxState if an entity end is found. *)
	 (* print an error and raise SyntaxState if appropriate.            *)
	 (* print an error if a name other than 'version' is found.         *)
	 (*-----------------------------------------------------------------*)
	 (* might raise: SyntaxState                                        *) 
	 (*-----------------------------------------------------------------*)
	 val caq1 as (_,_,q1) = skipSopt caq 
	 val (name,(caq2 as (c2,a2,q2))) = parseName caq1 
	    handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expVersion,[c])
					   val a1 = hookError(a,(getPos q,err))
				       in raise SyntaxError (c,a1,q)
				       end
      in 
	 if name=[0wx76,0wx65,0wx72,0wx73,0wx69,0wx6f,0wx6e] (* "version" *)
	    then let val (ver,caq3) = parseVersionInfo caq2 
		 in parseXmlDeclBeforeE ver caq3
		 end
	 else let val a3 = hookError(a2,(getPos q1,ERR_EXPECTED(expVersion,name)))
	      in case name
		   of [0wx65,0wx6e,0wx63,0wx6f,0wx64,0wx69,0wx6e,0wx67] => 
		    (* "encoding" *) parseXmlDeclAfterE NONE (c2,a3,q2)
		    | [0wx73,0wx74,0wx61,0wx6e,0wx64,0wx61,0wx6c,0wx6f,0wx6e,0wx65] => 
		    (* "standalone" *) parseXmlDeclAfterS auto (NONE,NONE) (c2,a3,q2)
		    | _ => let val (ver,caq3) = parseVersionInfo (c2,a3,q2) 
			   in parseXmlDeclBeforeE ver caq3
			   end
	      end
      end
      (*----------------------------------------------------------------*)
      (* catch entity end exceptions raised by subfunctions, print an   *)
      (* error and re-raise the exception.                              *)
      (*----------------------------------------------------------------*)
   handle SyntaxError(c,a,q) => 
      let val err = if c=0wx0 then ERR_ENDED_BY_EE LOC_XML_DECL
		    else ERR_CANT_PARSE LOC_XML_DECL
	  val a1 = hookError(a,(getPos q,err))
      in (auto,NONE,recoverXml(c,a1,q))
      end

   (*--------------------------------------------------------------------*)
   (* parse a text declaration starting after 'xml ' (i.e. the first     *)
   (* white space character is already consumed). Cf. 2.8:               *)
   (*                                                                    *) 
   (*   [77] TextDecl ::= '<?xml' VersionInfo? EncodingDecl S? '?>'      *)
   (*                                                                    *) 
   (*   [24] VersionInfo ::= S 'version' Eq (' VersionNum '              *)
   (*                                       | " VersionNum ")            *)
   (*                                                                    *) 
   (*   [80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"'           *)
   (*                                         |"'" EncName  "'")         *)
   (*                                                                    *) 
   (* default version and encoding to NONE.                              *)
   (*                                                                    *) 
   (* print an error if no leading white space is found.                 *)
   (* print an error whenever a wrong name is encountered.               *)
   (* print an Error if no EncodingDecl is found.                        *)
   (* print an Error if '?>' is found at the end.                        *)
   (* print an error and raise SyntaxState if no '=' or no literal is    *)
   (* found in VersionInfo or EncodingDecl.                              *)
   (* print an error if a literal does not have a correct value.         *)
   (*                                                                    *)
   (* return the corresponding TextDecl option and the next char & state.*)
   (*--------------------------------------------------------------------*)
   (* might raise: SyntaxState                                           *) 
   (*--------------------------------------------------------------------*)
   fun parseTextDecl auto caq =
      let 
	 (*-----------------------------------------------------------------*)
	 (* skip the '?>' at the end of the text declaration.               *)
	 (*                                                                 *) 
	 (* print an error and raise SyntaxState if no '?>' is found.       *) 
	 (*                                                                 *) 
	 (* return the info passed as first arg, and the next char & state. *)
	 (*-----------------------------------------------------------------*)
	 (* might raise: SyntaxState                                        *) 
	 (*-----------------------------------------------------------------*)
	 fun skipTextDeclEnd enc res (c,a,q) = 
	    if c=0wx3F (* "#?" *) 
	       then let val (c1,a1,q1) = getChar (a,q)
		    in if c1=0wx3E (* #">" *) then (enc,SOME res,getChar (a1,q1))
		       else let val a2 = hookError(a1,(getPos q1,ERR_EXPECTED(expGt,[c1])))
			    in raise SyntaxError(c1,a2,q1)
			    end
		    end
	    else let val a1 = hookError(a,(getPos q,ERR_EXPECTED(expProcEnd,[c])))
		 in raise SyntaxError(c,a1,q)
		 end
	 (*-----------------------------------------------------------------*)
	 (* parse the remainder after the keyword 'encoding', the version   *)
	 (* already parsed and given in the first arg.                      *) 
	 (*                                                                 *) 
	 (* pass the version and encoding and to skipTextDeclEnd.           *)
	 (*-----------------------------------------------------------------*)
	 (* might raise: SyntaxState                                        *) 
	 (*-----------------------------------------------------------------*)
	 fun parseTextDeclAfterE ver caq = 
	    let 
	       val (enc,(c1,a1,q1)) = parseEncodingDecl caq
	       val (a2,q2,enc1) = changeAuto(a1,q1,enc)
	       val caq3 = skipSopt (c1,a2,q2)
	    in skipTextDeclEnd enc1 (ver,SOME enc) caq3
	    end
	 (*-----------------------------------------------------------------*)
	 (* parse the remainder after the version info, the version given   *)
	 (* as first argument.                                              *) 
	 (*                                                                 *) 
	 (* print an error and raise SyntaxState is no name is found.       *) 
	 (* print an error if a name other than 'encoding' is found.        *) 
	 (*                                                                 *) 
	 (* pass obtained/default values to parseTextDeclAfterE.            *)
	 (*-----------------------------------------------------------------*)
	 (* might raise: SyntaxState                                        *) 
	 (*-----------------------------------------------------------------*)
	 fun parseTextDeclBeforeE ver caq = 
	    let
	       val caq1 as (_,_,q1) = skipS caq 
	       val (name,caq2) = parseName caq1
		  handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expEncoding,[c])
						 val a1 = hookError(a,(getPos q,err))
					     in raise SyntaxError (c,a1,q)
					     end
	    in 
	       if name=[0wx65,0wx6e,0wx63,0wx6f,0wx64,0wx69,0wx6e,0wx67] (* "encoding" *) 
		  then parseTextDeclAfterE ver caq2
	       else let val (c2,a2,q2) = caq2
			val a3 = hookError(a2,(getPos q1,ERR_EXPECTED(expEncoding,name)))
		    in parseTextDeclAfterE ver (c2,a3,q2)
		    end
	    end
	 (*-----------------------------------------------------------------*)
	 (* do the main work. if the first name is neither 'version' nor    *)
	 (* 'encoding' then assume typo of 'version'. Then parse the        *)
	 (* version number, call parseTextDeclBeforeE. if no name is found  *) 
	 (* at all, proceed with skipTextDeclEnd.                           *)
	 (*                                                                 *)
	 (* print an error and raise SyntaxState if appropriate.            *)
	 (* print an error if a name other than 'version' or 'encoding' is  *)
	 (* found.                                                          *)
	 (*-----------------------------------------------------------------*)
	 (* might raise: SyntaxState                                        *) 
	 (*-----------------------------------------------------------------*)
	 val caq1 as (_,_,q1) = skipSopt caq 
	 val (name,caq2) = parseName caq1 
	    handle NotFound (c,a,q) => let val err = ERR_EXPECTED(expEncVers,[c])
					   val a1 = hookError(a,(getPos q,err))
				       in raise SyntaxError(c,a1,q)
				       end
      in case name 
	   of [0wx76,0wx65,0wx72,0wx73,0wx69,0wx6f,0wx6e] => (* "version" *)
	      let val (ver,caq3) = parseVersionInfo caq2 
	      in parseTextDeclBeforeE ver caq3
	      end
	    | [0wx65,0wx6e,0wx63,0wx6f,0wx64,0wx69,0wx6e,0wx67] => (* "encoding" *)
	      parseTextDeclAfterE NONE caq2
	    | _ => let val (c2,a2,q2) = caq2
		       val a3 = hookError(a2,(getPos q1,ERR_EXPECTED(expEncVers,name)))
		       val (ver,caq3) = parseVersionInfo (c2,a3,q2) 
		   in parseTextDeclBeforeE ver caq3
		   end
      end
      (*----------------------------------------------------------------*)
      (* catch entity end exceptions raised by subfunctions, print an   *)
      (* error and re-raise the exception.                              *)
      (*----------------------------------------------------------------*)
   handle SyntaxError(c,a,q) => 
      let val err = if c=0wx0 then ERR_ENDED_BY_EE LOC_TEXT_DECL
		    else ERR_CANT_PARSE LOC_TEXT_DECL
	  val a1 = hookError(a,(getPos q,err))
      in (auto,NONE,recoverXml(c,a1,q))
      end
	 
   (*--------------------------------------------------------------------*)
   (* check for the string "<?xml" followed by a white space. The first  *)
   (* paramter seen is a prefix of that string already consued. If the   *)
   (* complete string is not found, unget all characters seen, including *)
   (* those from parameter seen.                                         *)
   (*                                                                    *)
   (* return a boolean indicating wheher the string was found, together  *)
   (* with the remaining app data and state.                             *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *) 
   (*--------------------------------------------------------------------*)
   fun checkForXml aq =
      let 
	 val unseen = [0wx3c,0wx3f,0wx78,0wx6d,0wx6c]
	 fun doit (seen,unseen) (a,q) = 
	    let val (c1,a1,q1) = getChar (a,q)
	    in case unseen 
		 of nil => if isS c1 then (true,(a1,q1))
			   else (false,(a1,ungetChars(q1,rev(c1::seen))))
		  | c::cs => if c1=c then doit (c1::seen,cs) (a1,q1)
			     else (false,(a1,ungetChars(q1,rev(c1::seen))))
	    end
      in doit (nil,unseen) aq
      end

   (*--------------------------------------------------------------------*)
   (* consume the text/xml declaration. The first parameter is a pair of *)
   (* the function that parses the declaration and a boolean indicating  *)
   (* whether a warning should we produced if the declaration is missing.*)
   (* The second parameter is a pair (seen,auto), where auto is the      *)
   (* auto-detected encoding, and seen is SOME cs, if auto-detection     *)
   (* found some initial characters cs of the string "<?xml", otherwise  *)
   (* NONE. A text/xml declaration can only be present if seen is SOME.  *)
   (*                                                                    *)
   (* Check whether the declaration is present with checkForXml. If yes, *)
   (* parse it, if no, possibly print a warning.                         *)
   (*                                                                    *)
   (* Return the encoding of the entity, the optional declaration and    *)
   (* the next char, app data and state.                                 *) 
   (*--------------------------------------------------------------------*)
   fun findTextDecl (parseDecl,warn) auto aq =
      let val (hasXml,aq1) = checkForXml aq
      in if hasXml then parseDecl auto (getChar aq1)
	 else let val (a1,q1) = aq1
		  val (a2,q2) = commitAuto(a1,q1)
		  val a3 = if warn then hookWarning(a2,(getPos q2,WARN_NO_XML_DECL)) else a2
	      in (auto,NONE,getChar(a3,q2))
	      end
      end

   (*--------------------------------------------------------------------*)
   (* open an external entity; consume its text declaration if present.  *)
   (* See 4.3.2:                                                         *)
   (*                                                                    *)
   (*   [78] extParsedEnt ::= TextDecl? content                          *)
   (*   [79]        extPE ::= TextDecl? extSubsetDecl                    *)
   (*                                                                    *)
   (* handle NoSuchFile by printing an error and opening an empty dummy  *)
   (* entity (some functions might rely on the entity's entity end).     *)
   (*                                                                    *)
   (* return the optional text declaration and the resulting first char  *)
   (* together with the new state.                                       *)
   (*--------------------------------------------------------------------*)
   (* might raise: none                                                  *) 
   (*--------------------------------------------------------------------*)
   fun openExtern (id,isParam,uri) (a,q) = 
      let val (q1,auto) = pushExtern (q,id,isParam,uri)
      in findTextDecl (parseTextDecl,false) auto (a,q1)
      end
   handle NoSuchFile fmsg => raise CantOpenFile(fmsg,a)

   (*--------------------------------------------------------------------*)
   (* open the external subset; consume its text declaration if present. *)
   (* See 2.8:                                                           *)
   (*                                                                    *)
   (*   [30]     extSubset ::= TextDecl? extSubsetDecl                   *)
   (*                                                                    *)
   (* return the optional text declaration and the first char and state. *)
   (*--------------------------------------------------------------------*)
   (* might raise: NoSuchFile                                            *) 
   (*--------------------------------------------------------------------*)
   fun openSubset uri a = 
      let val (q,auto) = pushSpecial (EXT_SUBSET,SOME uri)
      in findTextDecl (parseTextDecl,false) auto (a,q)
      end
   handle NoSuchFile fmsg => raise CantOpenFile(fmsg,a)

   (*--------------------------------------------------------------------*)
   (* open the document entity; consume its xml declaration if present.  *)
   (* See 2.8:                                                           *)
   (*                                                                    *)
   (*    [1] document ::= prolog element Misc*                           *)
   (*   [22]   prolog ::= XMLDecl? Misc* (doctypedecl Misc* )?           *)
   (*                                                                    *)
   (* return the optional xml declaration and the first char and state.  *)
   (*--------------------------------------------------------------------*)
   (* might raise: NoSuchFile                                            *) 
   (*--------------------------------------------------------------------*)
   fun openDocument uri a = 
      let val (q,auto) = pushSpecial (DOC_ENTITY,uri)
      in findTextDecl (parseXmlDecl,!O_WARN_XML_DECL) auto (a,q) 
      end
   handle NoSuchFile fmsg => raise CantOpenFile(fmsg,a)
end

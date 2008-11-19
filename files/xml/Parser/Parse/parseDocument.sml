(*--------------------------------------------------------------------------*)
(* Structure: ParseDocument                                                 *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   parseDocTypeDecl : none                                                *)
(*--------------------------------------------------------------------------*)
functor Parse 
   (structure Dtd           : Dtd
    structure Hooks         : Hooks 
    structure Resolve       : Resolve
    structure ParserOptions : ParserOptions) : 
       sig
	  val parseDocument : Uri.Uri option -> Dtd.Dtd option -> Hooks.AppData -> Hooks.AppFinal 
       end
      = 
struct
   structure ParseBase = ParseBase (structure Dtd = Dtd
				    structure Hooks = Hooks
				    structure Resolve = Resolve
				    structure ParserOptions = ParserOptions) 
      
   structure ParseContent = ParseContent (structure ParseBase = ParseBase)

   open
      Base UniChar Errors UniClasses Uri
      ParseContent

   val THIS_MODULE = "ParseContent"

   datatype Where = 
      PROLOG
    | EPILOG
    | INSTANCE of int option
	 
   fun locOf wher =
      case wher 
	of PROLOG => LOC_PROLOG
	 | INSTANCE _ => LOC_PROLOG
	 | EPILOG => LOC_EPILOG

   fun checkRoot dtd (a,q) (doc,stag as ((_,elem,_,_,_),_)) =
      if !O_VALIDATE 
	 then case doc
		of NONE => a
		 | SOME doc => 
		   if doc=elem then a
		   else let val err = ERR_ROOT_ELEM(Index2Element dtd doc,
						    Index2Element dtd elem)
			in hookError(a,(getPos q,err))
			end
      else a

   fun parseDoc dtd caq =
      let 
	 fun do_data wher caq =
	    let fun doit hadError ws (c,a,q) =
	       case c
		 of 0wx00            => (ws,(c,a,q))
		  | 0wx26 (* #"&" *) => (ws,(c,a,q))
		  | 0wx3C (* #"<" *) => (ws,(c,a,q))
		  | 0wx09 (* #"\t"*) => doit hadError (c::ws) (getChar(a,q))
		  | 0wx0A (* #"\n"*) => doit hadError (c::ws) (getChar(a,q))
		  | 0wx20 (* #" " *) => doit hadError (c::ws) (getChar(a,q))
		  | _ => let val a1 = if hadError then a
                                      else hookError(a,(getPos q,ERR_FORBIDDEN_HERE
                                                        (IT_DATA nil,locOf wher)))
                         in doit true ws (getChar(a1,q))
                         end

		val (ws,(c1,a1,q1)) = doit false nil caq
		val a2 = if null ws then a1
			 else hookWhite(a1,Data2Vector (rev ws))
	    in (c1,a2,q1)
	    end
			    
	 fun do_decl wher q0 (c,a,q) =
	    case c
	      of 0wx2D (* #"-" *) => 
		 let val (c1,a1,q1) = getChar(a,q)
		 in if c1=0wx2D then (wher,parseComment (getPos q0) (a1,q1))
		    else let val err = ERR_EXPECTED(expDash,[c1])
			     val a2 = hookError(a1,(getPos q1,err))
			     val caq2 = recoverDecl false (c1,a2,q1)
			 in (wher,caq2)
			 end
		 end
	       | 0wx5B (* #"[" *) => 
		 let 
		    val err = ERR_FORBIDDEN_HERE (IT_CDATA,locOf wher)
		    val a1 = hookError(a,(getPos q0,err))
		    val caq2 = skipBadSection (getChar(a1,q))
		 in (wher,caq2)
		 end
	       | _ => 
		 case wher 
		   of PROLOG => 
		      (let val (name,(c1,a1,q1)) = parseName (c,a,q)
			  handle NotFound (c,a,q) => 
			     let val err = expectedOrEnded(expDashDocLbrk,LOC_DECL) c
			     in raise SyntaxError (c,hookError(a,(getPos q,err)),q)
			     end
			     
			   val _ = if name=[0wx44,0wx4f,0wx43,0wx54,0wx59,0wx50,0wx45] 
				   (* "DOCTYPE" *) then ()
				   else let val err = ERR_EXPECTED(expDashDocLbrk,name)
					    val a2 = hookError(a1,(getPos q,err))
					in raise SyntaxError (c1,a2,q1)
					end
					
			   val (doc,caq2) = parseDocTypeDecl dtd (c1,a1,q1)
		       in (INSTANCE doc,caq2)
		       end
			  handle SyntaxError caq => (PROLOG,recoverDecl true caq))
			  
		    | _ => let val loc = if wher=EPILOG then LOC_EPILOG else LOC_AFTER_DTD
			       val err = ERR_FORBIDDEN_HERE (IT_DECL,loc)
			       val a1 = hookError(a,(getPos q0,err))
			       val caq2 = skipDecl true (c,a1,q)
			   in (wher,caq2)
			   end
			   
	 and doit wher (c,a,q) = 
	    case c 
	      of 0wx00 => if isSpecial q then (wher,(a,q))
			  else doit wher (getChar(a,q))
	       (*--------------------------------------------------------------*)
	       (* References are forbidden outside the document element        *) 
	       (*--------------------------------------------------------------*)
	       | 0wx26 (* #"&" *) => 
	         let 
		    val (c1,a1,q1) = getChar(a,q)
		    val caq2 = 
		       if c1=0wx23 (* #"#" *)
			  then let val err = ERR_FORBIDDEN_HERE(IT_CHAR_REF,locOf wher)
				   val a2 = hookError(a1,(getPos q,err))
			       in skipCharRef (a2,q1)
			       end
		       else let val err = ERR_FORBIDDEN_HERE(IT_REF,locOf wher)
				val a2 = hookError(a1,(getPos q,err))
			    in skipReference (c1,a2,q1)
			    end
		 in doit wher caq2
		 end
	       | 0wx3C (* #"<" *) => 
		 let val (c1,a1,q1) = getChar (a,q)
		 in case c1
		      of 0wx21 (* #"!" *) => 
			 let val (wher1,caq2) = do_decl wher q (getChar(a1,q1))
			 in doit wher1 caq2
			 end
		       | 0wx2F (* #"/" *) => 
			 let 
			    val err = ERR_FORBIDDEN_HERE(IT_ETAG,locOf wher)
			    val a2 = hookError(a1,(getPos q,err))
			    val caq3 = skipTag LOC_ETAG (a2,q1)
			 in doit wher caq3
			 end
		       | 0wx3F (* #"?" *) => doit wher (parseProcInstr (getPos q) (a1,q1))
		       | _ => 
			 if isName c1 then 
			    let val wher1 = 
			       case wher
				 of PROLOG => INSTANCE NONE
				  | _ => wher
			    in case wher1 
				 of PROLOG => 
				    raise InternalError(THIS_MODULE,"parseDoc.doit","")
				  | EPILOG => 
				    let 
				       val err = ERR_FORBIDDEN_HERE(IT_STAG,LOC_EPILOG)
				       val a2 = hookError(a1,(getPos q,err))
				       val caq3 = skipTag LOC_STAG (a2,q1)
				    in doit EPILOG caq3
				    end
				  | INSTANCE doc => 
				    (let 
					val a2 = 
					   if not (!O_VALIDATE) orelse isSome doc then a1
					   else hookError(a1,(getPos q,ERR_NO_DTD))
					val (stag,(c3,a3,q3)) = parseSTag 
					   dtd (getPos q) (c1,a2,q1)
					val a4 = checkRoot dtd (a3,q1) (doc,stag)
					val (opt,(c5,a5,q5)) = parseElement 
					   (dtd,nil,q,stag,(c3,a4,q3))
					val a6 = checkDefinedIds dtd (a5,q5)
				     in case opt
					  of NONE => doit EPILOG (c5,a6,q5)
					   | SOME (_,_,startPos,_) => 
					     let 
						val err = ERR_FORBIDDEN_HERE(IT_ETAG,LOC_EPILOG)
						val a7 = hookError(a6,(startPos,err))
					     in doit EPILOG (c5,a7,q5)
					     end
				     end
					handle SyntaxError caq => doit wher1 caq)
			    end
			 else let val err = ERR_FORBIDDEN_HERE(IT_CHAR 0wx3C,locOf wher)
				  val a2 = hookError(a1,(getPos q,err))
			      in doit wher (c1,a2,q1)
					  end
		 end
	       | _ => let val caq1 = do_data wher (c,a,q)
		      in doit wher caq1
		      end
      in 
	 doit PROLOG caq
      end	       

   (* to false. (cf. 2.9)                                                *)
   (*                                                                    *) 
   (*   ... If ... there is no standalone document declaration, the      *)
   (*   value "no" is assumed.                                           *)
   fun parseDocument uriOpt dtdOpt a = 
      let 
	 val dtd = case dtdOpt 
		     of NONE => initDtdTables () 
		      | SOME dtd => dtd 
	 val (enc,xmlDecl,(c1,a1,q1)) = openDocument uriOpt a
	 val uri = getUri q1
	 val alone = case xmlDecl 
		       of (SOME(_,_,SOME sa)) => sa
			| _ => false
	 val _ = if alone then setStandAlone dtd true else ()
	 val a2 = hookXml(a1,(uri,enc,xmlDecl))
	 val (wher,(a3,q3)) = 
	   let
	     val version = case xmlDecl 
	       of (SOME(SOME ver,_,_)) => ver
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
	     val r = parseDoc dtd (c1,a2,q1)
	     val _ = getCharRef := getCharOld
	     val _ = isNmsRef := isNmsOld
	     val _ = isNameRef := isNameOld
	     val _ = isXmlRef := isXmlOld
	   in
	     r
	   end
	 val _ = closeAll q3
	 val a4 = case wher 
		    of EPILOG => a3
		     | _ => hookError(a3,(getPos q3,ERR_ENDED_IN_PROLOG))
      in hookFinish a4
      end
   handle CantOpenFile(fmsg,a) => 
      let val a1 = hookError(a,(nullPosition,ERR_NO_SUCH_FILE fmsg))
      in hookFinish a1
      end
end

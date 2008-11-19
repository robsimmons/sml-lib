(*--------------------------------------------------------------------------*)
(* Structure: DtdAttributes                                                 *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   checkAttValue       : AttValue InternalError                           *)
(*   checkDefinedIds     : none                                             *)
(*   genMissingAtts      : none                                             *)
(*   makeAttValue        : AttValue InternalError                           *) 
(*--------------------------------------------------------------------------*)
functor DtdAttributes (structure Dtd           : Dtd
		       structure Entities      : Entities
		       structure ParserOptions : ParserOptions) = 
   struct
      structure DtdDeclare = DtdDeclare (structure Dtd = Dtd
					 structure Entities = Entities
					 structure ParserOptions = ParserOptions)
      open 
	 UniChar UniClasses UtilList 
	 Base Dtd DtdDeclare Errors Entities HookData ParserOptions 

      val THIS_MODULE = "DtdAttributes"
      
      exception AttValue of AppData

      (*--------------------------------------------------------------------*)
      (* this is the list of language codes in ISO 639.                     *)
      (*--------------------------------------------------------------------*)
      val iso639codes = 
	 Vector.fromList 
	 ["AA","AB","AF","AM","AR","AS","AY","AZ",
	  "BA","BE","BG","BH","BI","BN","BO","BR",
	  "CA","CO","CS","CY",
	  "DA","DE","DZ",
	  "EL","EN","EO","ES","ET","EU",
	  "FA","FI","FJ","FO","FR","FY",
	  "GA","GD","GL","GN","GU",
	  "HA","HE","HI","HR","HU","HY",
	  "IA","ID","IE","IK","IN","IS","IT","IU","IW",
	  "JA","JI","JW",
	  "KA","KK","KL","KM","KN","KO","KS","KU","KY",
	  "LA","LN","LO","LT","LV",
	  "MG","MI","MK","ML","MN","MO","MR","MS","MT","MY",
	  "NA","NE","NL","NO",
	  "OC","OM","OR",
	  "PA","PL","PS","PT",
	  "QU",
	  "RM","RN","RO","RU","RW",
	  "SA","SD","SG","SH","SI","SK","SL","SM","SN","SO","SQ","SR","SS","ST","SU","SV","SW",
	  "TA","TE","TG","TH","TI","TK","TL","TN","TO","TR","TS","TT","TW",
	  "UG","UK","UR","UZ",
	  "VI","VO",
	  "WO",
	  "XH",
	  "YI","YO",
	  "ZA","ZH","ZU"]

      (*--------------------------------------------------------------------*)
      (* a two-dimensional field [0..25][0..25] of booleans for ISO 639.    *)
      (*--------------------------------------------------------------------*)
      val iso639field = 
	 let 
	    val arr = Array.tabulate(26,fn _ => Array.array(26,false))
	    val _ = Vector.map 
	       (fn s => Array.update(Array.sub(arr,ord(String.sub(s,0))-65),
				     ord(String.sub(s,1))-65,
				     true)) 
	       iso639codes
	 in Vector.tabulate(26,fn i => Array.vector (Array.sub(arr,i)))
	 end

      (*--------------------------------------------------------------------*)
      (* for a letter, compute ord(toUpper c)-ord(#"A"), for subscripting.  *)
      (*--------------------------------------------------------------------*)
      val toUpperMask = Chars.notb(0wx20)
      fun cIndex c = Chars.toInt(Chars.andb(c,toUpperMask)-0wx41)

      (*--------------------------------------------------------------------*)
      (* are these two letters an ISO 639 code?                             *)
      (*--------------------------------------------------------------------*)
      fun isIso639 (c1,c2) =
	 if !O_CHECK_ISO639 then 
	    Vector.sub(Vector.sub(iso639field,cIndex c1),cIndex c2)
	    handle Subscript => false
	 else isAsciiLetter c1 andalso isAsciiLetter c2
	
      (*--------------------------------------------------------------------*)
      (* does this match Subcode ('-' Subcode)* ?                           *)
      (* is this a sequence of ('-' Subcode) ?                              *)
      (* Iana codes and user codes also end on ([a-z] | [A-Z])+             *)
      (*--------------------------------------------------------------------*)
      fun isSubcode' nil = false
	| isSubcode' (c::cs) = 
	 let fun doit nil = true
	       | doit (c::cs) = if c=0wx2D then isSubcode' cs
				else isAsciiLetter c andalso doit cs
	 in isAsciiLetter c andalso doit cs
	 end
      fun isSubcode nil = true
	| isSubcode (c::cs) = c=0wx2D andalso isSubcode' cs
      val isIanaUser = isSubcode'
	 
      (*--------------------------------------------------------------------*)
      (* Check whether a "xml:lang" attribute matches the LanguageID        *)
      (* production. 2.12:                                                  *)
      (*                                                                    *)
      (*   [33] LanguageID ::= Langcode ('-' Subcode)*                      *)
      (*   [34]   Langcode ::= ISO639Code |  IanaCode |  UserCode           *)
      (*   [35] ISO639Code ::= ([a-z] | [A-Z]) ([a-z] | [A-Z])              *)
      (*   [36]   IanaCode ::= ('i' | 'I') '-' ([a-z] | [A-Z])+             *)
      (*   [37]   UserCode ::= ('x' | 'X') '-' ([a-z] | [A-Z])+             *)
      (*   [38]    Subcode ::= ([a-z] | [A-Z])+                             *)
      (*                                                                    *)
      (* print an error and raise AttValue if the "xml:lang" attribute does *)
      (* not have a valid value.                                            *)
      (*--------------------------------------------------------------------*)
      fun checkAttSpec (a,q) (aidx,cs) = 
	 if !O_CHECK_LANGID andalso aidx=xmlLangIdx
	    then let val valid = case cs
				   of c::0wx2D::cs' => (c=0wx49 orelse
							c=0wx69 orelse
							c=0wx58 orelse 
							c=0wx78) andalso isIanaUser cs'
				    | c1::c2::cs' => isIso639 (c1,c2) andalso isSubcode cs'
				    | _ => false 
		 in 
		    if valid then a 
		    else raise AttValue(hookError(a,(getPos q,ERR_ATT_IS_NOT(cs,IT_LANG_ID))))
		 end
	 else a

      (*--------------------------------------------------------------------*)
      (* Normalize an attribute value of type other than CDATA, and split   *)
      (* it into tokens at space characters. Cf. 3.3.3:                     *)
      (*                                                                    *)
      (*   ... If the declared value is not CDATA, then the XML processor   *)
      (*   must further process the normalized attribute value by dis-      *)
      (*   carding any leading and trailing space (#x20) characters, and by *)
      (*   replacing sequences of space (#x20) characters by a single space *)
      (*   (#x20) character.                                                *)
      (*                                                                    *)
      (* replacement of references is already done when parsing the literal,*)
      (* thus we need only do whitespace normalization. we don't need to    *)
      (* take care of the 3rd rule since replacement of sequences of #x20   *)
      (* and then splitting subsumes its effect.                            *)
      (*                                                                    *)
      (* return the list of tokens as character lists and the normalized    *)
      (* value as a char vector.                                            *)
      (*--------------------------------------------------------------------*)
      fun splitAttValue av =
	 let 
	    fun doOne nil = (nil,nil,nil)
	      | doOne (c::cs) = if c=0wx20 then let val (toks,ys) = doAll true cs
						in (nil,toks,ys)
						end
				else let val (tok,toks,ys) = doOne cs
				     in ((c::tok),toks,c::ys)
				     end
	    and doAll addS nil = (nil,nil)
	      | doAll addS (c::cs) = if c=0wx20 then doAll addS cs
				     else let val (tok,toks,ys) = doOne cs
					  in ((c::tok)::toks,
					      if addS then 0wx20::c::ys else c::ys)
					  end
		    
	    val (tokens,normed) = doAll false av
	 in (Data2Vector normed,tokens)
	 end
      (*--------------------------------------------------------------------*)
      (* normalize an attribute value other than CDATA according to 3.3.3.  *)
      (*                                                                    *)
      (* return the normalized att value as a Vector.                       *)
      (*--------------------------------------------------------------------*)
      fun normAttValue av =
	 let fun doOne nil = nil
	       | doOne (c::cs) = if c=0wx20 then doAll true cs
				 else c::doOne cs
	     and doAll addS nil = nil
	       | doAll addS (c::cs) = if c=0wx20 then doAll addS cs
				      else let val ys = doOne cs
					   in if addS then 0wx20::c::ys else c::ys
					   end
	     val normed = doAll false av
	 in Data2Vector normed
	 end

      (*--------------------------------------------------------------------*)
      (* Check whether a sequence of chars forms a name (token).            *) 
      (*--------------------------------------------------------------------*)
      fun isNmToken cs = List.all isName cs
      fun isaName nil = false
	| isaName (c::cs) = isNms c andalso List.all isName cs

      (*--------------------------------------------------------------------*)
      (* Check whether a list of tokens is a single what fulfilling isWhat. *)
      (* print an error and raise AttValue if it is not.                    *)
      (*--------------------------------------------------------------------*)
      fun checkOne (isWhat,what,detail) (a,q) toks = 
	 case toks 
	   of nil => raise AttValue (hookError(a,(getPos q,ERR_EXACTLY_ONE detail)))
	    | [one] => if isWhat one then one 
		       else raise AttValue(hookError(a,(getPos q,ERR_ATT_IS_NOT(one,what))))
	    | more => raise AttValue(hookError(a,(getPos q,ERR_AT_MOST_ONE detail)))
      (*--------------------------------------------------------------------*)
      (* Check whether a list of tokens is non-empty and all elements ful-  *)
      (* fil isWhat.                                                        *)
      (* print an error and raise AttValue if not.                          *)
      (*--------------------------------------------------------------------*)
      fun checkList (isWhat,what,detail) (a,q) toks = 
	 case toks 
	   of nil => raise AttValue (hookError(a,(getPos q,ERR_AT_LEAST_ONE detail)))
	    | _ => app (fn one => if isWhat one then () 
				  else let val err = ERR_ATT_IS_NOT(one,what)
				       in raise AttValue(hookError(a,(getPos q,err)))
				       end) toks
      (*--------------------------------------------------------------------*)
      (* Convert a list of tokens into an ID att value. 3.3.1:              *)
      (*                                                                    *)
      (*   Validity Constraint: ID                                          *)
      (*   Values of type ID must match the Name production.                *)
      (*                                                                    *)
      (*   Validity Constraint: ID                                          *)
      (*   ... A name must not appear more than once in an XML document as  *)
      (*   a value of this type; i.e., ID values must uniquely identify the *)
      (*   elements which bear them.                                        *)
      (*                                                                    *)
      (* mark the value as used, print an error and raise AttValue if it    *)
      (* was already used.                                                  *)
      (* print an error and raise AttValue if it is not a name.             *)
      (*--------------------------------------------------------------------*)
      fun takeId (dtd,inDtd) (a,q) toks = 
	 let val one = checkOne (isaName,IT_NAME,IT_ID_NAME) (a,q) toks 
	     val idx = Id2Index dtd one
	     val _ = if inDtd then ()
		     else let val (decl,refs) = getId dtd idx
			  in if decl then let val err = ERR_REPEATED_ID one
					  in raise AttValue (hookError(a,(getPos q,err)))
					  end
			     else setId dtd (idx,(true,refs))
			  end
	 in (SOME(AV_ID idx),a)
	 end
	    
      (*--------------------------------------------------------------------*)
      (* Convert a list of tokens into an IDREF/IDREFS att value. 3.3.1:    *)
      (*                                                                    *)
      (*   Validity Constraint: IDREF                                       *)
      (*   Values of type IDREF must match the Name production.             *)
      (*                                                                    *)
      (* print an error an raise AttValue if it is not a (list of) name(s). *)
      (*--------------------------------------------------------------------*)
      fun setIdRef (dtd,q) idx =
	 let val (decl,refs) = getId dtd idx
	 in setId dtd (idx,(decl,getPos q::refs))
	 end
      fun takeIdref (dtd,_) (a,q) toks = 
	 let val one = checkOne (isaName,IT_NAME,IT_ID_NAME) (a,q) toks 
	     val idx=Id2Index dtd one
	     val _ = setIdRef (dtd,q) idx
	 in (SOME(AV_IDREF idx),a)
	 end
      fun takeIdrefs (dtd,_) (a,q) toks = 
	 let val _ = checkList (isaName,IT_NAME,IT_ID_NAME) (a,q) toks 
	     val idxs = map (Id2Index dtd) toks
	     val _ = app (setIdRef (dtd,q)) idxs
	 in (SOME(AV_IDREFS idxs),a)
	 end

      (*--------------------------------------------------------------------*)
      (* Convert a list of tokens into an ENTITY/IES att value. 3.3.1:      *)
      (*                                                                    *)
      (*   Validity Constraint: Entity Name                                 *)
      (*   Values of type ENTITY must match the Name production...          *)
      (*   must match the name of an unparsed entity declared in the DTD.   *)
      (*                                                                    *)
      (* print an error and raise AttValue if a token is not a name.        *)
      (* print an error and raise AttValue if an entity is undeclared or a  *)
      (* parsed entity.                                                     *)
      (*--------------------------------------------------------------------*)
      fun checkEntity (dtd,inDtd) (a,q) name =
	 let val idx = GenEnt2Index dtd name
	     val (ent,_) = getGenEnt dtd idx
	     val _ = if inDtd then ()
		     else case ent
			    of GE_UNPARSED _ => ()
			     | GE_NULL => let val err = ERR_UNDECLARED(IT_GEN_ENT,name,LOC_NONE)
					  in raise AttValue (hookError(a,(getPos q,err)))
					  end
			     | _ => let val err = ERR_MUST_BE_UNPARSED(name,LOC_NONE)
				    in raise AttValue (hookError(a,(getPos q,err)))
				    end
	 in idx
	 end
      fun takeEntity (dtd,inDtd) (aq as (a,_)) toks = 
	 let val one = checkOne (isaName,IT_NAME,IT_ENT_NAME) aq toks 
	     val idx = checkEntity (dtd,inDtd) aq one
	 in (SOME(AV_ENTITY idx),a)
	 end
      fun takeEntities (dtd,inDtd) (aq as (a,_)) toks = 
	 let val _ = checkList (isaName,IT_NAME,IT_ENT_NAME) aq toks 
	     val idxs = map (checkEntity (dtd,inDtd) aq) toks
	 in (SOME(AV_ENTITIES idxs),a)
	 end

      (*--------------------------------------------------------------------*)
      (* Convert a list of tokens into a NOTATION att value. 3.3.1:         *)
      (*                                                                    *)
      (*   Validity Constraint: Notation Attributes                         *)
      (*   Values of this type must match one of the notation names         *)
      (*   included in the declaration.                                     *)
      (*                                                                    *)
      (* print an error and raise AttValue if it is not a single name.      *)
      (* print an error and raise AttValue if the notation's index  is not  *)
      (* in the list given as 1st arg.                                      *)
      (*--------------------------------------------------------------------*)
      fun takeNotation is (dtd,inDtd) (aq as (a,q)) toks = 
	 let val one = checkOne (isaName,IT_NAME,IT_NOT_NAME) aq toks 
	     val idx = AttNot2Index dtd one
	     val _ = if member idx is then ()
		     else let val nots = map (Index2AttNot dtd) is
			      val err = ERR_MUST_BE_AMONG(IT_NOT_NAME,one,nots)
			  in raise AttValue (hookError(a,(getPos q,err)))
			  end 
	 in (SOME(AV_NOTATION(is,idx)),a)
	 end
	    
      (*--------------------------------------------------------------------*)
      (* Convert a list of tokens into an enumerated att value. 3.3.1:      *)
      (*                                                                    *)
      (*   Validity Constraint: Enumeration                                 *)
      (*   Values of this type must match one of the Nmtoken tokens in      *)
      (*   the declaration.                                                 *)
      (*                                                                    *)
      (* print an error and raise AttValue if it is not a single name token.*)
      (* print an error and raise AttValue if the token's index is not      *)
      (* in the list given as 1st arg.                                      *)
      (*--------------------------------------------------------------------*)
      fun takeGroup is (dtd,_) (aq as (a,q)) toks = 
	 let val one = checkOne (isNmToken,IT_NMTOKEN,IT_NMTOKEN) aq toks 
	     val idx = AttNot2Index dtd one
	     val _ = if member idx is then ()
		     else let val toks = map (Index2AttNot dtd) is
			      val err = ERR_MUST_BE_AMONG(IT_NMTOKEN,one,toks)
			  in raise AttValue (hookError(a,(getPos q,err)))
			  end
	 in (SOME(AV_GROUP(is,idx)),a)
	 end
	       
      (*--------------------------------------------------------------------*)
      (* Given an attribute type and a list of characters, construct the    *)
      (* corresponding AttValue.                                            *)
      (*                                                                    *)
      (* print an error (and possibly raise AttValue) if the attribute      *)
      (* is ill-formed.                                                     *)
      (*--------------------------------------------------------------------*)
      fun makeAttValue dtd (a,q) (aidx,attType,ext,inDtd,cs) =
	 if attType=AT_CDATA 
	    then let val cv = Data2Vector cs
		 in if !O_VALIDATE andalso hasDtd dtd
		       then (cv,(SOME(AV_CDATA cv),checkAttSpec (a,q) (aidx,cs)))
		    else (cv,(NONE,a))
		 end
	 else 
	    if !O_VALIDATE andalso hasDtd dtd then 
	       let 
		  val a1 = checkAttSpec (a,q) (aidx,cs)
		  val (cv,toks) = splitAttValue cs
		  val a2 = 
		     if ext andalso standsAlone dtd
			then let val cdata = Data2Vector cs
			     in if cdata=cv then a1
				else let val err = ERR_STANDALONE_NORM(Index2AttNot dtd aidx)
					   val _ = setStandAlone dtd (not (!O_ERROR_MINIMIZE))
				     in hookError(a1,(getPos q,err))
				     end
			     end
		     else a1
	       in case attType 
		    of AT_NMTOKEN => (cv,(SOME(AV_NMTOKEN(checkOne(isNmToken,IT_NMTOKEN,
								   IT_NMTOKEN) (a2,q) toks)),a2))
		     | AT_NMTOKENS => (cv,(SOME(AV_NMTOKENS toks),a2)) before
		       checkList(isNmToken,IT_NMTOKEN,IT_NMTOKEN) (a2,q) toks
		     | AT_ID => (cv,takeId (dtd,inDtd) (a2,q) toks)
		     | AT_IDREF => (cv,takeIdref (dtd,inDtd) (a2,q) toks)
		     | AT_IDREFS => (cv,takeIdrefs (dtd,inDtd) (a2,q) toks)
		     | AT_ENTITY => (cv,takeEntity (dtd,inDtd) (a2,q) toks)
		     | AT_ENTITIES => (cv,takeEntities (dtd,inDtd) (a2,q) toks)
		     | AT_GROUP is => (cv,takeGroup is (dtd,inDtd) (a2,q) toks)
		     | AT_NOTATION is => (cv,takeNotation is (dtd,inDtd) (a2,q) toks)
		     | AT_CDATA => raise InternalError(THIS_MODULE,"makeAttValue",
						       "AT_CDATA in the innermost case")
	       end
	    else (normAttValue cs,(NONE,a))
	       
      (*--------------------------------------------------------------------*)
      (* given an attribute value literal and the attribute type, generate  *)
      (* the AttValue, and check whether it complies with its default value.*)
      (* If yes, make an AttPresent value out of it.                        *)
      (* See 3.3.2:                                                         *)
      (*                                                                    *)
      (*   Validity Constraint: Fixed Attribute Default                     *)
      (*   If an attribute has a default value declared with the #FIXED     *)
      (*   keyword, instances of that attribute must match the default      *)
      (*   value.                                                           *)
      (*                                                                    *)
      (* print an error and raise AttValue if the attribute value doesn't   *)
      (* comply.                                                            *)
      (*                                                                    *)
      (* return the value as a AttPresent value.                            *)
      (*--------------------------------------------------------------------*)
      fun checkAttValue dtd (a,q) ((aidx,attType,defVal,ext),literal,cs) = 
	 let val (cv,(av,a1)) = makeAttValue dtd (a,q) (aidx,attType,ext,false,cs)
	 in if !O_VALIDATE andalso hasDtd dtd then 
	    case defVal 
	      of AD_FIXED((def,cv',_),_) => 
		 if cv=cv' then (AP_PRESENT(literal,cv,av),a1)
		 else raise AttValue
		    (hookError(a1,(getPos q,ERR_FIXED_VALUE(Index2AttNot dtd aidx,cv,cv'))))
	       | _ => (AP_PRESENT(literal,cv,av),a1)
	    else (AP_PRESENT(literal,cv,av),a1)
	 end

      (*--------------------------------------------------------------------*)
      (* check a defaulted attribute value for validity.                    *)
      (*                                                                    *)
      (* since the lexical constraints are checked when the default is      *)
      (* declared we only need to check whether notations are declared and  *)
      (* entities are declared and unparsed. An ID attribute cannot be      *) 
      (* defaulted, so no need to check for duplicate ID attributes.        *)
      (*--------------------------------------------------------------------*)
      fun checkDefaultValue dtd (a,q,pos) av =
	 let 
	    fun checkEntity (idx,a) = 
	       let val (ent,_) = getGenEnt dtd idx
	       in case ent
		    of GE_UNPARSED _ => a
		     | GE_NULL => hookError(a,(getPos q,ERR_UNDECLARED
					       (IT_GEN_ENT,Index2GenEnt dtd idx,
						LOC_ATT_DEFAULT pos)))
		     | _ => hookError(a,(getPos q,ERR_MUST_BE_UNPARSED 
					 (Index2GenEnt dtd idx,LOC_ATT_DEFAULT pos)))
	       end
	    
	    fun checkNotation (idx,a) =
	       if hasNotation dtd idx then a
	       else hookError(a,(getPos q,ERR_UNDECLARED
				 (IT_NOTATION,Index2AttNot dtd idx,LOC_ATT_DEFAULT pos)))
	 in 
	    case av
	      of SOME(AV_ENTITY i) => checkEntity (i,a)
	       | SOME(AV_ENTITIES is) => foldl checkEntity a is
	       | SOME(AV_NOTATION(_,i)) => checkNotation(i,a)
	       | _ => a
	 end

      (*--------------------------------------------------------------------*)
      (* Generate the attributes not specified in a start-tag, the defs of  *)
      (* these atts and the specified atts given as argument. 3.3.2:        *)
      (*                                                                    *)
      (*   If the declaration is neither #REQUIRED nor #IMPLIED, then the   *)
      (*   AttValue value contains the declared default value; ... If a     *)
      (*   default value is declared, when an XML processor encounters an   *)
      (*   omitted attribute, it is to behave as though the attribute were  *)
      (*   present with the declared default value.                         *)
      (*                                                                    *)
      (*   Validity Constraint: Required Attribute                          *)
      (*   If the default declaration is the keyword #REQUIRED, then the    *)
      (*   attribute must be specified for all elements of the type in the  *)
      (*   attribute-list declaration.                                      *)
      (*                                                                    *)
      (* print an error if a required attribute was omitted.                *)
      (*                                                                    *)
      (* return the AttSpecList of all attributes for this tag.             *)
      (*--------------------------------------------------------------------*)
      fun genMissingAtts dtd (a,q) (defs,specd) = 
	 let 
	    fun default a (idx,(v as (_,_,av),(pos,checked)),ext) = 
	       let val a1 = if ext andalso !O_VALIDATE andalso standsAlone dtd
			       then let val err = ERR_STANDALONE_DEF(Index2AttNot dtd idx)
					val _ = setStandAlone dtd (not (!O_ERROR_MINIMIZE))
				    in hookError(a,(getPos q,err))
				    end
			    else a
		   val a2 = if !O_VALIDATE andalso not (!checked andalso !O_ERROR_MINIMIZE)
			       then checkDefaultValue dtd (a1,q,pos) av before checked := true
			    else a1 
	       in (AP_DEFAULT v,a1)
	       end
	    fun doit a nil = (specd,a)
	      | doit a ((idx,_,dv,ext)::rest) = 
	       let val (value,a1) = 
		  case dv 
		    of AD_DEFAULT v => default a (idx,v,ext)
		     | AD_FIXED v => default a (idx,v,ext)
		     | AD_IMPLIED => (AP_IMPLIED,a)
		     | AD_REQUIRED => 
		       let val a1 = if not (!O_VALIDATE) then a
				    else hookError(a,(getPos q,
						      ERR_MISSING_ATT(Index2AttNot dtd idx)))
		       in (AP_MISSING,a1)
		       end
		   val (other,a2) = doit a1 rest
	       in ((idx,value,NONE)::other,a2)
	       end
	 in doit a defs
	 end

      (*--------------------------------------------------------------------*)
      (* process an undeclared attribute in a start-tag.                    *)
      (* At option, an error message is generated only once for the same    *)
      (* attribute and element.                                             *)
      (*                                                                    *)
      (* possibly print an error.                                           *)
      (*                                                                    *)
      (* return nothing.                                                    *)
      (*--------------------------------------------------------------------*)
      fun handleUndeclAtt dtd (a,q) (aidx,att,eidx,elem) =
	 if !O_ERROR_MINIMIZE then 
	    let val {decl,atts,errAtts} = getElement dtd eidx
	    in if member aidx errAtts then a
	       else let val a1 = if !O_VALIDATE andalso hasDtd dtd
				    then let val err = ERR_UNDECL_ATT(att,elem)
					 in hookError(a,(getPos q,err))
					 end
				 else a
			val a2 = checkAttName (a1,q) att
			val _ = setElement dtd (eidx,{decl    = decl,
						      atts    = atts,
						      errAtts = aidx::errAtts})
		    in a2
		    end
	    end
	 else let val a1 = if !O_VALIDATE andalso hasDtd dtd
			      then hookError(a,(getPos q,ERR_UNDECL_ATT(att,elem)))
			   else a
	      in checkAttName (a1,q) att
	      end
	   
   end

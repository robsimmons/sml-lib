(*--------------------------------------------------------------------------*)
(* Structure: DtdDeclare                                                    *)
(*                                                                          *)
(*--------------------------------------------------------------------------*)
(* Functor: DtdDeclare                                                      *)
(*--------------------------------------------------------------------------*)
(* This module provides functions for adding declarations to the DTD tables *)
(* and for doing checks on components of declarations.                      *)
(*--------------------------------------------------------------------------*)
functor DtdDeclare (structure Dtd           : Dtd
		    structure Entities      : Entities
		    structure ParserOptions : ParserOptions) = 
   struct
      open
	 UtilInt UtilList
	 Base Dtd Errors Entities ParserOptions UniChar UniClasses
	 
      (*--------------------------------------------------------------------*)
      (* check whether a sequence a chars is the b-adic representation of a *)
      (* character's code, terminated by ";". base will be 10 or 16, isBase *)
      (* will check for a character being a decimal/hexadecimal number.     *)
      (*--------------------------------------------------------------------*)
      fun checkBasimal (base,baseValue) (ch:Char,cs) = 
	 let fun doit _ (nil:Data) = false
	       | doit yet [0wx3B] = yet=ch
	       | doit yet (c::cs) = case baseValue c
				      of NONE => false
				       | SOME v => doit (base*yet+v) cs
	 in doit 0w0 cs
	 end
      val checkDecimal = checkBasimal (0w10,decValue)
      val checkHeximal = checkBasimal (0wx10,hexValue)

      (*--------------------------------------------------------------------*)
      (* check a character reference for identifying a character.           *)
      (*--------------------------------------------------------------------*)
      fun checkRef (ch,0wx26::0wx23::0wx78::cs) (* "&#x..." *) = checkHeximal(ch,cs)
	| checkRef (ch,0wx26::0wx23::cs) (* "&#..." *) = checkDecimal(ch,cs)
	| checkRef _ = false

      (*--------------------------------------------------------------------*)
      (* check for a single character ch.                                   *)
      (*--------------------------------------------------------------------*)
      fun checkSingle (ch,[c]) = c=ch
	| checkSingle _ = false 

      (*--------------------------------------------------------------------*)
      (* check a predefined entity for being well defined. Note that both   *)
      (* a single char and a char ref representation are allowed, except    *)
      (* for 'amp' which must be escaped.                                   *)
      (*--------------------------------------------------------------------*)
      fun checkPredef (idx,cs) =
	 case idx
	   of 1 => checkRef(0wx26,cs) 
	    | 2 => checkSingle(0wx3C,cs) orelse checkRef(0wx3C,cs) 
	    | 3 => checkSingle(0wx3E,cs) orelse checkRef(0wx3E,cs) 
	    | 4 => checkSingle(0wx27,cs) orelse checkRef(0wx27,cs) 
	    | 5 => checkSingle(0wx22,cs) orelse checkRef(0wx22,cs) 
	    | _ => true

      (*--------------------------------------------------------------------*)
      (* Given the declaration of an entity check whether it is predefined. *)
      (* If no return false. If yes, check whether is was already declared  *)
      (* and whether it is correctly declared. See 4.6:                     *)
      (*                                                                    *)
      (*   All XML processors must recognize these entities whether they    *)
      (*   are declared or not. For interoperability, valid XML documents   *)
      (*   should declare these entities, like any others, before using     *)
      (*   them. If the entities in question are declared, they must be     *)
      (*   declared as internal entities whose replacement text is the      *)
      (*   single character being escaped or a character reference to that  *)
      (*   character, as shown below.                                       *)
      (*                                                                    *)
      (*   <!ENTITY lt     "&#38;#60;">                                     *)
      (*   <!ENTITY gt     "&#62;">                                         *)
      (*   <!ENTITY amp    "&#38;#38;">                                     *)
      (*   <!ENTITY apos   "&#39;">                                         *)
      (*   <!ENTITY quot   "&#34;">                                         *)
      (*                                                                    *)
      (*   Note that the < and & characters in the declarations of "lt" and *)
      (*   "amp" are doubly escaped to meet the requirement that entity     *)
      (*   replacement be well-formed.                                      *)
      (*                                                                    *)
      (* print an error if the entity was already declared.                 *)
      (* print an error if the declaration is not correct.                  *)
      (*--------------------------------------------------------------------*)
      fun checkPredefined dtd (a,q) (idx,ent) = 
	 if !O_VALIDATE andalso idx>=1 andalso idx<=5 then 
	    let
	       val a1 = if !O_WARN_MULT_ENT_DECL andalso isRedefined dtd idx
			   then let val warn = WARN_MULT_DECL(IT_GEN_ENT,Index2GenEnt dtd idx)
				in hookWarning(a,(getPos q,warn))
				end
			else a before setRedefined dtd idx
	       val a2 = 
		  if !O_CHECK_PREDEFINED then 
		     let val correct = 
			case ent 
			  of GE_INTERN(_,rep) => checkPredef (idx,Vector2Data rep)
			   | _ => false
		     in if correct then a1
			else let val err = ERR_DECL_PREDEF(Index2GenEnt dtd idx,validPredef idx)
			     in hookError(a1,(getPos q,err))
			     end
		     end
		  else a1
	    in (true,a2)
	    end
	 else (false,a)
	    
      (*--------------------------------------------------------------------*)
      (* add an entity declaration to the DTD tables. 4.2                   *)
      (*                                                                    *)
      (*   ... If the same entity is declared more than once, the first     *)
      (*   declaration encountered is binding; at user option, an XML       *)
      (*   processor may issue a warning if entities are declared multiple  *)
      (*   times.                                                           *)
      (*                                                                    *)
      (* For general entities, check whether it is a predefined entity and  *)
      (* if so, whether it is declared correctly.                           *)
      (*--------------------------------------------------------------------*)
      (* print a warning and ignore the declaration if the notation was     *)
      (* declared previously.                                               *)
      (*--------------------------------------------------------------------*)
      fun addGenEnt dtd (a,q) (idx,ent,ext) = 
	 case getGenEnt dtd idx
	   of (GE_NULL,_) => a before setGenEnt dtd (idx,(ent,ext))
	    | _ => let val (pre,a1) = checkPredefined dtd (a,q) (idx,ent) 
		   in if pre orelse not (!O_WARN_MULT_ENT_DECL) then a1
		      else hookWarning(a1,(getPos q,WARN_MULT_DECL
					   (IT_GEN_ENT,Index2GenEnt dtd idx)))
		   end

      fun addParEnt dtd (a,q) (idx,ent,ext) = 
	 case getParEnt dtd idx
	   of (PE_NULL,_) => a before setParEnt dtd (idx,(ent,ext))
	    | _ => if !O_WARN_MULT_ENT_DECL 
		      then hookWarning(a,(getPos q,WARN_MULT_DECL
					  (IT_PAR_ENT,Index2ParEnt dtd idx)))
		   else a

      (*--------------------------------------------------------------------*)
      (* at option print a warning if not all predefined entities have been *)
      (* declared. Cf. 4.1:                                                 *)
      (*                                                                    *)
      (*   For interoperability, valid documents should declare the         *)
      (*   entities amp, lt, gt, apos, quot, in the form specified in       *)
      (*   "4.6 Predefined Entities".                                       *)
      (*--------------------------------------------------------------------*)
      fun checkPreDefined dtd (a,q) = 
	 if !O_VALIDATE andalso !O_INTEROPERABILITY andalso 
	    !O_WARN_SHOULD_DECLARE andalso hasDtd dtd  
	    then case notRedefined dtd
		   of nil => a
		    | ents => hookWarning(a,(getPos q,WARN_SHOULD_DECLARE ents))
	 else a

      (*--------------------------------------------------------------------*)
      (* add a notation declaration to the DTD tables.                      *)
      (*                                                                    *)
      (* though the rec. says nothing about repeated notation declarations, *)
      (* I assume that the intention is to treat them like entities, i.e.   *)
      (* ignore repeated declarations with an optional warning.             *)
      (*                                                                    *)
      (* print a warning and ignore the declaration if the notation was     *)
      (* declared previously.                                               *)
      (*--------------------------------------------------------------------*)
      fun addNotation dtd (a,q) (idx,nt) = 
	 if hasNotation dtd idx 
	    then if !O_WARN_MULT_NOT_DECL 
		    then hookWarning(a,(getPos q,WARN_MULT_DECL
					(IT_NOTATION,Index2AttNot dtd idx)))
		 else a
	 else a before setNotation dtd (idx,nt)

      (*--------------------------------------------------------------------*)
      (* add an element declaration to the element table. Only the content  *)
      (* part of the element info is updated. 3.2:                          *)
      (*                                                                    *)
      (*   Validity Constraint: Unique Element Type Declaration             *)
      (*   No element type may be declared more than once.                  *)
      (*                                                                    *)
      (* print an error and ignore the declaration if the element was       *)
      (* declared previously.                                               *)
      (*--------------------------------------------------------------------*)
      fun addElement dtd (a,q) (idx,cont,ext) = 
	 let val {decl,atts,errAtts,...} = getElement dtd idx
	 in case decl
	      of NONE => a before setElement dtd (idx,{decl    = SOME(cont,ext),
						       atts    = atts,
						       errAtts = errAtts})
	       | SOME _ => if !O_VALIDATE 
			      then hookError(a,(getPos q,ERR_REDEC_ELEM(Index2Element dtd idx))) 
			   else a
	 end

      (*--------------------------------------------------------------------*)
      (* at option, pretend an element is declared by adding a default      *)
      (* declaration. Only the decl flag of the element info is updated.    *)
      (*--------------------------------------------------------------------*)
      fun handleUndeclElement dtd idx = 
         let 
            val {atts,errAtts,...} = getElement dtd idx
            val newInfo = {decl    = SOME(CT_ANY,false),
                           atts    = atts,
                           errAtts = errAtts}
         in newInfo before setElement dtd (idx,newInfo)
         end

      (*--------------------------------------------------------------------*)
      (* check whether an element is declared and whether it already had an *)
      (* attribute list declaration. Cf. 3.3:                               *)
      (*                                                                    *)
      (*   At user option, an XML processor may issue a warning if          *)
      (*   attributes are declared for an element type not itself declared, *)
      (*   but this is not an error.                                        *)
      (*                                                                    *)
      (*   ... an XML processor may at user option issue a warning when     *)
      (*   more than one attribute-list declaration is provided for a given *)
      (*   element type, ...                                                *)
      (*                                                                    *)
      (* print a warning if the element is not declared or already had an   *)
      (* attribute list declaration.                                        *)
      (*--------------------------------------------------------------------*)
      fun enterAttList dtd (a,q) idx =
	 let 
	    val {decl,atts,errAtts,...} = getElement dtd idx
	    val a1 = if isSome decl orelse not (!O_WARN_ATT_NO_ELEM) then a
		     else hookWarning(a,(getPos q,WARN_ATT_UNDEC_ELEM(Index2Element dtd idx)))
	 in 
	    case atts
	      of NONE => a1 before 
		 setElement dtd (idx,{decl=decl,atts=SOME(nil,false),errAtts=errAtts}) 
	       | _ => if !O_INTEROPERABILITY andalso !O_WARN_MULT_ATT_DECL 
			 then hookWarning(a1,(getPos q,WARN_MULT_ATT_DECL(Index2Element dtd idx)))
		      else a1
	 end

      (*--------------------------------------------------------------------*)
      (* check whether attribute "xml:space" is declared correctly. 2.10:   *)
      (*                                                                    *)
      (*   A special attribute named xml:space may be attached ... In valid *)
      (*   documents, this attribute, like any other, must be declared if   *)
      (*   it is used. When declared, it must be given as an enumerated     *)
      (*   type whose only possible values are "default" and "preserve".    *)
      (*--------------------------------------------------------------------*)
      (*  XML 1.0 (Third Edition) has corrected this to: When declared,     *)
      (*  it MUST be given as an enumerated type whose values are one or    *)
      (*  both of "default" and "preserve".                                 *)
      (*--------------------------------------------------------------------*)

      fun checkAttDef (a,q) (aidx,attType,_,_) =
	 if aidx<>xmlSpaceIdx then a
	 else
	   case attType of
	     AT_GROUP [a1,a2] =>
	       if (a1 = preserveIdx andalso a2 = defaultIdx) 
		 orelse (a2 = preserveIdx andalso a1 = defaultIdx) then a
	       else hookError(a,(getPos q,ERR_XML_SPACE))
	   | AT_GROUP [a1] =>
		 if (a1 = preserveIdx orelse a1 = defaultIdx) then a
		 else hookError(a,(getPos q,ERR_XML_SPACE))
	   | _ => hookError(a,(getPos q,ERR_XML_SPACE))



      (*--------------------------------------------------------------------*)
      (* enter a definition of a single attribute to the element table.     *)
      (* ignore the definition if the attribute is already defined for that *)
      (* element. Cf. 3.3:                                                  *)
      (*                                                                    *)
      (*   When more than one AttlistDecl is provided for a given element   *)
      (*   type, the contents of all those provided are merged. When more   *)
      (*   than one definition is provided for the same attribute of a      *)
      (*   given element type, the first declaration is binding and later   *)
      (*   declarations are ignored. For interoperability, an XML processor *)
      (*   may at user option issue a warning when ... more than one        *)
      (*   attribute definition is provided for a given attribute, but this *)
      (*   is not an error.                                                 *)
      (*                                                                    *)
      (* If the attribute type is ID, check whether an element already has  *)
      (* an attribute of that type. 3.3.1:                                  *)
      (*                                                                    *)
      (*   Validity Constraint: One ID per Element Type                     *)
      (*   No element type may have more than one ID attribute specified.   *)
      (*--------------------------------------------------------------------*)
      (* print an error if the element already has an ID attribute.         *)
      (* print a warning if the attr. is already defined for this element.  *) 
      (*--------------------------------------------------------------------*)
      (* return the new application data.                                   *)
      (*--------------------------------------------------------------------*)
      fun addAttribute dtd (a,q) (eidx,attDef as (att,attType,attDefault,_)) =
	 let 
	    val a1 = checkAttDef (a,q) attDef

	    fun doit nil = (false,[attDef],a)
	      | doit (atts as (ad as (aidx,_,_,_))::rest) = 
	       if aidx=att 
		  then let val a1 = if !O_INTEROPERABILITY andalso !O_WARN_MULT_ATT_DEF 
				       then let val warn = WARN_MULT_ATT_DEF
					  (Index2Element dtd eidx,Index2AttNot dtd att)
					    in hookWarning(a,(getPos q,warn))
					    end
				    else a
		       in (true,atts,a1)
		       end
	       else (if aidx<att then (false,attDef::atts,a)
		     else let val (redefined,atts1,a1) = doit rest 
			  in (redefined,ad::atts1,a1)
			  end)

	    val {decl,atts,errAtts,...} = getElement dtd eidx
	    val (defs,hadId) = getOpt(atts,(nil,false))
	    val (redefined,defs1,a1) = doit defs
	    val (newId,a1) = if isIdType attType 
				then let val a1 = if hadId andalso (not redefined) andalso !O_VALIDATE 
						     then hookError(a,(getPos q,ERR_MULT_ID_ELEM
								       (Index2Element dtd eidx)))
						  else a
				     in (true,a1)
				     end
			     else (hadId,a)
	    val (_,defs1,a1) = doit defs
	    val _ = setElement dtd (eidx,{decl    = decl,
					  atts    = SOME(defs1,newId),
					  errAtts = errAtts})
	 in a1
	 end
      
      (*--------------------------------------------------------------------*)
      (* check whether a name starts with (a case variant of) "xml" and if  *)
      (* yes, whether it is an allowed name from the spec. Cf. 3:           *)
      (*                                                                    *)
      (*   This specification does not constrain ... names of the element   *)
      (*   types and attributes, except that names beginning with a match   *)
      (*   to (('X'|'x')('M'|'m')('L'|'l')) are reserved for standardization*)
      (*   in this or future versions of this specification.                *)
      (*                                                                    *)
      (* and 2.10, 2.12:                                                    *)
      (*                                                                    *)
      (*   ... a special attribute named xml:space may be attached ...      *)
      (*   ... A special attribute named xml:lang may be inserted ...       *)
      (*                                                                    *)
      (* print an error if the name is reserved and not standardized.       *)
      (*--------------------------------------------------------------------*)
      fun startsWithXml name =
	 case name
	   of c1::c2::c3::cs => (c1=0wx58 orelse c1=0wx78) andalso 
	      (c2=0wx4D orelse c2=0wx6D) andalso (c3=0wx4C orelse c3=0wx6C)
	    | _ => false
      fun checkAttName (a,q) name = 
	 if !O_CHECK_RESERVED andalso startsWithXml name then 
	    case name 
	      of [0wx78,0wx6d,0wx6c,0wx3a,0wx6c,0wx61,0wx6e,0wx67] (* ":lang" *) => a
	       | [0wx78,0wx6d,0wx6c,0wx3a,0wx73,0wx70,0wx61,0wx63,0wx65] (* ":space" *) => a
	       | _ => hookError(a,(getPos q,ERR_RESERVED(name,IT_ATT_NAME)))
	 else a
      fun checkElemName (a,q) name = 
	 if !O_CHECK_RESERVED andalso startsWithXml name 
	    then hookError(a,(getPos q,ERR_RESERVED(name,IT_ELEM)))
	 else a

      (*--------------------------------------------------------------------*)
      (* check for each element in the dtd, whether a name token occurs     *)
      (* more than once in its enumerated attribute types.                  *)
      (*                                                                    *)
      (* print a warning for each element where this is true.               *)
      (*                                                                    *)
      (* return nothing.                                                    *)
      (*--------------------------------------------------------------------*)
      fun checkMultEnum dtd (a,q) =
	 if !O_INTEROPERABILITY andalso !O_WARN_MULT_ENUM then 
	    let 
	       fun doElem a idx = 
		  let
                     (*-----------------------------------------------------*)
		     (* for each i, add i to yet if it not in that list.    *)
		     (* otherwise add it to dup.                            *)
                     (*-----------------------------------------------------*)
		     fun do_list yd nil = yd
		       | do_list (yet,dup) (i::is) = 
			let val yd' = case insertNewInt (i,yet)
					of NONE => (yet,insertInt (i,dup))
					 | SOME new => (new,dup)
			in do_list yd' is
			end
                     (*-----------------------------------------------------*)
		     (* For each enumerated attribute type call the appro-  *)
		     (* priate function.                                    *)
                     (*-----------------------------------------------------*)
		     fun doit (yet,dup) nil = dup
		       | doit (yet,dup) ((_,attType,_,_)::rest) =
			case attType
			  of AT_GROUP is => doit (do_list (yet,dup) is) rest
			   | AT_NOTATION is => doit (do_list (yet,dup) is) rest
			   | _ => doit (yet,dup) rest

		     val defs = case #atts(getElement dtd idx)
				  of NONE => nil
				   | SOME(defs,_) => defs
		     val dup = doit (nil,nil) defs
		  in 
		     if null dup then a
		     else hookWarning(a,(getPos q,WARN_ENUM_ATTS
					 (Index2Element dtd idx,map (Index2AttNot dtd) dup)))
		  end
               (*-----------------------------------------------------------*)
               (* the highest used index is usedIndices-1.                  *)
               (*-----------------------------------------------------------*)
	       val maxIdx = maxUsedElem dtd

	       fun doit a i = if i>maxIdx then a else doit (doElem a i) (i+1)
	    in 
	       doit a 0
	    end
	 else a

      (*--------------------------------------------------------------------*)
      (* check for all id names refereneced by some IDREF attribute whether *)
      (* it was also declared by an ID attribute.                           *)
      (*                                                                    *)
      (* print an error if a referenced ID name was not defined.            *)
      (*                                                                    *)
      (* return nothing.                                                    *)
      (*--------------------------------------------------------------------*)
      fun checkDefinedIds dtd (a,q) = 
	 if !O_VALIDATE then 
	    let 
	       val maxId = maxUsedId dtd
	       
	       fun doOne a i = let val (decl,refs) = getId dtd i
			       in if decl orelse null refs then a
				  else hookError(a,(hd refs,ERR_UNDECL_ID(Index2Id dtd i,tl refs)))
			       end
	       fun doAll a i = if i>maxId then a else doAll (doOne a i) (i+1)
	    in 
	       doAll a 0
	    end 
	 else a

      (*--------------------------------------------------------------------*)
      (* check for all declared unparsed entities, whether their notations  *)
      (* have been declared.                                                *)
      (*                                                                    *)
      (* print an error if a notation was not declared.                     *)
      (*                                                                    *)
      (* return nothing.                                                    *)
      (*--------------------------------------------------------------------*)
      fun checkUnparsed dtd a = 
	 if !O_VALIDATE then 
	    let 
	       val maxGen = maxUsedGen dtd
	       
	       fun doOne a i = 
		  case getGenEnt dtd i
		    of (GE_UNPARSED(_,nidx,pos),_) => 
		       if hasNotation dtd nidx then a
		       else hookError(a,(pos,ERR_UNDECLARED
					 (IT_NOTATION,Index2AttNot dtd nidx,LOC_NONE)))
		     | _ => a
	       fun doAll a i = if i>maxGen then a else doAll (doOne a i) (i+1)
	    in 
	       doAll a 0
	    end 
	 else a
   end

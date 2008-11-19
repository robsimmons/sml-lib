(*--------------------------------------------------------------------------*)
(* Structure: Dtd                                                           *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   AttNot2Index        : none                                             *)
(*   Element2Index       : none                                             *)
(*   GenEnt2Index        : none                                             *)
(*   Id2Index            : none 		                            *)
(*   Index2AttNot        : NoSuchIndex	                                    *)
(*   Index2Element       : NoSuchIndex	                                    *)
(*   Index2GenEnt        : NoSuchIndex                                      *)
(*   Index2Id            : NoSuchIndex			                    *)
(*   Index2ParEnt        : NoSuchIndex                                      *)
(*   ParEnt2Index        : none                                             *)
(*   entitiesWellformed  : none                                             *)
(*   getElement          : NoSuchIndex	                                    *)
(*   getGenEnt           : NoSuchIndex                                      *)
(*   getId               : NoSuchIndex	                                    *)
(*   getNotation         : NoSuchIndex	                                    *)
(*   getParEnt           : NoSuchIndex                                      *)
(*   hasNotation         : NoSuchIndex	                                    *)
(*   initDtdTables       : none                                             *)
(*   maxUsedElem         : none                                             *)
(*   maxUsedId           : none                                             *)
(*   printAttNotTable    : none				                    *)
(*   printIdTable        : none				                    *)
(*   printParEntTable    : none                                             *)
(*   printxElementTable  : none		                                    *)
(*   printxGenEntTable   : none                                             *)
(*   setElement          : NoSuchIndex	                                    *)
(*   setGenEnt           : NoSuchIndex                                      *)
(*   setId               : NoSuchIndex	                                    *)
(*   setNotation         : NoSuchIndex	                                    *)
(*   setParEnt           : NoSuchIndex                                      *)
(*--------------------------------------------------------------------------*)
signature Dtd = 
   sig
      type Dtd 
      
      val hasDtd       : Dtd -> bool
      val hasExternal  : Dtd -> bool
      val standsAlone  : Dtd -> bool
	 
      val setHasDtd     : Dtd -> unit
      val setExternal   : Dtd -> unit
      val setStandAlone : Dtd -> bool -> unit
	 
      val entitiesWellformed : Dtd -> bool

      val validPredef  : int -> UniChar.Vector
      val isRedefined  : Dtd -> int -> bool
      val setRedefined : Dtd -> int -> unit
      val notRedefined : Dtd -> UniChar.Data list

      val AttNot2Index    : Dtd -> UniChar.Data -> int
      val Element2Index   : Dtd -> UniChar.Data -> int
      val Id2Index        : Dtd -> UniChar.Data -> int
      val GenEnt2Index    : Dtd -> UniChar.Data -> int
      val ParEnt2Index    : Dtd -> UniChar.Data -> int


      val hasAttNot    : Dtd -> UniChar.Data -> int option
      val hasElement   : Dtd -> UniChar.Data -> int option
      val hasId        : Dtd -> UniChar.Data -> int option
      val hasGenEnt    : Dtd -> UniChar.Data -> int option
      val hasParEnt    : Dtd -> UniChar.Data -> int option

      val Index2Element   : Dtd -> int -> UniChar.Data
      val Index2Id        : Dtd -> int -> UniChar.Data
      val Index2GenEnt    : Dtd -> int -> UniChar.Data
      val Index2AttNot    : Dtd -> int -> UniChar.Data
      val Index2ParEnt    : Dtd -> int -> UniChar.Data
	 
      val getId       : Dtd -> int -> Base.IdInfo
      val getElement  : Dtd -> int -> Base.ElemInfo
      val getGenEnt   : Dtd -> int -> Base.GenEntInfo
      val getNotation : Dtd -> int -> Base.NotationInfo
      val getParEnt   : Dtd -> int -> Base.ParEntInfo

      val hasNotation : Dtd -> int -> bool

      val setId       : Dtd -> int * Base.IdInfo -> unit
      val setElement  : Dtd -> int * Base.ElemInfo -> unit
      val setGenEnt   : Dtd -> int * Base.GenEntInfo -> unit
      val setNotation : Dtd -> int * Base.ExternalId -> unit
      val setParEnt   : Dtd -> int * Base.ParEntInfo -> unit

      val maxUsedId   : Dtd -> int
      val maxUsedElem : Dtd -> int
      val maxUsedGen  : Dtd -> int

      val initDtdTables  : unit -> Dtd
      val printDtdTables : Dtd -> unit
	 
      val printAttNotTable  : Dtd -> unit
      val printIdTable      : Dtd -> unit
      val printElementTable : Dtd -> unit
      val printGenEntTable  : Dtd -> unit
      val printParEntTable  : Dtd -> unit

      val defaultIdx   : int
      val preserveIdx  : int
      val xmlLangIdx   : int
      val xmlSpaceIdx  : int
   end

structure Dtd : Dtd = 
   struct 
      open 
	 UtilInt
	 Base UniChar
	 DataDict DataSymTab
	 
      val O_TS_ELEM    = ref 6 (* Initial size of element table             *)
      val O_TS_GEN_ENT = ref 6 (* Initial size of general entity table      *)
      val O_TS_ID      = ref 6 (* Initial size of id attribute table        *)
      val O_TS_ATT_NOT = ref 6 (* Initial size of notation table            *)
      val O_TS_PAR_ENT = ref 6 (* Initial size of parameter entity table    *)

      (*--------------------------------------------------------------------*)
      (* this is how the predefined entities must be declared.              *)
      (*--------------------------------------------------------------------*)
      val predefined = Vector.fromList 
	 (map (fn (x,y,z) => (String2Data x,String2Vector y,String2Vector z))
	  [("","",""),
	   ("amp" ,"'&#38;'","&#38;"),
	   ("lt"  ,"'&#60;'","&#60;"),
	   ("gt"  ,"'&#62;'","&#62;"),
	   ("apos","\"'\""  ,"'"    ),
	   ("quot","'\"'"   ,"\""   )])
      fun validPredef i = #3(Vector.sub(predefined,i))
	 
      (*--------------------------------------------------------------------*)
      (* this type holds all information relevent to the DTD.               *)
      (*--------------------------------------------------------------------*)
      type Dtd = {hasDtdFlag     : bool ref,
		  standAloneFlag : bool ref,
		  externalFlag   : bool ref,
		  elDict         : ElemInfo DataDict.Dict,
		  genDict        : GenEntInfo DataDict.Dict,
		  idDict         : IdInfo DataDict.Dict,
		  notDict        : NotationInfo DataDict.Dict,
		  parDict        : ParEntInfo DataDict.Dict,
		  preRedef       : bool array
		  }

      fun newDtd() = {hasDtdFlag     = ref false,
		      standAloneFlag = ref false,
		      externalFlag   = ref false,
		      elDict         = nullDict ("element",nullElemInfo),
		      idDict         = nullDict ("ID name",nullIdInfo),
		      genDict        = nullDict ("general entity",(GE_NULL,false)),
		      notDict        = nullDict ("attribute and notation",NONE:NotationInfo),
		      parDict        = nullDict ("parameter entity",(PE_NULL,false)),
		      preRedef       = Array.array(6,false)
		      } : Dtd 

      val default  = String2Data "default"
      val preserve = String2Data "preserve"
      val xmlLang  = String2Data "xml:lang"
      val xmlSpace = String2Data "xml:space"

      (*--------------------------------------------------------------------*)
      (* standalone status, existance of a DTD and of external declarations *)
      (* externalFlag is true if there is an external subset or a (not nece-*)
      (* ssarily external) parameter entity reference in the DTD. (cf. 4.1) *)
      (*--------------------------------------------------------------------*)
      fun standsAlone (dtd:Dtd) = !(#standAloneFlag dtd)
      fun hasExternal (dtd:Dtd) = !(#externalFlag dtd)
      fun hasDtd (dtd:Dtd)      = !(#hasDtdFlag dtd)

      fun setHasDtd (dtd:Dtd) = #hasDtdFlag dtd := true
      fun setExternal (dtd:Dtd) = #externalFlag dtd := true
      fun setStandAlone (dtd:Dtd) x = #standAloneFlag dtd := x


      (*--------------------------------------------------------------------*)
      (* 4.1:                                                               *)
      (*   Well-Formedness Constraint: Entity Declared                      *)
      (*   In a document without any DTD, a document with only an internal  *)
      (*   DTD subset which contains no parameter entity references, or a   *)
      (*   document with "standalone='yes'", the Name given in the entity   *)
      (*   reference must match that in an entity declaration ... Note that *)
      (*   if entities are declared in the external subset or in external   *)
      (*   parameter entities, a non-validating processor is not obligated  *)
      (*   to read and process their declarations; for such documents, the  *)
      (*   rule that an entity must be declared is a well-formedness        *)
      (*   constraint only if standalone='yes'.                             *)
      (*                                                                    *)
      (* Thus a reference to an undeclared entity is a well-formedness      *)
      (* error if either #hasDtdFlag or #externalFlag is false, or if       *)
      (* #standaloneFlag is true                                            *)
      (*--------------------------------------------------------------------*)
      (* bug fixed 080600: changed !hasDtdFlag to not(!hasDtdFlag)          *)
      (*--------------------------------------------------------------------*)
      fun entitiesWellformed ({hasDtdFlag,standAloneFlag,externalFlag,...}:Dtd) = 
	 not (!hasDtdFlag andalso !externalFlag) orelse !standAloneFlag 

      fun initStandalone ({hasDtdFlag,standAloneFlag,externalFlag,...}:Dtd) = 
	 (hasDtdFlag := false; standAloneFlag := false; externalFlag := false)

      (*--------------------------------------------------------------------*)
      (* this array tells whether the predefined entities (index 1-5) have  *)
      (* been declared in the dtd.                                          *)
      (*--------------------------------------------------------------------*)
      fun isRedefined (dtd:Dtd) i = Array.sub(#preRedef dtd,i)
      fun setRedefined (dtd:Dtd) i = Array.update(#preRedef dtd,i,true)
      fun notRedefined dtd = List.mapPartial 
	 (fn i => if isRedefined dtd i then NONE else SOME(#1(Vector.sub(predefined,i)))) 
	 [1,2,3,4,5]

      fun AttNot2Index  (dtd:Dtd) name = getIndex(#notDict dtd,name)
      fun Element2Index (dtd:Dtd) name = getIndex(#elDict dtd,name)
      fun GenEnt2Index  (dtd:Dtd) name = getIndex(#genDict dtd,name)
      fun Id2Index      (dtd:Dtd) name = getIndex(#idDict dtd,name)
      fun ParEnt2Index  (dtd:Dtd) name = getIndex(#parDict dtd,name)

      fun hasAttNot    (dtd:Dtd) name = hasIndex(#notDict dtd,name)
      fun hasElement   (dtd:Dtd) name = hasIndex(#elDict dtd,name)
      fun hasId        (dtd:Dtd) name = hasIndex(#idDict dtd,name)
      fun hasGenEnt    (dtd:Dtd) name = hasIndex(#genDict dtd,name)
      fun hasParEnt    (dtd:Dtd) name = hasIndex(#parDict dtd,name)

      fun Index2AttNot  (dtd:Dtd) idx = getKey(#notDict dtd,idx)
      fun Index2Element (dtd:Dtd) idx = getKey(#elDict dtd,idx)
      fun Index2GenEnt  (dtd:Dtd) idx = getKey(#genDict dtd,idx)
      fun Index2Id      (dtd:Dtd) idx = getKey(#idDict dtd,idx)
      fun Index2ParEnt  (dtd:Dtd) idx = getKey(#parDict dtd,idx)

      fun getElement  (dtd:Dtd) idx = getByIndex(#elDict dtd,idx)
      fun getGenEnt   (dtd:Dtd) idx = getByIndex(#genDict dtd,idx)
      fun getId       (dtd:Dtd) idx = getByIndex(#idDict dtd,idx)
      fun getNotation (dtd:Dtd) idx = getByIndex(#notDict dtd,idx)
      fun getParEnt   (dtd:Dtd) idx = getByIndex(#parDict dtd,idx)

      fun hasNotation (dtd:Dtd) idx = isSome(getByIndex(#notDict dtd,idx))

      fun setElement  (dtd:Dtd) (idx,el) = setByIndex(#elDict dtd,idx,el)
      fun setGenEnt   (dtd:Dtd) (idx,ge) = setByIndex(#genDict dtd,idx,ge)
      fun setId       (dtd:Dtd) (idx,a)  = setByIndex(#idDict dtd,idx,a)
      fun setNotation (dtd:Dtd) (idx,nt) = setByIndex(#notDict dtd,idx,SOME nt)
      fun setParEnt   (dtd:Dtd) (idx,pe) = setByIndex(#parDict dtd,idx,pe)

      fun maxUsedElem (dtd:Dtd) = usedIndices(#elDict dtd)-1
      fun maxUsedGen  (dtd:Dtd) = usedIndices(#genDict dtd)-1
      fun maxUsedId   (dtd:Dtd) = usedIndices(#idDict dtd)-1

      (*--------------------------------------------------------------------*)
      (* initialize the attribute tables. Make sure that indices 0...3 are  *)
      (* assigned to "default", "preserve", "xml:lang" and "xml:space".     *)
      (*--------------------------------------------------------------------*)
      fun initAttNotTable (dtd as {idDict,notDict,...}:Dtd) = 
	 let 
	    val _ = clearDict(notDict,SOME(!O_TS_ATT_NOT))
	    val _ = clearDict(idDict,SOME(!O_TS_ID))
	    val _ = AttNot2Index dtd default
	    val _ = AttNot2Index dtd preserve
	    val _ = AttNot2Index dtd xmlLang
	    val _ = AttNot2Index dtd xmlSpace
	 in ()
	 end
      fun initElementTable (dtd:Dtd) = clearDict(#elDict dtd,SOME(!O_TS_ELEM))
      (*--------------------------------------------------------------------*)
      (* reserve 0 for gen entity -,    i.e., the document entity.          *) 
      (* reserve 1 for gen entity amp,  i.e., "&#38;#38;"                   *)
      (* reserve 2 for gen entity lt,   i.e., "&#38;#60;"                   *) 
      (* reserve 3 for gen entity gt,   i.e., "&#62;"                       *) 
      (* reserve 4 for gen entity apos, i.e., "&#39;"                       *) 
      (* reserve 5 for gen entity quot, i.e., "&#34;"                       *) 
      (* reserve 0 for par entity -,    i.e., the external dtd subset.      *) 
      (*                                                                    *)
      (* Cf. 4.1:                                                           *)
      (*                                                                    *)
      (*   ... except that well-formed documents need not declare any of    *)
      (*   the following entities: amp, lt, gt, apos, quot.                 *)
      (*                                                                    *)
      (* and 4.6:                                                           *)
      (*                                                                    *)
      (*   <!ENTITY lt     "&#38;#60;">                                     *)
      (*   <!ENTITY gt     "&#62;">                                         *)
      (*   <!ENTITY amp    "&#38;#38;">                                     *)
      (*   <!ENTITY apos   "&#39;">                                         *)
      (*   <!ENTITY quot   "&#34;">                                         *)
      (*--------------------------------------------------------------------*)
      fun initEntityTables (dtd as {genDict,parDict,preRedef,...}:Dtd) = 
	 let 
	    val _ = clearDict(genDict,SOME(!O_TS_GEN_ENT))
	    val _ = clearDict(parDict,SOME(!O_TS_PAR_ENT))
	    val _ = map (fn i => Array.update(preRedef,i,false)) [1,2,3,4,5]
	    val _ = GenEnt2Index dtd [0wx2D] (* "-" *)
	    val _ = ParEnt2Index dtd [0wx2D] (* "-" *)
	    val _ = VectorSlice.appi 
	       (fn (_,(name,lit,cs)) 
		=> (setGenEnt dtd (GenEnt2Index dtd name,(GE_INTERN(lit,cs),false)))) 
	       (VectorSlice.slice (predefined,1,NONE))
	 in ()
	 end

      fun initDtdTables() = 
	 let
	    val dtd = newDtd()
	    val _ = initAttNotTable dtd
	    val _ = initElementTable dtd
	    val _ = initEntityTables dtd
	    val _ = initStandalone dtd
	 in dtd
	 end
			  	    
      local 
	 val dtd = initDtdTables() 
      in 
	 val defaultIdx = AttNot2Index dtd default
	 val preserveIdx = AttNot2Index dtd preserve
	 val xmlLangIdx = AttNot2Index dtd xmlLang
	 val xmlSpaceIdx = AttNot2Index dtd xmlSpace
      end

      fun printAttNotTable (dtd:Dtd) = 
	 printDict NotationInfo2String (#notDict dtd)
      fun printElementTable dtd = 
	 printDict (ElemInfo2xString (UniChar.Data2String o (Index2AttNot dtd),
				      UniChar.Data2String o (Index2Element dtd),
				      UniChar.Data2String o (Index2GenEnt dtd),
				      UniChar.Data2String o (Index2Id dtd),
				      UniChar.Data2String o (Index2AttNot dtd))) (#elDict dtd)
      fun printGenEntTable dtd = 
	 printDict (fn (ent,ext) => GenEntity2xString (Data2String o (Index2AttNot dtd)) ent
		    ^(if ext then "[external]" else "")) (#genDict dtd)
      fun printIdTable (dtd:Dtd) = printDict (IdInfo2String) (#idDict dtd)
      fun printParEntTable (dtd:Dtd) = 
	 printDict (fn (ent,ext) => ParEntity2String ent
		    ^(if ext then "[external]" else "")) (#parDict dtd)

      fun printDtdTables dtd = (printAttNotTable dtd;
				printElementTable dtd;
				printGenEntTable dtd;
				printIdTable dtd;
				printParEntTable dtd)
   end

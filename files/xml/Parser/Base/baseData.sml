(*--------------------------------------------------------------------------*)
(* Structure: BaseData                                                      *)
(*--------------------------------------------------------------------------*)

structure BaseData = 
   struct
      open DfaData

      (*--- external ids may have a public id and must have a system id ---*)
      (*--- for notations, however, also the system id can be optional ----*)
      datatype ExternalId = 
	 EXTID of (string * UniChar.Char) option * (Uri.Uri * Uri.Uri * UniChar.Char) option 

      (*--- external ids may have a public id and must have a system id ---*)
      type NotationInfo = ExternalId option
	 
      (*--- replacement of a general entity ---*)
      datatype GenEntity = 
	 GE_NULL
       | GE_INTERN of UniChar.Vector * UniChar.Vector
       | GE_EXTERN of ExternalId
       | GE_UNPARSED of ExternalId * int * Errors.Position
      type GenEntInfo = GenEntity * bool

      fun isExtGen (GE_EXTERN _) = true
	| isExtGen _ = false 
	 
      (*--- replacement of a parameter entity ---*)
      datatype ParEntity = 
	 PE_NULL
       | PE_INTERN of UniChar.Vector * UniChar.Vector
       | PE_EXTERN of ExternalId
      type ParEntInfo = ParEntity * bool

      fun isExtPar (PE_EXTERN _) = true
	| isExtPar _ = false 
	 
      (*--- declared type of an attribute ---*)
      datatype AttType = 
	 AT_CDATA
       | AT_NMTOKEN
       | AT_NMTOKENS
       | AT_ID 
       | AT_IDREF
       | AT_IDREFS
       | AT_ENTITY
       | AT_ENTITIES
       | AT_GROUP of int list
       | AT_NOTATION of int list

      (*--- typed attribute value ---*)
      datatype AttValue =
	 AV_CDATA of UniChar.Vector
       | AV_NMTOKEN of UniChar.Data
       | AV_NMTOKENS of UniChar.Data list
       | AV_ID of int
       | AV_IDREF of int
       | AV_IDREFS of int list
       | AV_ENTITY of int 
       | AV_ENTITIES of int list
       | AV_GROUP of int list * int
       | AV_NOTATION of int list * int

      fun isIdType at = at=AT_ID 

      (*--- default values of attributes ---*) 
      datatype AttDefault = 
	 AD_IMPLIED
       | AD_REQUIRED
       | AD_DEFAULT of (UniChar.Vector * UniChar.Vector * AttValue option) 
	 * (Errors.Position * bool ref)
       | AD_FIXED of (UniChar.Vector * UniChar.Vector * AttValue option) 
	 * (Errors.Position * bool ref)
	  
      (*--- attribute definition (list) ---*)
      (*--- the boolean says whether it was externally declared ---*) 
      type AttDef = int * AttType * AttDefault * bool
      type AttDefList = AttDef list

      (*--- content specification ---*)
      fun defaultAttDef idx = (idx,AT_CDATA,AD_IMPLIED,false)

      (*--- content specification ---*)
      datatype ContentSpec =
	 CT_ANY
       | CT_EMPTY
       | CT_MIXED of int list
       | CT_ELEMENT of DfaData.ContentModel * DfaData.Dfa

      fun isMixed ct = 
	 case ct 
	   of CT_ANY     => true
	    | CT_MIXED _ => true
	    | _          => false
	      
      type ElemInfo = {decl    : (ContentSpec * bool) option,
		       atts    : (AttDefList * bool) option,
		       errAtts : int list}

      val nullElemInfo : ElemInfo = {decl=NONE,
				     atts=NONE,
				     errAtts=nil}

      (*--------------------------------------------------------------------*)
      (* the id info tells whether an id value has occurred for a name and  *)
      (* the list of all positions where it occurred as an idref value.     *)
      (*--------------------------------------------------------------------*)
      type IdInfo = bool * Errors.Position list
      val nullIdInfo : IdInfo = (false,nil)
   end

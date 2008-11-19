











(*--------------------------------------------------------------------------*)
(* Structure: BaseString                                                    *)
(*                                                                          *)
(* Depends on:                                                              *)
(*   UniChar                                                                *)
(*   Dfa                                                                    *)
(*   BaseData                                                               *)
(*   UtilString                                                             *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   ElemInfo2xString  : InternalError                                      *)
(*   ExternalId2String : none                                               *)
(*   GenEntity2xString : none                                               *)
(*   Notation2String   : none                                               *)
(*   IdInfo2String     : none                                               *)
(*   ParEntity2String  : none                                               *)
(*--------------------------------------------------------------------------*)
signature BaseString = 
   sig
      val ExternalId2String   : BaseData.ExternalId -> string
      val NotationInfo2String : BaseData.NotationInfo -> string

      val GenEntity2xString : (int -> string) -> BaseData.GenEntity -> string
      val ParEntity2String  : BaseData.ParEntity -> string

      val ElemInfo2xString  : (int -> string) * (int -> string) * (int -> string) 
	 * (int -> string) * (int -> string) -> BaseData.ElemInfo -> string

      val IdInfo2String     : BaseData.IdInfo -> string
   end

structure BaseString : BaseString = 
   struct
      open 
	 UtilString Uri
	 Errors UniChar DfaString
	 BaseData 

      val THIS_MODULE = "BaseString"

      fun ExternalId2String (EXTID id) = 
	 case id 
	   of (SOME(p,pq),SOME(rel,s,sq)) => String.concat
	      ["PUBLIC ",quoteUni pq p,
	       " ",quoteUni sq (Uri2String rel),
	       " @ ",quoteUni sq (Uri2String s)]
	    | (SOME(p,pq),NONE) => String.concat
	      ["PUBLIC ",quoteUni pq p]
	    | (NONE,SOME(rel,s,sq)) => String.concat
	      ["SYSTEM ",quoteUni sq (Uri2String rel),
	       " @ ",quoteUni sq (Uri2String s)]
	    | (NONE,NONE) => "<none>"
      fun NotationInfo2String not = 
	 case not 
	   of NONE => "undeclared"
	    | SOME extId => ExternalId2String extId

      fun GenEntity2xString NotIdx2String ge = 
	 case ge
	   of GE_NULL => "NULL"
	    | GE_INTERN(lit,cv) => let val quote = Vector.sub(lit,0)
				   in String.concat ["INTERN ",Vector2String lit,
						     " - ",quoteVector quote cv]
				   end
	    | GE_EXTERN id => "EXTERN "^ExternalId2String id
	    | GE_UNPARSED(id,not,_) => "UNPARSED "^ExternalId2String id^" "^NotIdx2String not

      fun ParEntity2String pe = 
	 case pe
	   of PE_NULL => "NULL"
	    | PE_INTERN(lit,cv) => let val quote = Vector.sub(lit,0)
				   in String.concat ["INTERN ",Vector2String lit,
						     " - ",quoteVector quote cv]
				   end
	    | PE_EXTERN id => "EXTERN "^ExternalId2String id

      fun ContentSpec2String Elem2String cs =
	 case cs
	   of CT_ANY => "ANY"
	    | CT_EMPTY => "EMPTY"
	    | CT_MIXED is => List2xString ("MIXED (","|",")") Elem2String is
	    | CT_ELEMENT(cm,_) => "ELEMENT "^ContentModel2String Elem2String cm
	 
      fun AttValue2xString (Att2String,Ent2String,Id2String,Not2String) quote av = 
	 quoteUni quote (case av
			   of AV_CDATA buf => Vector2String buf
			    | AV_NMTOKEN cs => Data2String cs
			    | AV_NMTOKENS css => List2xString (""," ","") Data2String css
			    | AV_ID idx => Id2String idx
			    | AV_IDREF idx => Id2String idx
			    | AV_IDREFS idxs => List2xString (""," ","") Id2String idxs
			    | AV_ENTITY idx => Ent2String idx
			    | AV_ENTITIES idxs => List2xString (""," ","") Ent2String idxs
			    | AV_GROUP(_,idx) => Att2String idx
			    | AV_NOTATION(_,idx) => Not2String idx)

      fun AttDefault2xString funs ad = 
         case ad 
	   of AD_DEFAULT ((lit,cv,av),_) => 
	      let val quote = Vector.sub(lit,0)
	      in String.concat [quoteVector quote cv," ",
				Option2String0 (AttValue2xString funs quote) av]
	      end
	    | AD_FIXED ((lit,cv,av),_) => 
	      let val quote = Vector.sub(lit,0)
	      in String.concat ["#FIXED ",quoteVector quote cv," ",
				Option2String0 (AttValue2xString funs quote) av]
	      end
	    | AD_IMPLIED => "#IMPLIED"
	    | AD_REQUIRED => "#REQUIRED"
	  
      fun AttType2xString (Att2String,Not2String) at = 
	 case at 
	   of AT_CDATA => "CDATA"
	    | AT_NMTOKEN => "NMTOKEN"
	    | AT_NMTOKENS => "NMTOKENS"
	    | AT_ID => "ID"
	    | AT_IDREF => "IDREF"
	    | AT_IDREFS => "IDREFS"
	    | AT_ENTITY => "ENTITY"
	    | AT_ENTITIES => "ENTITIES"
	    | AT_GROUP idxs => List2xString ("(","|",")") Att2String idxs
	    | AT_NOTATION idxs => List2xString ("NOTATION(","|",")") Not2String idxs

      fun AttDef2xString (funs  as (Att2String,_,_,Not2String)) (idx,attType,default,ext) = 
	 String.concat [Att2String idx," ",
			AttType2xString (Att2String,Not2String) attType," ",
			AttDefault2xString funs default,
			Bool2xString ("[external]","") ext]
			
      fun AttDefList2xString funs adl = List2xString ("",",","") (AttDef2xString funs) adl

      fun ElemInfo2xString (Att2String,Elem2String,Ent2String,Id2String,Not2String)
	 ({decl,atts,...}:ElemInfo) =
	 let val dec = case decl
			 of NONE => "elem undeclared"
			  | SOME(cont,ext) => String.concat 
			    ["elem declared ",if ext then "ex" else "in","ternally: ",
			     ContentSpec2String Elem2String cont]
	     val att = case atts
			 of NONE => "no atts declared"
			  | SOME(defs,hadId) => String.concat
			    ["atts were declared",if hadId then "(has id attribute): " else ": ",
			     AttDefList2xString (Att2String,Ent2String,Id2String,Not2String) defs]
	 in dec^att
	 end

      fun IdInfo2String (decl,refs) = 
	 Bool2xString ("declared","undeclared") decl^"/"^
	 (if null refs then "no references"
	  else List2xString ("references: ",", ","") Position2String refs)
   end







(*--------------------------------------------------------------------------*)
(* Structure: DfaString                                                     *)
(*                                                                          *)
(* Notes:                                                                   *)
(*   This structure is needed for debugging of content models and tables.   *)
(*                                                                          *)
(* Depends on:                                                              *)
(*   DfaData                                                                *)
(*   UtilString                                                             *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   Table2String        : none                                             *)
(*   ContentModel2String : none                                             *)
(*--------------------------------------------------------------------------*)
signature DfaString =
   sig
      val ContentModel2String : (int -> string) -> DfaData.ContentModel -> string
      val Dfa2String          : (int -> string) -> DfaData.Dfa -> string
   end

structure DfaString : DfaString =
    struct 
	open DfaBase UtilString

	fun State2String q = if q=dfaError then "Error" else Int2String q
	   
	fun Info2String Elem2String (q,mt,fst) = String.concat 
	   (State2String q::Bool2xString ("[empty]","") mt
	    ::map (fn (q,a) => " "^Elem2String a^"->"^State2String q) fst)

	fun ContentModel2String Elem2String cm = 
	   case cm
	     of CM_ELEM i => Elem2String i
	      | CM_OPT cm => ContentModel2String Elem2String cm^"?"
	      | CM_REP cm => ContentModel2String Elem2String cm^"*"
	      | CM_PLUS cm => ContentModel2String Elem2String cm^"+"
	      | CM_ALT cms => List2xString ("(","|",")") (ContentModel2String Elem2String) cms
	      | CM_SEQ cms => List2xString ("(",",",")") (ContentModel2String Elem2String) cms
		
	fun CM2String Elem2String =
	    let fun cm2s indent cm =
		case cm
		  of (ELEM a,info) => String.concat 
		     [indent,Elem2String a,"  ",Info2String Elem2String info,"\n"]
		   | (OPT cm',info) => String.concat 
		     [indent,"?  ",Info2String Elem2String info,"\n",cm2s (indent^" ") cm'] 
		   | (REP cm',info) => String.concat 
		     [indent,"*  ",Info2String Elem2String info,"\n",cm2s (indent^" ") cm'] 
		   | (PLUS cm',info) => String.concat 
		     [indent,"+  ",Info2String Elem2String info,"\n",cm2s (indent^" ") cm'] 
		   | (ALT cms,info) => String.concat 
		     (indent^"|  "::Info2String Elem2String info::"\n"
		      ::map (cm2s (indent^" ")) cms) 
		   | (SEQ cms,info) => String.concat 
		     (indent^",  "::Info2String Elem2String info::"\n"
		      ::map (cm2s (indent^" ")) cms) 
	    in cm2s ""
	    end
	 
	fun Row2String Elem2String (lo,hi,tab,fin) =
	   String.concat 
	   (Vector.foldri 
	    (fn (i,q,yet) => if q<0 then yet 
			     else " "::Elem2String (i+lo)::"->"::State2String q::yet)
	    (if fin then [" [Final]"] else nil)
		tab)
	   
	fun Dfa2String Elem2String tab =
	   String.concat 
	   (Vector.foldri
	    (fn (q,row,yet) => State2String q::":"::Row2String Elem2String row::yet)
	    nil tab)
    end










signature SocatParse =
   sig
      val parseSoCat  : Uri.Uri -> CatData.Catalog
   end

functor SocatParse ( structure Params : CatParams ) : SocatParse =
   struct
      structure CatFile = CatFile ( structure Params = Params )
      
      open CatData CatError CatFile Params UniChar UniClasses Uri

      exception SyntaxError of UniChar.Char * CatFile.CatFile
      exception NotFound of UniChar.Char * CatFile.CatFile
      
      val getChar = catGetChar

      fun parseName' (c,f) = 
	 if isName c then let val (cs,cf1) = parseName' (getChar f)
			  in (c::cs,cf1)
			  end
	 else (nil,(c,f))
      fun parseName (c,f) = 
	 if isNms c then let val (cs,cf1) = parseName' (getChar f)
			 in (c::cs,cf1)
			 end
	 else raise NotFound (c,f)

      datatype Keyword = 
	 KW_BASE
       | KW_CATALOG
       | KW_DELEGATE
       | KW_PUBLIC
       | KW_SYSTEM
       | KW_OTHER of UniChar.Data

      fun parseKeyword cf = 
	 let 
	    val (name,cf1) = parseName cf
	    val kw = case name
		       of [0wx42,0wx41,0wx53,0wx45] => KW_BASE
			| [0wx43,0wx41,0wx54,0wx41,0wx4c,0wx4f,0wx47] => KW_CATALOG
			| [0wx44,0wx45,0wx4c,0wx45,0wx47,0wx41,0wx54,0wx45] => KW_DELEGATE
			| [0wx50,0wx55,0wx42,0wx4c,0wx49,0wx43] => KW_PUBLIC
			| [0wx53,0wx59,0wx53,0wx54,0wx45,0wx4d] => KW_SYSTEM
			| _ => KW_OTHER name
	 in (kw,cf1)
	 end

      fun parseSysLit' quote f =
	 let 
	    fun doit text (c,f) = 
	       if c=quote then (text,getChar f)
	       else if c<>0wx0 then doit (c::text) (getChar f)
		    else let val _ = catError(catPos f,ERR_EOF LOC_SYSID)
			 in (text,(c,f))
			 end
	    val (text,cf1) = doit nil (getChar f)
	 in (Data2Uri(rev text),cf1)
	 end
      fun parseSysLit req (c,f) = 
	 if c=0wx22 orelse c=0wx27 then parseSysLit' c f 
	 else if req then let val _ = catError(catPos f,ERR_EXPECTED(EXP_LITERAL,c))
			  in raise SyntaxError (c,f) 
			  end
	      else raise NotFound (c,f)

      fun parsePubLit' quote f =
	 let 
	    fun doit (hadSpace,atStart,text) (c,f) = 
	       case c
		 of 0wx0 => let val _ = catError(catPos f,ERR_EOF LOC_PUBID)
			    in (text,(c,f))
			    end
		  | 0wx0A => doit (true,atStart,text) (getChar f)
		  | 0wx20 => doit (true,atStart,text) (getChar f)
		  | _ => 
		    if c=quote then (text,getChar f)
		    else if isPubid c 
			    then if hadSpace andalso not atStart 
				    then doit (false,false,c::0wx20::text) (getChar f)
				 else doit (false,false,c::text) (getChar f)
			 else let val _ = catError(catPos f,ERR_ILLEGAL_HERE(c,LOC_PUBID))
			      in doit (hadSpace,atStart,text) (getChar f)
			      end
	    val (text,cf1) = doit (false,true,nil) (getChar f)
	 in (Latin2String(rev text),cf1)
	 end
      fun parsePubLit (c,f) = 
	 if c=0wx22 orelse c=0wx27 then parsePubLit' c f 
	 else let val _ = catError(catPos f,ERR_EXPECTED(EXP_LITERAL,c))
	      in raise SyntaxError (c,f) 
	      end

      fun skipComment (c,f) =
	 case c 
	   of 0wx00 => let val _ = catError(catPos f,ERR_EOF LOC_COMMENT)
		       in (c,f)
		       end
	    | 0wx2D => let val (c1,f1) = getChar f
		       in if c1 = 0wx2D then (getChar f1) else skipComment (c1,f1)
		       end
	    | _ => skipComment (getChar f)
      fun skipCopt (c,f) =
	 case c 
	   of 0wx00 => (c,f)
	    | 0wx2D => let val (c1,f1) = getChar f
		       in if c1=0wx2D then skipComment (getChar f1)
			  else let val _ = catError(catPos f,ERR_ILLEGAL_HERE(c,LOC_NOCOMMENT))
			       in (c1,f1)
			       end
		       end
	    | _ => (c,f)

      fun skipScomm req0 cf = 
	 let
	    fun endit req (c,f) =
	       if req andalso c<>0wx00 
		  then let val _ = catError(catPos f,ERR_MISSING_WHITE)
		       in (c,f) 
		       end 
	       else (c,f)
	    fun doit req (c,f) = 
	       case c
		 of 0wx00 => endit req (c,f)
		  | 0wx09 => doit false (getChar f)
		  | 0wx0A => doit false (getChar f)
		  | 0wx20 => doit false (getChar f)
		  | 0wx22 => endit req (c,f)
		  | 0wx27 => endit req (c,f)
		  | 0wx2D => 
		    let val (c1,f1) = getChar f
		    in if c1=0wx2D 
			  then let val _ = if not req then ()
					   else catError(catPos f1,ERR_MISSING_WHITE)
				   val cf1 = skipComment (getChar f1)
			       in doit true cf1
					end
		       else let val _ = catError(catPos f,ERR_ILLEGAL_HERE(c,LOC_NOCOMMENT))
			    in doit req (c1,f1)
			    end
		    end
		  | _ => if isNms c then endit req (c,f) 
			 else let val _ = catError(catPos f,ERR_ILLEGAL_HERE(c,LOC_NOCOMMENT))
			      in doit req (getChar f)
			      end
	 in doit req0 cf
	 end
      
      val skipWS = skipScomm true 
      val skipCommWS = (skipScomm false) o skipCopt 
      val skipWSComm = skipScomm false

      fun skipOther cf =
	 let 
	    val cf1 = skipWS cf
	    val cf2 = let val (_,cf') = parseName cf1
		      in skipWS cf'
		      end 
		   handle NotFound cf => cf

	    fun doit cf =
	       let val (_,cf1) = parseSysLit false cf
	       in doit (skipWS cf1)
	       end
	    handle NotFound(c,f) => (c,f)
	 in
	    (NONE,doit cf2)
	 end

      fun parseBase cf =
	 let 
	    val cf1 = skipWS cf
	    val (lit,cf2) = parseSysLit true cf1
	    val cf3 = skipWS cf2
	 in 
	    (SOME(E_BASE lit),cf3)
	 end

      fun parseExtend cf =
	 let 
	    val cf1 = skipWS cf
	    val (lit,cf2) = parseSysLit true cf1
	    val cf3 = skipWS cf2
	 in 
	    (SOME(E_EXTEND lit),cf3)
	 end

      fun parseDelegate cf =
	 let 
	    val cf1 = skipWS cf
	    val (pub,cf2) = parsePubLit cf1
	    val cf3 = skipWS cf2
	    val (sys,cf4) = parseSysLit true cf3
	    val cf5 = skipWS cf4
	 in 
	    (SOME(E_DELEGATE(pub,sys)),cf5)
	 end

      fun parseRemap cf =
	 let 
	    val cf1 = skipWS cf
	    val (sys0,cf2) = parseSysLit true cf1
	    val cf3 = skipWS cf2
	    val (sys,cf4) = parseSysLit true cf3
	    val cf5 = skipWS cf4
	 in 
	    (SOME(E_REMAP(sys0,sys)),cf5)
	 end

      fun parseMap cf =
	 let 
	    val cf1 = skipWS cf
	    val (pub,cf2) = parsePubLit cf1
	    val cf3 = skipWS cf2
	    val (sys,cf4) = parseSysLit true cf3
	    val cf5 = skipWS cf4
	 in 
	    (SOME(E_MAP(pub,sys)),cf5)
	 end

      fun recover cf =
	 let 
	    fun do_lit q (c,f) = 
	       if c=0wx00 then (c,f) 
	       else if c=q then getChar f 
		    else do_lit q (getChar f)
	    fun do_com (c,f) = 
	       case c
		  of 0wx00 => (c,f)
		   | 0wx2D => let val (c1,f1) = getChar f
			      in if c1=0wx2D then getChar f1
				 else do_com (c1,f1)
			      end
		   | _ => do_com (getChar f)
	    fun doit (c,f) =
	       case c
		 of 0wx00 => (c,f)
		  | 0wx22 => doit (do_lit c (getChar f)) 
		  | 0wx27 => doit (do_lit c (getChar f)) 
		  | 0wx2D => let val (c1,f1) = getChar f
			     in if c1=0wx2D then doit (do_com (getChar f1))
				else doit (c1,f1)
			     end
		  | _ => if isNms c then (c,f)
			 else doit (getChar f)
	 in doit cf
	 end
      
      fun parseEntry (cf as (c,f)) = 
	 let val (kw,cf1) = parseKeyword cf handle NotFound cf => raise SyntaxError cf
	 in case kw
	      of KW_BASE => parseBase cf1
	       | KW_CATALOG => parseExtend cf1
	       | KW_DELEGATE => parseDelegate cf1
	       | KW_SYSTEM => parseRemap cf1
	       | KW_PUBLIC => parseMap cf1
	       | KW_OTHER _ => skipOther cf1
	 end
      handle SyntaxError cf => (NONE,recover cf)

      fun parseDocument cf =
	 let 
	    fun doit (c,f) =
	       if c=0wx0 then nil before catCloseFile f
	       else let val (opt,cf1) = parseEntry (c,f)
			val entries = doit cf1
		    in case opt
			 of NONE => entries
			  | SOME entry => entry::entries
		    end

	    val cf1 = skipCommWS cf
	 in 
	    doit cf1
	 end

      fun parseSoCat uri = 
	 let 
	    val f = catOpenFile uri
	    val cf1 = getChar f
	 in 
	    (uri,parseDocument cf1)
	 end
   end

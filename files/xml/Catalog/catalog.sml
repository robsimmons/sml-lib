








signature Catalog =
   sig
      val resolveExtId : string option * (Uri.Uri * Uri.Uri) option -> Uri.Uri option
   end

functor Catalog ( structure Params : CatParams ) : Catalog = 
   struct
      structure CatParse = CatParse ( structure Params = Params )

      open CatData CatParse Params Uri UriDict

      val catDict = makeDict("catalog",6,NONE:Catalog option)
	 
      fun getCatalog uri = 
	 let val idx = getIndex(catDict,uri)
	 in case getByIndex(catDict,idx)
	      of SOME cat => cat
	       | NONE => let val cat = parseCatalog uri
			     val _ = setByIndex(catDict,idx,SOME cat)
			 in cat
			 end
	 end

      datatype SearchType =  
	  SYS of Uri
	| PUB of string
      datatype SearchResult = 
	  FOUND of Uri * Uri
	| NOTFOUND of Uri list
	  
      fun searchId id =
	 let
	    fun searchOne (base,other) nil = NOTFOUND other
	      | searchOne (base,other) (entry::entries) = 
	       case entry
		 of E_BASE path => 
		    let val newBase = uriJoin(base,path)
		    in searchOne (newBase,other) entries
		    end
		  | E_EXTEND path => 
		    let val fullPath = uriJoin(base,path)
		    in searchOne (base,fullPath::other) entries 
		    end
		  | E_DELEGATE(prefix,path) => 
		    (case id
		       of PUB pid => if String.isPrefix prefix pid 
					then let val fullPath = uriJoin(base,path)
					     in searchOne (base,fullPath::other) entries
					     end
				     else searchOne (base,other) entries 
			| SYS _ => searchOne (base,other) entries)
		  | E_MAP(pubid,path) => 
		    (case id 
		       of PUB pid => if pubid=pid then FOUND (base,path)
				     else searchOne (base,other) entries
			| _ => searchOne (base,other) entries)
		  | E_REMAP(sysid,path) => 
	            (case id
		       of SYS sid => if sysid=sid then FOUND(base,path)
				     else searchOne (base,other) entries 
			| _ => searchOne (base,other) entries)
			
	    fun searchLevel other nil = NOTFOUND(rev other)
	      | searchLevel other (fname::fnames) = 
	       let 
		  val (base,entries) = getCatalog fname
	       in 
		  case searchOne (base,other) entries 
		    of FOUND bp => FOUND bp
		     | NOTFOUND other' => searchLevel other' fnames
	       end

	    fun searchAll fnames = 
	       if null fnames then NONE
	       else case searchLevel nil fnames
		      of FOUND bp => SOME bp
		       | NOTFOUND other => searchAll other

	    val fnames = !O_CATALOG_FILES
	 in 
	    case id
	      of PUB _ => searchAll fnames
	       | SYS _ => if !O_SUPPORT_REMAP then searchAll fnames else NONE
	 end

      fun resolveExtId (pub,sys) =
	 let 
	    fun resolvePubCat () = 
	       case pub
		 of NONE => NONE
		  | SOME id => case searchId (PUB id)
				 of NONE => NONE
				  | SOME(base,sysid) => case searchId (SYS sysid)
							  of NONE => SOME(base,sysid)
							   | new => new
							    
	    fun resolveSysCat () = 
	       case sys 
		 of NONE => NONE
		  | SOME(base,id) => searchId (SYS id)

	    fun resolveCat () =
	       if !O_PREFER_SYSID 
		  then case resolveSysCat ()
			 of NONE => resolvePubCat ()
			  | found => found
	       else case resolvePubCat ()
		      of NONE => resolveSysCat ()
		       | found => found

	    fun resolve () =
	       if !O_PREFER_CATALOG 
		  then case resolveCat ()
			 of NONE => (case sys
				       of NONE => NONE
					| SOME(base,id) => SOME(base,id))
			  | found => found
	       else case sys 
		      of NONE => resolvePubCat ()
		       | SOME(base,id) => SOME(base,id)
	 in 
	    if null (!O_CATALOG_FILES) 
	       then case sys 
		      of NONE => NONE
		       | SOME(base,id) => SOME (uriJoin (base,id))
	    else case resolve ()
		   of NONE => NONE
		    | SOME bp => SOME (uriJoin bp)
	 end
   end








functor ResolveCatalog ( structure Params : CatParams ) : Resolve = 
   struct
      structure Catalog = Catalog ( structure Params = Params )
	 
      open Base Errors

      fun resolveExtId (id as EXTID(pub,sys)) =
	 let val pub1 = case pub
			  of NONE => NONE
			   | SOME (str,_) => SOME str
	     val sys1 = case sys
			  of NONE => NONE 
			   | SOME (base,file,_) => SOME(base,file)
	 in case Catalog.resolveExtId (pub1,sys1)
	      of NONE => raise NoSuchFile ("","Could not generate system identifier")
	       | SOME uri => uri
	 end
   end





signature Resolve = 
   sig
      val resolveExtId : Base.ExternalId -> Uri.Uri
   end

structure ResolveNull : Resolve = 
   struct
      open Base Errors Uri

      fun resolveExtId (EXTID(_,sys)) =
	 case sys
	   of NONE => raise NoSuchFile ("","Could not generate system identifier")
	    | SOME (base,file,_) => uriJoin(base,file)
   end

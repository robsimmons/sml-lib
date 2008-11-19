signature CatOptions =
   sig
      val O_CATALOG_FILES  : Uri.Uri list ref
      val O_PREFER_SOCAT   : bool ref
      val O_PREFER_SYSID   : bool ref
      val O_PREFER_CATALOG : bool ref
      val O_SUPPORT_REMAP  : bool ref
      val O_CATALOG_ENC    : Encoding.Encoding ref
	 
      val setCatalogDefaults : unit -> unit
      val setCatalogOptions  : Options.Option list * (string -> unit) -> Options.Option list

      val catalogUsage       : Options.Usage
   end

functor CatOptions () : CatOptions =
   struct
      open Encoding Options Uri 

      val O_CATALOG_FILES  = ref nil: Uri list ref
      val O_PREFER_SOCAT   = ref false
      val O_PREFER_SYSID   = ref false
      val O_PREFER_CATALOG = ref true
      val O_SUPPORT_REMAP  = ref true
      val O_CATALOG_ENC    = ref LATIN1

      fun setCatalogDefaults() = 
	 let 
	    val _ = O_CATALOG_FILES  := nil
	    val _ = O_PREFER_SOCAT   := false
	    val _ = O_PREFER_SYSID   := false
	    val _ = O_PREFER_CATALOG := true
	    val _ = O_SUPPORT_REMAP  := true
	    val _ = O_CATALOG_ENC    := LATIN1
	 in ()
	 end
      
      val catalogUsage = 
	 [U_ITEM(["-C <url>","--catalog=<url>"],"Use catalog <url>"),
          U_ITEM(["--catalog-syntax=(soc|xml)"],"Default syntax for catalogs (xml)"),
          U_ITEM(["--catalog-encoding=<enc>"],"Default encoding for Socat catalogs (LATIN1)"), 
	  U_ITEM(["--catalog-remap=[(yes|no)]"],"Support remapping of system identifiers (yes)"),
          U_ITEM(["--catalog-priority=(map|remap|sys)"],"Resolving strategy in catalogs (map)")
	  ]

      fun setCatalogOptions (opts,doError) = 
	 let 
	    val catalogs = ref nil:string list ref

	    fun hasNoArg key = "option "^key^" has no argument"
	    fun mustHave key = String.concat ["option ",key," must have an argument"]
	    fun mustBe(key,what) = String.concat ["the argument to --",key," must be ",what]

	    val yesNo       = "'yes' or 'no'"
	    val mapRemapSys = "'map', 'remap' or 'sys'"
	    val encName     = "'ascii', 'latin1', 'utf8' or 'utf16'"
	    val syntaxName  = "'soc' or 'xml'"

	    fun do_catalog valOpt =
	       case valOpt 
		 of NONE => doError(mustHave "--catalog")
		  | SOME s => catalogs := s::(!catalogs)

	    fun do_prio valOpt =
	       let fun set(cat,sys) = (O_PREFER_CATALOG := cat; O_PREFER_SYSID := sys) 
	       in case valOpt 
		    of NONE => doError(mustHave "--catalog-priority")
		     | SOME "map" => set(true,false)
		     | SOME "remap" => set(true,true)
		     | SOME "sys" => set(false,true)
		     | SOME s => doError(mustBe("catalog-priority",mapRemapSys))
	       end

	    fun do_enc valOpt =
	       case valOpt 
		 of NONE => doError(mustHave "--catalog-encoding")
		  | SOME s => case isEncoding s 
				of NOENC => doError("unsupported encoding "^s)
				 | enc => O_CATALOG_ENC := enc

	    fun do_remap valOpt =
	       case valOpt 
		 of NONE => doError(mustHave "--catalog-remap")
		  | SOME "no" => O_SUPPORT_REMAP := false
		  | SOME "yes" => O_SUPPORT_REMAP := true
		  | SOME s => doError(mustBe("catalog-remap",yesNo))

	    fun do_syntax valOpt =
	       case valOpt 
		 of NONE => doError(mustHave "--catalog-syntax")
		  | SOME "soc" => O_PREFER_SOCAT := true
		  | SOME "xml" => O_PREFER_SOCAT := false
		  | SOME s => doError(mustBe("catalog-remap",syntaxName))

	    fun do_long(key,valOpt) = 
	       case key
		 of "catalog" => true before do_catalog valOpt
		  | "catalog-remap" => true before do_remap valOpt
		  | "catalog-syntax" => true before do_syntax valOpt
		  | "catalog-encoding" => true before do_enc valOpt
		  | "catalog-priority" => true before do_prio valOpt
		  | _ => false

	    fun do_short cs opts =
	       case cs 
		 of nil => doit opts
		  | [#"C"] => 
		    (case opts 
		       of OPT_STRING s::opts1 => (catalogs := s::(!catalogs);
						  doit opts1)
			| _ => let val _ = doError (mustHave "-C")
			       in doit opts
			       end)
  		  | cs => 
		    let val cs1 = List.filter
		       (fn c => if #"C"<>c then true
				else false before doError (mustHave "-C")) cs
		    in if null cs1 then doit opts else (OPT_SHORT cs1)::doit opts
		    end
		 
	    and doit nil = nil
	      | doit (opt::opts) =
	       case opt
		 of OPT_NOOPT => opts
		  | OPT_LONG(key,value) => if do_long(key,value) then doit opts 
					   else opt::doit opts
		  | OPT_SHORT cs => do_short cs opts
		  | OPT_NEG cs => opt::doit opts
		  | OPT_STRING s => opt::doit opts
	 
	    val opts1 = doit opts
	    val uris = map String2Uri (!catalogs)
	    val _ = O_CATALOG_FILES := uris
	 in opts1
	 end
   end

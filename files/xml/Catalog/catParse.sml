signature CatParse =
   sig
      val parseCatalog  : Uri.Uri -> CatData.Catalog
   end

functor CatParse (structure Params : CatParams) : CatParse = 
   struct
      structure SocatParse = SocatParse (structure Params = Params) 

      structure ParserOptions = 
	 struct
	    structure Options = ParserOptions()
	    open Options
	       
	    local 
	       fun setDefaults() = 
		  let 
		     val _ = setParserDefaults()
			
		     val _ = O_WARN_MULT_ENUM      := false
		     val _ = O_WARN_XML_DECL       := false
		     val _ = O_WARN_ATT_NO_ELEM    := false
		     val _ = O_WARN_MULT_ENT_DECL  := false
		     val _ = O_WARN_MULT_NOT_DECL  := false
		     val _ = O_WARN_MULT_ATT_DEF   := false
		     val _ = O_WARN_MULT_ATT_DECL  := false
		     val _ = O_WARN_SHOULD_DECLARE := false
			
		     val _ = O_VALIDATE            := false
		     val _ = O_COMPATIBILITY       := false
		     val _ = O_INTEROPERABILITY    := false
			
		     val _ = O_INCLUDE_EXT_PARSED  := true
		  in ()
		  end
	    in 
	       val setParserDefaults = setDefaults
	    end

	 end
      structure CatHooks = CatHooks (structure Params = Params
				     structure Dtd    = CatDtd)
      structure Parse = Parse (structure Dtd = CatDtd
			       structure Hooks = CatHooks
			       structure Resolve = ResolveNull
			       structure ParserOptions = ParserOptions)
	 
      open CatHooks CatDtd Parse ParserOptions SocatParse Uri

      fun parseXmlCat uri = 
	 let 
	    val _ = setParserDefaults()
	    val dtd = initDtdTables()
	    val items = parseDocument (SOME uri) (SOME dtd) (initCatHooks dtd)
	 in 
	    (uri,items)
	 end

      fun isSocatSuffix x = x="soc" orelse x="SOC"
      fun isXmlSuffix x = x="xml" orelse x="XML"

      fun parseCatalog uri = 
	 let val suffix = uriSuffix uri
	 in if isSocatSuffix suffix then parseSoCat uri 
 	    else (if isXmlSuffix suffix then parseXmlCat uri
		  else (if !O_PREFER_SOCAT then parseSoCat uri 
			else parseXmlCat uri))
	 end
   end

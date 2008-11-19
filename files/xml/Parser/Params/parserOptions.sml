(*--------------------------------------------------------------------------*)
(* Structure: ParserOptions                                                 *)
(*                                                                          *)
(* Depends on: none                                                         *)
(*--------------------------------------------------------------------------*)
signature ParserOptions =
   sig
      structure DfaOptions : DfaOptions

      val O_CHECK_ISO639        : bool ref
      val O_CHECK_LANGID        : bool ref
      val O_CHECK_PREDEFINED    : bool ref
      val O_CHECK_RESERVED      : bool ref
      val O_CHECK_VERSION       : bool ref

      val O_WARN_MULT_ENUM      : bool ref
      val O_WARN_XML_DECL       : bool ref
      val O_WARN_ATT_NO_ELEM    : bool ref
      val O_WARN_MULT_ENT_DECL  : bool ref
      val O_WARN_MULT_NOT_DECL  : bool ref
      val O_WARN_MULT_ATT_DEF   : bool ref
      val O_WARN_MULT_ATT_DECL  : bool ref
      val O_WARN_SHOULD_DECLARE : bool ref
      val O_WARN_NON_ASCII_URI  : bool ref

      val O_ERROR_MINIMIZE      : bool ref

      val O_VALIDATE            : bool ref
      val O_COMPATIBILITY       : bool ref
      val O_INTEROPERABILITY    : bool ref

      val O_INCLUDE_EXT_PARSED  : bool ref
      val O_INCLUDE_PARAM_ENTS  : bool ref

      val setParserDefaults     : unit -> unit
      val setParserOptions      : Options.Option list * (string -> unit) -> Options.Option list

      val parserUsage           : Options.Usage
   end

functor ParserOptions () : ParserOptions =
   struct
      structure DfaOptions = DfaOptions ()

      open DfaOptions Options UtilInt UtilList 

      val O_CHECK_VERSION = ref true (* check for conforming xml version?   *)
      val O_CHECK_ISO639 = ref true (* check whether a two-letter LangCode  *)
	                            (* is acording to ISO 639?              *)
      val O_CHECK_LANGID = ref true (* check whether a LangCode fullfills   *)
	                            (* IETF RFC 1766?                       *)
      val O_CHECK_RESERVED = ref false(* check for names starting with xml? *)
      val O_CHECK_PREDEFINED = ref true (* check declarations of predefined *)
      val O_WARN_MULT_ENUM = ref true  (* check whether a token occurs      *)
	                               (* twice in the enumerated attribute *)
                                       (* types of the same element         *)
      val O_WARN_XML_DECL = ref false (* warn if the XML decl is missing?   *)

      val O_WARN_ATT_NO_ELEM   = ref true (* warn for undeclared elements   *)
	                                  (* in att def list declarations?  *)

      val O_WARN_MULT_ENT_DECL  = ref true (* warn about redefined entities *)
      val O_WARN_MULT_NOT_DECL  = ref true (* warn about redefined notations*)
      val O_WARN_SHOULD_DECLARE = ref true (* warn if predefined entities   *)
	                                   (* are not declared in the dtd   *)

      val O_WARN_MULT_ATT_DEF  = ref true (* warn if an attributes is defd  *)
	                                  (* twice for the same element?    *) 
      val O_WARN_MULT_ATT_DECL = ref true (* warn if there are multiple att *)
	                                  (* def lists for one element?     *)
      val O_WARN_NON_ASCII_URI = ref true (* warn about non-ascii chars in  *)
	                                  (* system identifiers?            *)

      val O_ERROR_MINIMIZE  = ref true (* try to avoid repeating errors?    *)

      val O_VALIDATE         = ref true 
      val O_COMPATIBILITY    = ref true
      val O_INTEROPERABILITY = ref false

      val O_INCLUDE_EXT_PARSED = ref false
      val O_INCLUDE_PARAM_ENTS = ref false

      fun setParserDefaults() = 
	 let 
	    val _ = setDfaDefaults()

	    val _ = O_CHECK_ISO639        := false
	    val _ = O_CHECK_LANGID        := false
	    val _ = O_CHECK_PREDEFINED    := true
	    val _ = O_CHECK_RESERVED      := false
	    val _ = O_CHECK_VERSION       := true
	    
	    val _ = O_WARN_MULT_ENUM      := true
	    val _ = O_WARN_XML_DECL       := false
	    val _ = O_WARN_ATT_NO_ELEM    := false
	    val _ = O_WARN_MULT_ENT_DECL  := false
	    val _ = O_WARN_MULT_NOT_DECL  := false
	    val _ = O_WARN_MULT_ATT_DEF   := false
	    val _ = O_WARN_MULT_ATT_DECL  := false
	    val _ = O_WARN_SHOULD_DECLARE := true
	    val _ = O_WARN_NON_ASCII_URI  := true

	    val _ = O_VALIDATE            := true
	    val _ = O_COMPATIBILITY       := true
	    val _ = O_INTEROPERABILITY    := false
	    
	    val _ = O_ERROR_MINIMIZE      := true

	    val _ = O_INCLUDE_EXT_PARSED  := false
            val _ = O_INCLUDE_PARAM_ENTS  := false
	 in ()
	 end

      val parserUsage = 
	 [U_ITEM(["-[n]v","--validate[=(yes|no)]"],"Turn on or off validation (yes)"),
	  U_ITEM(["-[n]c","--compat[=(yes|no)]","--compatibility[=(yes|no)]"],
                 "Turn on or off compatibility checking (yes)"),
	  U_ITEM(["-[n]i","--interop[=(yes|no)]","--interoperability[=(yes|no)]"],
                 "Turn on or off interoperability checking (no)"),
          U_SEP,
	  U_ITEM(["--few-errors[=(yes|no)]"],"Report fewer errors (no)"),
	  U_ITEM(["--check-reserved[=(yes|no)]"],
                 "Checking for reserved names (no)"),
	  U_ITEM(["--check-predef[=(yes|no)]","--check-predefined[=(yes|no)]"],
                 "Check declaration of predefined entities (yes)"),
	  U_ITEM(["--check-lang-id[=(yes|no)]"],"Checking language identifiers (no)"),
	  U_ITEM(["--check-iso639[=(yes|no)]"],"Check ISO 639 language codes (no)"),
	  U_ITEM(["--check-xml-version[=(yes|no)]"], "Check XML version number (yes)"),
          U_SEP,
	  U_ITEM(["--warn-xml-decl[=(yes|no)]"],"Warn if there is no XML declaration (no)"),
	  U_ITEM(["--warn-att-elem[=(yes|no)]"],
                 "Warn about attlist declarations for undeclared elements (no)"),
	  U_ITEM(["--warn-predefined[=(yes|no)]"],
                 "Warn if the predefined entities are not declared (no)"),
	  U_ITEM(["--warn-mult-decl[=<arg>]"],"Warn about multiple declarations (none)"),
          U_ITEM(["--warn-uri[=(yes|no)]"],"Warn about non-ASCII characters in URIs (yes)"),
          U_ITEM(["--warn[=all]"],"Warn about nearly everything"),
          U_ITEM(["--warn=none"],"Do not print warnings"),
	  U_SEP,
	  U_ITEM(["--include-ext[=(yes|no)]","--include-external[=(yes|no)]"],
                 "Include external entities in non-validating mode (no)"),
	  U_ITEM(["--include-par[=(yes|no)]","--include-parameter[=(yes|no)]"],
                 "Include parameter entities and external subset in "^
                 "non-validating mode (no)"),
	  U_SEP]
	 @dfaUsage

      fun setParserOptions(opts,doError) = 
	 let 
	    datatype What = ATT | ATTLIST | ENT | NOT 
	       
	    exception Failed of string option

	    fun getNat str = 
	       if str="" then raise Failed NONE
	       else let val cs = String.explode str
		    in foldl (fn (c,n) => if #"0">c orelse #"9"<c then raise Failed NONE
					  else 10*n+ord c-48) 0 cs
		       handle Overflow => raise Failed
			  (SOME("number "^str^" is too large for this system"))
		    end
		 
	    val allNone = "'all' or 'none'"
	    val yesNo = "'yes' or 'no'"
	    val yesNoWhat = "'yes', 'no' or a list of 'att', 'attlist', 'ent' and 'not'"
	    fun errorMustBe(key,what) = doError
	       (String.concat ["the argument to option --",key," must be ",what])
	    fun errorNoArg key = doError
	       (String.concat ["option --",key," has no argument"])
	       
	    fun do_mult_decl(key,valOpt) =
	       let 
		  val all = [ATT,ATTLIST,ENT,NOT]
		  fun setFlags whats = app (fn (what,flag) => flag := member what whats)
		     [(ATT,O_WARN_MULT_ATT_DEF),(ATTLIST,O_WARN_MULT_ATT_DECL),
		      (ENT,O_WARN_MULT_ENT_DECL),(NOT,O_WARN_MULT_NOT_DECL)]
	       in case valOpt 
		    of NONE => setFlags all
		     | SOME "yes" => setFlags all
		     | SOME "no" => setFlags nil
		     | SOME s => let val fields = String.fields (fn c => #","=c) s
				     val whats = map 
					(fn s => case s
						   of "att" => ATT
						    | "attlist" => ATTLIST
						    | "ent" => ENT
						    | "not" => NOT
						    | _ => raise Failed NONE) fields
				 in setFlags whats
				 end
			      handle Failed _ => errorMustBe(key,yesNoWhat)
	       end

	    fun do_noarg(key,valOpt,flag) = 
	       case valOpt 
		 of NONE => flag := true
		  | SOME _ => errorNoArg key

	    fun do_yesno(key,valOpt,flag) =
	       case valOpt 
		 of NONE => flag := true
		  | SOME "yes" => flag := true
		  | SOME "no" => flag := false
		  | SOME s => errorMustBe(key,yesNo)

	    fun do_num(key,valOpt,flag) =
	       case valOpt 
		 of NONE => errorMustBe(key,"a number")
		  | SOME s => flag := getNat s
		    handle Failed NONE => errorMustBe(key,"a number")
			 | Failed (SOME s) => doError s

	    fun do_warn(key,valOpt) =
	       let val all = [O_WARN_MULT_ENUM,O_WARN_ATT_NO_ELEM,   
			      O_WARN_MULT_ENT_DECL,O_WARN_MULT_NOT_DECL,O_WARN_MULT_ATT_DEF,
			      O_WARN_MULT_ATT_DECL,O_WARN_SHOULD_DECLARE,O_WARN_XML_DECL]
		  fun setFlags value = app (fn flag => flag := value) all
	       in case valOpt
		    of NONE => setFlags true
		     | SOME "all" => setFlags true
		     | SOME "none" => setFlags false
		     | SOME _ => errorMustBe(key,allNone)
	       end

	    fun do_long(key,valOpt) = 
	       case key
		 of "validate" => true before do_yesno(key,valOpt,O_VALIDATE)
		  | "compat" => true before do_yesno(key,valOpt,O_COMPATIBILITY)
		  | "compatibility" => true before do_yesno(key,valOpt,O_COMPATIBILITY)
		  | "interop" => true before do_yesno(key,valOpt,O_INTEROPERABILITY)
		  | "interoperability" => true before do_yesno(key,valOpt,O_INTEROPERABILITY)

		  | "few-errors" => true before do_yesno(key,valOpt,O_ERROR_MINIMIZE)
		    
		  | "check-reserved" => true before do_yesno(key,valOpt,O_CHECK_RESERVED)
		  | "check-predef" => true before do_yesno(key,valOpt,O_CHECK_PREDEFINED)
		  | "check-predefined" => true before do_yesno(key,valOpt,O_CHECK_PREDEFINED)
		  | "check-lang-id" => true before do_yesno(key,valOpt,O_CHECK_LANGID)
		  | "check-iso639" => true before do_yesno(key,valOpt,O_CHECK_ISO639)
		  | "check-xml-version" => true before do_yesno(key,valOpt,O_CHECK_VERSION)
		    
		  | "warn" => true before do_warn(key,valOpt)
		  | "warn-xml-decl" => true before do_yesno(key,valOpt,O_WARN_XML_DECL)
		  | "warn-att-elem" => true before do_yesno(key,valOpt,O_WARN_ATT_NO_ELEM)
		  | "warn-predefined" => true before do_yesno(key,valOpt,O_WARN_SHOULD_DECLARE)
		  | "warn-mult-decl" => true before do_mult_decl(key,valOpt)
		  | "warn-uri" => true before do_yesno(key,valOpt,O_WARN_NON_ASCII_URI)
		    
		  | "include-ext" => true before do_yesno(key,valOpt,O_INCLUDE_EXT_PARSED)
		  | "include-external" => true before do_yesno(key,valOpt,O_INCLUDE_EXT_PARSED)
		  | "include-par" => true before do_yesno(key,valOpt,O_INCLUDE_PARAM_ENTS)
		  | "include-parameter" => true before do_yesno(key,valOpt,O_INCLUDE_PARAM_ENTS)
		    
		  | _ => false

	    fun do_short cs = 
	       let fun doOne c = 
		  case c
		    of #"v" => false before O_VALIDATE := true
		     | #"c" => false before O_COMPATIBILITY := true
		     | #"i" => false before O_INTEROPERABILITY := true
		     | _ => true
	       in List.filter doOne cs
	       end 

	    fun do_neg cs = 
	       let fun doOne c = 
		  case c
		    of #"v" => false before O_VALIDATE := false
		     | #"c" => false before O_COMPATIBILITY := false
		     | #"i" => false before O_INTEROPERABILITY := false
		     | _ => true
	       in List.filter doOne cs
	       end 

	    and doit nil = nil
	      | doit (opt::opts) =
	       case opt
		 of OPT_NOOPT => opts
		  | OPT_LONG(key,value) => if do_long(key,value) then doit opts 
					   else opt::doit opts
		  | OPT_SHORT cs => (case do_short cs
				       of nil => doit opts
					| rest => OPT_SHORT rest::doit opts)
		  | OPT_NEG cs => (case do_neg cs
				       of nil => doit opts
					| rest => OPT_NEG rest::doit opts)
		  | OPT_STRING s => opt::doit opts
	 
	    val opts1 = setDfaOptions (opts,doError)
	 in 
	    doit opts1
	 end
   end

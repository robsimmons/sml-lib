







signature ErrorMessage =
   sig
      val errorMessage   : ErrorData.Error -> string list
      val warningMessage : ErrorData.Warning -> string list
   end

structure ErrorMessage : ErrorMessage =
   struct
      open 
	 Decode
	 UtilString
	 ErrorData ErrorString

      val quoteChar0  = quoteErrorChar0
      val quoteChar   = quoteErrorChar
      val quoteData   = quoteErrorData
      val quoteString = quoteErrorString
      val quoteVector = quoteErrorVector

      fun errorMessage err =
	 case err
	   (* syntax errors *)
	   of ERR_EMPTY loc => ["Empty",Location2String loc]
	    | ERR_ENDED_BY_EE loc => [toUpperFirst (Location2String loc),"ended by entity end"]
	    | ERR_EXPECTED (exp,found) => 
	      ["Expected",Expected2String exp,"but found",Found2String found]	      
	    | ERR_MISSING_WHITE => ["Missing white space"]
	    | ERR_NON_XML_CHAR c => ["Non-XML character",quoteChar0 c]
	    | ERR_NON_XML_CHARREF c => ["Reference to non-XML character",quoteChar0 c]
	    | ERR_MUST_CHARREF c => ["Control character",quoteChar0 c,
				     "must appear in XML 1.1 only as character reference"]


	    (* other well-formedness errors *)
	    | ERR_CANT_PARSE loc => ["Cannot parse",Location2String loc]
	    | ERR_ELEM_ENT_NESTING elem => 	       
	      ["The first and last character of element",quoteData elem,
	       "are in different entities"]  
	    | ERR_ELEM_TYPE_MATCH (elem,other) => 
	      ["Element",quoteData elem,"was ended by an end-tag for",quoteData other]
	    | ERR_IGNORED_END_TAG(elem,other) =>
	      ["An end-tag for element type",quoteData other,"is not allowed in the",
	       "content of element",quoteData elem]
	    | ERR_OMITTED_END_TAG elem =>
	      ["Element",quoteData elem,"has no end-tag"]
	    | ERR_ENDED_IN_PROLOG => ["Document entity ended in prolog"]
	    | ERR_FORBIDDEN_HERE(what,loc) => 
	      [AnItem2String what,"is not allowed",InLocation2String loc]
	    | ERR_ILLEGAL_ENTITY(what,ent,loc) => 
	      ["Reference to",EntityClass2String what,"entity",quoteData ent,InLocation2String loc]
	    | ERR_MULTIPLE_DTD => ["Repeated document type declaration"]
	    | ERR_MULT_ATT_SPEC att => 
	      ["A value for attribute",quoteData att,"was already specified in this tag"]
	    | ERR_RECURSIVE_ENTITY(what,ent) =>
	      ["Reference to",EntityClass2String what,"entity",quoteData ent,
	       "that is already open"]
	    | ERR_UNDEC_ENTITY(what,ent) => 
	      ["Reference to undeclared",EntityClass2String what,"entity",quoteData ent]
	      
	    (* validity errors concerning attributes *)
	    | ERR_AT_LEAST_ONE what => ["At least one",Item2String what,"must be specified"] 
	    | ERR_AT_MOST_ONE what => ["Only one",Item2String what,"may be specified"] 
	    | ERR_ATT_IS_NOT(cs,what) => [quoteData cs,"is not",AnItem2String what] 
	    | ERR_EXACTLY_ONE what => [toUpperFirst (AnItem2String what),"must be specified"]
	    | ERR_FIXED_VALUE(att,value,fixed) => 
	      ["Attribute",quoteData att,"has the value",quoteVector value,
	       "but was declared with a fixed default value of",quoteVector fixed]
	    | ERR_ID_DEFAULT => 
	      ["An ID attribute must have a default value of #IMPLIED or #REQUIRED"]
	    | ERR_MISSING_ATT att => 
	      ["No value was specified for required attribute",quoteData att] 
	    | ERR_MULT_ID_ELEM elem => 
	      ["Element type",quoteData elem,"already has an ID attribute"]
	    | ERR_MUST_BE_AMONG (what,x,ys) => 
	      [toUpperFirst (Item2String what),quoteData x,"is none of",
	       List2xString ("",",","") quoteData ys]
	    | ERR_MUST_BE_UNPARSED (name,loc) => 
	      [quoteData name,InLocation2String loc,"is not the name of an unparsed entity"]
	    | ERR_REPEATED_ID name => 
	      ["ID name",quoteData name,"already occurred as an attribute value"]
	    | ERR_UNDECL_ATT(att,elem) => 
	      ["Attribute",quoteData att,"was not declared for element type",quoteData elem]
	    | ERR_UNDECL_ID(name,refs) => 
	      (if null refs then ["Reference to non-existent ID",quoteData name]
	       else ["Reference to non-existent ID",quoteData name,
		     "(also referenced at",List2xString ("",", ",")") Position2String refs])

	    (* validity errors concerning elements *)
	    | ERR_BAD_ELEM (curr,elem) => 
	      ["Element type",quoteData elem,"not allowed at this point",
	       "in the content of element",quoteData curr] 
	    | ERR_ELEM_CONTENT what => 
	      [toUpperFirst (AnItem2String what),"is not allowed in element content"] 
	    | ERR_EMPTY_TAG elem => 	 
	      ["Empty-element tag for element type",quoteData elem,
	       "whose content model requires non-empty content"]
	    | ERR_ENDED_EARLY elem =>
	      ["Element",quoteData elem,"ended before its content was completed"]
	    | ERR_MULT_MIXED elem =>
	      ["Element type",quoteData elem,"already occurred in this mixed-content declaration"]
	    | ERR_NONEMPTY elem =>  
	      ["The end-tag for element",quoteData elem,"with declared EMPTY content",
	       "must follow immediately after its start-tag"]
	    | ERR_REDEC_ELEM elem => ["Element type",quoteData elem,"was already declared"]
	    | ERR_ROOT_ELEM (dec,root) => 
	      ["Document element",quoteData root,"does not match the name",
	       quoteData dec,"in the document type declaration"]

	    (* other validity errors *)
	    | ERR_DECL_ENT_NESTING loc => 
	      ["The first and last character of this",Location2String loc,
	       "are not in the same entity replacement text"]  
	    | ERR_EE_INT_SUBSET =>
	      ["An entity end is not allowed in a declaration in the internal subset"]
	    | ERR_GROUP_ENT_NESTING loc => 
	      ["The opening and closing parentheses of this",Location2String loc,
	       "are not in the same entity replacement text"]  
	    | ERR_NO_DTD => 
	      ["There is no document type declaration. Switching to semi-validating mode",
	       "(will not check for declaredness of entities, elements, etc.)"]
	    | ERR_STANDALONE_DEF att => 
	      ["Externally declared attribute",quoteData att,"was defaulted,",
	       "although the standalone declaration is",quoteString "yes"]
	    | ERR_STANDALONE_ELEM elem => 
	      ["White space occurred in the content of externally declared",
	       "element",quoteData elem,"with declared element content",
	       "although the standalone declaration is",quoteString "yes"]
	    | ERR_STANDALONE_ENT(what,ent) => 
	      ["Reference to externally declared",EntityClass2String what,"entity",
	       quoteData ent^",","although the standalone declaration is",quoteString "yes"]
	    | ERR_STANDALONE_NORM att => 
	      ["The value for externally declared attribute",
	       quoteData att,"was changed as a result of normalization,",
	       "although the standalone declaration is",quoteString "yes"]
	    | ERR_UNDECLARED (what,x,loc) =>
	      ["Undeclared",Item2String what,quoteData x,InLocation2String loc]

	    (* miscellaneous errors *)
	    | ERR_DECL_PREDEF(ent,def) => 
	      ["General entity",quoteData ent,"must be declared as internal entity",
	       "with replacement text",quoteVector def]
	    | ERR_NO_SUCH_FILE(f,msg) => ["Could not open file",quoteString f,"("^msg^")"]
	    | ERR_RESERVED(name,what) =>
	      [quoteData name,"is reserved for standardization and therefore not allowed as",
	       AnItem2String what]
	    | ERR_VERSION version => 
	      ["XML version",quoteString version,"is not supported"]
	    | ERR_XML_SPACE => 
	      ["Attribute",quoteString "xml:space","must be given an enumeration type",
	       "whose values are one or both of",quoteString "default","and",quoteString "preserve"]

	    (* compatibility errors *)
	    | ERR_AMBIGUOUS(a,n1,n2) => 	       
	      ["Content model is ambiguous: conflict between the",numberNth n1,
	       "and the",numberNth n2,"occurrence of element",quoteData a^".",
	       "Using an approximation instead"]
	    | ERR_MUST_ESCAPE c => ["Character",quoteChar c,"must be escaped for compatibility"]

	    (* interoperability errors *)
	    | ERR_EMPTY_TAG_INTER elem =>
	      ["Empty-element tag for element",quoteData elem,"with non-EMPTY declared content"]
	    | ERR_MUST_BE_EMPTY elem => 
	      ["An empty-element tag must be used for element type",
	       quoteData elem,"with EMPTY declared content"]

	    (* decoding errors *) 
	    | ERR_DECODE_ERROR err => "Decoding error:"::Decode.Error.decodeMessage err

      fun warningMessage warn =
	 case warn
	   of WARN_NO_XML_DECL => ["Document entity has no XML declaration"]

	    | WARN_MULT_DECL(what,name) => 
	      ["Repeated declaration for",Item2String what,quoteData name]
	    | WARN_SHOULD_DECLARE(ents) =>
	      let val (one,more) = (hd ents,tl ents)
	      in case more 
		   of nil => ["The predefined entity",quoteData one,"should have been declared"]
		    | _ => ["The predefined entities",List2xString ("",", ","") quoteData more,
			    "and",quoteData one,"should have been declared"]
	      end
	   
	    | WARN_ATT_UNDEC_ELEM elem =>
	      ["Attribute-list declaration for undeclared element type",quoteData elem]
	    | WARN_MULT_ATT_DECL elem => 
	      ["Repeated attribute-list declaration for element type",quoteData elem]
	    | WARN_MULT_ATT_DEF(elem,att) =>
	      ["Repeated definition of attribute",quoteData att,"for element type",quoteData elem]
	    | WARN_ENUM_ATTS(elem,names) =>
	      ["The following name tokens occur more than once in the enumerated attribute",
	       "types of element",quoteData elem^":",List2xString ("",", ","") quoteData names]

	    | WARN_DFA_TOO_LARGE (elem,max) =>
	      ["The finite state machine for the content model of element type",
	       quoteData elem,"would have more than the maximal allowed number of",
	       Int2String max,"states. Using an approximation instead"]

	    | WARN_NON_ASCII_URI c =>
	      ["System identifier contains non-ASCII character",quoteChar c]

   end

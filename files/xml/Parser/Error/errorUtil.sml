

signature ErrorUtil =
   sig
      val isFatalError      : ErrorData.Error -> bool
      val isDecodeError     : ErrorData.Error -> bool
      val isSyntaxError     : ErrorData.Error -> bool
      val isValidityError   : ErrorData.Error -> bool
      val isWellFormedError : ErrorData.Error -> bool
   end

structure ErrorUtil : ErrorUtil = 
   struct
      open ErrorData

      fun isDecodeError err =
	 case err
	   of ERR_DECODE_ERROR _ => true
	    | _                  => false

      fun isSyntaxError err = 
	 case err
	   of ERR_EMPTY _           => true
	    | ERR_ENDED_BY_EE _     => true
	    | ERR_EXPECTED _        => true
	    | ERR_MISSING_WHITE     => true
	    | ERR_NON_XML_CHAR _    => true
	    | ERR_NON_XML_CHARREF _ => true
	    | _                     => false

      fun isWellFormedError err =
	 case err
	   of ERR_CANT_PARSE _       => true
	    | ERR_ELEM_ENT_NESTING _ => true
	    | ERR_ELEM_TYPE_MATCH _  => true
	    | ERR_OMITTED_END_TAG _  => true
	    | ERR_IGNORED_END_TAG _  => true
	    | ERR_ENDED_IN_PROLOG    => true
	    | ERR_FORBIDDEN_HERE _   => true
	    | ERR_ILLEGAL_ENTITY _   => true
	    | ERR_MULTIPLE_DTD       => true
	    | ERR_MULT_ATT_SPEC _    => true
	    | ERR_RECURSIVE_ENTITY _ => true
	    | ERR_UNDEC_ENTITY _     => true 
	    | _ => isSyntaxError err 

      fun isFatalError err =
	 case err
	   of ERR_NO_SUCH_FILE _ => true
	    | _ => isWellFormedError err 

      fun isValidityError err = 
	 case err
	   of ERR_AT_LEAST_ONE _      => true
	    | ERR_AT_MOST_ONE _	      => true
	    | ERR_ATT_IS_NOT _	      => true
	    | ERR_EXACTLY_ONE _       => true
	    | ERR_FIXED_VALUE _       => true
	    | ERR_ID_DEFAULT   	      => true
	    | ERR_MISSING_ATT _       => true
	    | ERR_MULT_ID_ELEM _      => true
	    | ERR_MUST_BE_AMONG _     => true
	    | ERR_MUST_BE_UNPARSED _  => true
	    | ERR_REPEATED_ID _       => true
	    | ERR_UNDECL_ATT _        => true
	    | ERR_UNDECL_ID _ 	      => true
	    | ERR_BAD_ELEM _ 	      => true
	    | ERR_ELEM_CONTENT _      => true
	    | ERR_EMPTY_TAG _ 	      => true
	    | ERR_ENDED_EARLY _	      => true
	    | ERR_MULT_MIXED _ 	      => true
	    | ERR_NONEMPTY _ 	      => true
	    | ERR_REDEC_ELEM _	      => true
	    | ERR_ROOT_ELEM _	      => true
	    | ERR_DECL_ENT_NESTING _  => true
	    | ERR_EE_INT_SUBSET       => true
	    | ERR_GROUP_ENT_NESTING _ => true
	    | ERR_NO_DTD 	      => true
	    | ERR_STANDALONE_DEF _    => true
	    | ERR_STANDALONE_ELEM _   => true
	    | ERR_STANDALONE_ENT _    => true
	    | ERR_STANDALONE_NORM _   => true
	    | ERR_UNDECLARED _	      => true
	    | _                       => false
   end

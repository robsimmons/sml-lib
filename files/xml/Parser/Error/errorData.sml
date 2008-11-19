structure ErrorData =
   struct
      (*--------------------------------------------------------------------*)
      (* a position holds the filename, line and column number.             *)
      (*--------------------------------------------------------------------*)
      type Position = string * int * int 
      val nullPosition = ("",0,0)

      datatype ExpItem = 
	  EXP_CHAR of UniChar.Char
	| EXP_DATA of UniChar.Data
	| EXP_STRING of string
      type Expected = ExpItem list 
      type Found = UniChar.Data

      datatype Location = 
	  LOC_NONE
	| LOC_AFTER_DTD
	| LOC_ATT_DECL
	| LOC_ATT_DEFAULT of Position
	| LOC_ATT_VALUE
	| LOC_CDATA
	| LOC_CHOICE
	| LOC_COMMENT
	| LOC_CONTENT
	| LOC_DECL
	| LOC_DOC_DECL
	| LOC_ELEM_DECL
	| LOC_ENCODING
	| LOC_ENT_DECL
	| LOC_ENT_VALUE
	| LOC_EPILOG
	| LOC_ETAG
	| LOC_IGNORED
	| LOC_INCLUDED
	| LOC_INT_DECL
	| LOC_INT_SUBSET
	| LOC_LITERAL
	| LOC_MIXED
	| LOC_NOT_DECL
	| LOC_OUT_COND
	| LOC_PROC
	| LOC_PROLOG
	| LOC_PUB_LIT
	| LOC_SEQ
	| LOC_STAG
	| LOC_SUBSET
	| LOC_SYS_LIT
	| LOC_TEXT_DECL
	| LOC_VERSION
	| LOC_XML_DECL

      datatype EntityClass =
	  ENT_GENERAL
	| ENT_PARAMETER
	| ENT_EXTERNAL
	| ENT_UNPARSED

      datatype Item = 
	  IT_ATT_NAME
	| IT_CDATA
	| IT_CHAR of UniChar.Char
	| IT_CHAR_REF
	| IT_COND
	| IT_DATA of UniChar.Data
	| IT_DECL
	| IT_DTD
 	| IT_ELEM
	| IT_ENT_NAME
	| IT_ETAG
	| IT_GEN_ENT
	| IT_ID_NAME
	| IT_LANG_ID
	| IT_NAME
	| IT_NMTOKEN
	| IT_NOT_NAME
	| IT_NOTATION
	| IT_PAR_ENT 
	| IT_PAR_REF
	| IT_REF
	| IT_STAG
	| IT_TARGET

      datatype Error = 
	 (* syntax errors *)
	  ERR_EMPTY of Location
	| ERR_ENDED_BY_EE of Location
	| ERR_EXPECTED of Expected * Found
	| ERR_NON_XML_CHAR of UniChar.Char
	| ERR_MISSING_WHITE
	| ERR_NON_XML_CHARREF of UniChar.Char
	| ERR_MUST_CHARREF of UniChar.Char (* for XML 1.1 *)

	(* other well-formedness errors *)
	| ERR_CANT_PARSE of Location
	| ERR_ELEM_ENT_NESTING of UniChar.Data
	| ERR_ELEM_TYPE_MATCH of UniChar.Data * UniChar.Data
	| ERR_OMITTED_END_TAG of UniChar.Data
	| ERR_IGNORED_END_TAG of UniChar.Data * UniChar.Data
	| ERR_ENDED_IN_PROLOG
	| ERR_FORBIDDEN_HERE of Item * Location
	| ERR_ILLEGAL_ENTITY of EntityClass * UniChar.Data * Location 
	| ERR_MULTIPLE_DTD
	| ERR_MULT_ATT_SPEC of UniChar.Data
	| ERR_RECURSIVE_ENTITY of EntityClass * UniChar.Data
	| ERR_UNDEC_ENTITY of EntityClass * UniChar.Data

	(* validity errors concerning attributes *)
	| ERR_AT_LEAST_ONE of Item
	| ERR_AT_MOST_ONE of Item
	| ERR_ATT_IS_NOT of UniChar.Data * Item
	| ERR_EXACTLY_ONE of Item
	| ERR_FIXED_VALUE of UniChar.Data * UniChar.Vector * UniChar.Vector
	| ERR_ID_DEFAULT
	| ERR_MISSING_ATT of UniChar.Data
	| ERR_MULT_ID_ELEM of UniChar.Data
	| ERR_MUST_BE_AMONG of Item * UniChar.Data * UniChar.Data list
	| ERR_MUST_BE_UNPARSED of UniChar.Data * Location
	| ERR_REPEATED_ID of UniChar.Data
	| ERR_UNDECL_ATT of UniChar.Data * UniChar.Data
	| ERR_UNDECL_ID of UniChar.Data * Position list

	(* validity errors concerning elements *)
	| ERR_BAD_ELEM of UniChar.Data * UniChar.Data
	| ERR_ELEM_CONTENT of Item
	| ERR_EMPTY_TAG of UniChar.Data
	| ERR_ENDED_EARLY of UniChar.Data 
	| ERR_MULT_MIXED of UniChar.Data 
	| ERR_NONEMPTY of UniChar.Data
	| ERR_REDEC_ELEM of UniChar.Data
	| ERR_ROOT_ELEM of UniChar.Data * UniChar.Data

	(* other validity errors *)
	| ERR_DECL_ENT_NESTING of Location
	| ERR_EE_INT_SUBSET 
	| ERR_GROUP_ENT_NESTING of Location
 	| ERR_NO_DTD
	| ERR_STANDALONE_DEF of UniChar.Data
	| ERR_STANDALONE_ELEM of UniChar.Data
	| ERR_STANDALONE_ENT of EntityClass *UniChar.Data
	| ERR_STANDALONE_NORM of UniChar.Data
	| ERR_UNDECLARED of Item * UniChar.Data * Location
	  
	(* miscellaneous errors *)
	| ERR_DECL_PREDEF of UniChar.Data * UniChar.Vector
	| ERR_NO_SUCH_FILE of string * string
	| ERR_RESERVED of UniChar.Data * Item
	| ERR_VERSION of string
	| ERR_XML_SPACE

	(* compatibility errors *)
	| ERR_AMBIGUOUS of UniChar.Data * int * int
	| ERR_MUST_ESCAPE of UniChar.Char

	(* interoperability errors *)
	| ERR_EMPTY_TAG_INTER of UniChar.Data
	| ERR_MUST_BE_EMPTY of UniChar.Data 

	(* decoding errors *)
	| ERR_DECODE_ERROR of Decode.Error.DecodeError

      datatype Warning = 
	  WARN_NO_XML_DECL

	| WARN_MULT_DECL of Item * UniChar.Data
	| WARN_SHOULD_DECLARE of UniChar.Data list

	| WARN_ATT_UNDEC_ELEM of UniChar.Data
	| WARN_MULT_ATT_DECL of UniChar.Data
	| WARN_MULT_ATT_DEF of UniChar.Data * UniChar.Data
	| WARN_ENUM_ATTS of UniChar.Data * UniChar.Data list

	| WARN_DFA_TOO_LARGE of UniChar.Data * int
       
	| WARN_NON_ASCII_URI of UniChar.Char
   end











signature CatError =
   sig
      type Position
      val nullPosition : Position
      val Position2String : Position -> string

      datatype Location =
	 LOC_CATALOG
       | LOC_COMMENT
       | LOC_NOCOMMENT
       | LOC_PUBID
       | LOC_SYSID
	 
      datatype Expected =
	 EXP_NAME
       | EXP_LITERAL

      datatype CatError =
	 ERR_DECODE_ERROR of Decode.Error.DecodeError
       | ERR_NO_SUCH_FILE of string * string
       | ERR_ILLEGAL_HERE of UniChar.Char * Location
       | ERR_MISSING_WHITE 
       | ERR_EOF of Location
       | ERR_EXPECTED of Expected * UniChar.Char
       | ERR_XML of Errors.Error
       | ERR_MISSING_ATT of UniChar.Data * UniChar.Data
       | ERR_NON_PUBID of UniChar.Data * UniChar.Data

      val catMessage : CatError -> string list
   end

structure CatError : CatError = 
   struct
      open Errors UtilError UtilString

      type Position = string * int * int 
      val nullPosition = ("",0,0)

      fun Position2String (fname,l,c) = 
	 if fname="" then "" 
	 else String.concat ["[",fname,":",Int2String l,".",Int2String c,"]"]

      datatype Location =
	 LOC_CATALOG
       | LOC_COMMENT
       | LOC_NOCOMMENT
       | LOC_PUBID
       | LOC_SYSID

      fun Location2String loc =
	 case loc 
	   of LOC_CATALOG => "catalog file"
	    | LOC_COMMENT => "comment"
	    | LOC_NOCOMMENT => "something other than a comment"
	    | LOC_PUBID   => "public identifier"
	    | LOC_SYSID   => "system identifier"
      
      fun InLocation2String loc =
	 case loc 
	   of LOC_CATALOG => "in a catalog file"
	    | LOC_COMMENT => "in a comment"
	    | LOC_NOCOMMENT => "outside of comments"
	    | LOC_PUBID   => "in a public identifier"
	    | LOC_SYSID   => "in a system identifier"

      datatype Expected =
	 EXP_NAME
       | EXP_LITERAL

      fun Expected2String exp =
	 case exp
	   of EXP_NAME => "a name"
	    | EXP_LITERAL => "a literal"

      datatype CatError =
	 ERR_DECODE_ERROR of Decode.Error.DecodeError
       | ERR_NO_SUCH_FILE of string * string
       | ERR_ILLEGAL_HERE of UniChar.Char * Location
       | ERR_MISSING_WHITE 
       | ERR_EOF of Location
       | ERR_EXPECTED of Expected * UniChar.Char
       | ERR_XML of Error
       | ERR_MISSING_ATT of UniChar.Data * UniChar.Data
       | ERR_NON_PUBID of UniChar.Data * UniChar.Data

      fun catMessage err =
	 case err
	   of ERR_DECODE_ERROR err => Decode.Error.decodeMessage err
	    | ERR_NO_SUCH_FILE(f,msg) => ["Could not open file",quoteErrorString f,"("^msg^")"]
	      
	    | ERR_ILLEGAL_HERE (c,loc) => 
	      ["Character",quoteErrorChar c,"is not allowed",InLocation2String loc]
	      
	    | ERR_MISSING_WHITE => ["Missing white space"]
	    | ERR_EOF loc => [toUpperFirst (Location2String loc),"ended by end of file"]
	    | ERR_EXPECTED (exp,c) => 
	      ["Expected",Expected2String exp,"but found",quoteErrorChar c]

	    | ERR_XML err => errorMessage err
	    | ERR_MISSING_ATT(elem,att) => 
	      ["Element",quoteErrorData elem,"has no",quoteErrorData att,"attribute"]
	    | ERR_NON_PUBID(att,cs) => 
	      ["Value specified for attribute",quoteErrorData att,"contains non-PublicId",
	       case cs 
		 of [c] => "character"^quoteErrorChar c
		  | cs => List2xString ("characters ",", ","") quoteErrorChar cs]
   end

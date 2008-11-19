






signature EncodeError = 
   sig
      datatype EncodeError = 
	 ERR_ILLEGAL_CHAR of UniChar.Char * string

      val encodeMessage : EncodeError -> string list

      exception EncodeError of EncodeBasic.File * EncodeError 
   end

structure EncodeError : EncodeError =
   struct
      open 
	 UtilString
	 UniChar

      datatype EncodeError = 
	 ERR_ILLEGAL_CHAR of UniChar.Char * string

      fun encodeMessage err =
	 case err
	   of ERR_ILLEGAL_CHAR(c,what) => [Char2Uni c,"is not",prependAnA what,"character"]

      exception EncodeError of EncodeBasic.File * EncodeError 
   end

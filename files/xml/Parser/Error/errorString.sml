











signature ErrorString =
   sig
      val errorChar2String   : UniChar.Char -> string
      val errorData2String   : UniChar.Data -> string
      val errorVector2String : UniChar.Vector -> string

      val quoteErrorChar0    : UniChar.Char -> string
      val quoteErrorChar     : UniChar.Char -> string
      val quoteErrorData     : UniChar.Data -> string
      val quoteErrorString   : string -> string
      val quoteErrorVector   : UniChar.Vector -> string

      val Position2String    : ErrorData.Position -> string

      val Expected2String    : ErrorData.Expected -> string
      val Found2String       : ErrorData.Found -> string

      val Item2String        : ErrorData.Item -> string
      val AnItem2String      : ErrorData.Item -> string

      val Location2String    : ErrorData.Location -> string
      val InLocation2String  : ErrorData.Location -> string

      val EntityClass2String : ErrorData.EntityClass -> string
   end

structure ErrorString : ErrorString =
   struct
      open
	 ErrorData UniChar UtilString
	 

      fun errorChar2String c = 
	 case c
	   of 0wx9 => "\\t"
	    | 0wxA => "\\n"
	    | _ => if (c>=0wx20 andalso c<0wx7F) orelse (c>0wx9F andalso c<0wx100) then String.implode [Char2char c]
		   else "U+"^UtilString.toUpperString
		      (StringCvt.padLeft #"0" 4 (Chars.toString c))

      fun errorData2String cs = 
	 String.concat (map errorChar2String cs)
      fun errorVector2String vec = 
	 errorData2String (Vector.foldr (op ::) nil vec)
      
      val QUOTE = "'"
      fun quoteErrorChar0 c = QUOTE^errorChar2String c^QUOTE
      fun quoteErrorChar c = if c=0wx0 then "entity end" else QUOTE^errorChar2String c^QUOTE
      fun quoteErrorData cs = QUOTE^errorData2String cs^QUOTE
      fun quoteErrorString s = QUOTE^s^QUOTE
      fun quoteErrorVector v = QUOTE^errorVector2String v^QUOTE

      fun Position2String (fname,l,c) = 
	 if fname="" then "" 
	 else String.concat ["[",fname,":",Int2String l,".",Int2String c,"]"]

      fun ExpItem2String exp =
	 case exp
	   of EXP_CHAR c => quoteErrorChar c
	    | EXP_DATA cs => quoteErrorData cs
	    | EXP_STRING s => s

      fun Expected2String exp =
	 case exp 
	   of nil => "nothing"
	    | [one] => ExpItem2String one
	    | _ => let val l=List.length exp 
		   in List2xString ("",", ","") ExpItem2String (List.take (exp,l-1))
		      ^" or "^ExpItem2String (List.last exp)
		   end
      fun Found2String fnd =
	 case fnd
	   of [0wx0] => "entity end"
	    | cs => quoteErrorData cs

      fun Location2String loc =
	 case loc
	   of LOC_NONE => "nothing"
	    | LOC_AFTER_DTD => "document instance"
	    | LOC_ATT_DECL => "attribute list declaration"
	    | LOC_ATT_DEFAULT pos => "default value declared at "^Position2String pos
	    | LOC_ATT_VALUE => "attribute value"
	    | LOC_CDATA => "CDATA section"
	    | LOC_CHOICE => "choice list"
	    | LOC_COMMENT => "comment"
	    | LOC_CONTENT => "content"
	    | LOC_DECL => "declaration"
	    | LOC_DOC_DECL => "document type declaration"
	    | LOC_ELEM_DECL => "element type declaration"
	    | LOC_ENCODING => "encoding name"
	    | LOC_ENT_DECL => "entity declaration"
	    | LOC_ENT_VALUE => "entity value"
	    | LOC_EPILOG => "epilog"
	    | LOC_ETAG => "end-tag"
	    | LOC_IGNORED => "ignored section"
	    | LOC_INCLUDED => "included section"
	    | LOC_INT_DECL => "declaration in the internal subset"
	    | LOC_INT_SUBSET => "internal subset"
	    | LOC_LITERAL => "literal"
	    | LOC_MIXED => "Mixed list"
	    | LOC_NOT_DECL => "notation declaration"
	    | LOC_OUT_COND => "outside a conditional section"
	    | LOC_PROLOG => "prolog"
	    | LOC_PROC => "processing instruction"
	    | LOC_PUB_LIT => "public identifier"
	    | LOC_SEQ => "sequence list"
	    | LOC_STAG => "start-tag"
	    | LOC_SUBSET => "declaration subset"
	    | LOC_SYS_LIT => "system identifier"
	    | LOC_TEXT_DECL => "text declaration"
	    | LOC_VERSION => "version number"
	    | LOC_XML_DECL => "XML declaration"
      fun InLocation2String loc =
	 case loc
	   of LOC_NONE => ""
	    | LOC_AFTER_DTD => "after the DTD"
	    | LOC_CONTENT => "in content"
	    | LOC_ATT_DEFAULT pos => "in default value declared at "^Position2String pos
	    | LOC_DOC_DECL => "in the document type declaration"
	    | LOC_EPILOG => "after the root element"
	    | LOC_INT_SUBSET => "in the internal subset"
	    | LOC_OUT_COND => "outside a conditional section"
	    | LOC_PROLOG => "in prolog"
	    | LOC_SUBSET => "in the declaration subset"
	    | LOC_XML_DECL => "in the XML declaration"
	    | _ => "in "^prependAnA (Location2String loc)

      fun EntityClass2String ent =
	 case ent
	   of ENT_GENERAL => "general"
	    | ENT_PARAMETER => "parameter"
	    | ENT_UNPARSED => "unparsed"
	    | ENT_EXTERNAL => "external"

      fun Item2String item =
	 case item 
	   of IT_ATT_NAME => "attribute name"
	    | IT_CDATA => "CDATA section"
	    | IT_CHAR c => "character "^quoteErrorChar c
	    | IT_CHAR_REF => "character reference"
	    | IT_COND => "conditional section"
	    | IT_DATA cs => if null cs then "character data" else quoteErrorData cs
	    | IT_DECL => "declaration"
	    | IT_DTD => "document type declaration"
	    | IT_ELEM => "element type"
	    | IT_ENT_NAME => "entity name"
	    | IT_ETAG => "end-tag"
	    | IT_GEN_ENT => "general entity"
	    | IT_ID_NAME => "ID name"
	    | IT_LANG_ID => "language identifier"
	    | IT_NAME => "name"
	    | IT_NMTOKEN => "name token"
	    | IT_NOT_NAME => "notation name"
	    | IT_NOTATION => "notation"
	    | IT_PAR_ENT => "parameter entity"
	    | IT_PAR_REF => "parameter entity reference" 
	    | IT_REF => "reference"
	    | IT_STAG => "start-tag"
	    | IT_TARGET => "target name"

      fun AnItem2String item =
	 case item 
	   of IT_CHAR c => Item2String item
	    | IT_DATA cs => Item2String item
	    | _ => prependAnA (Item2String item) 
   end
   

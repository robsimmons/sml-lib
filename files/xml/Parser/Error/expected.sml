



structure Expected = 
   struct
      local 
	 open UniChar ErrorData 
      in 
	 val expAnElemName  = [EXP_STRING "an element name"]
	 val expAnEntName   = [EXP_STRING "an entity name"]
	 val expAName       = [EXP_STRING "a name"]
	 val expANameToken  = [EXP_STRING "a name token"]
	 val expANotName    = [EXP_STRING "a notation name"]
	 val expATarget     = [EXP_STRING "a target name"]
	 val expAttDefKey   = [EXP_DATA (String2Data "REQUIRED"),EXP_DATA (String2Data "IMPLIED"),
			       EXP_DATA (String2Data "FIXED")]
	 val expAttNameGt   = [EXP_STRING "an attribute name",EXP_CHAR 0wx3E]
	 val expAttSTagEnd  = [EXP_STRING "an attribute name",EXP_CHAR 0wx3E,
			       EXP_DATA(String2Data "/>")]
	 val expAttType     = [EXP_CHAR 0wx28,EXP_DATA (String2Data "CDATA"),
			       EXP_DATA (String2Data "ID"),EXP_DATA (String2Data "IDREF"),
			       EXP_DATA (String2Data "IDREFS"),EXP_DATA (String2Data "ENTITY"),
			       EXP_DATA (String2Data "ENTITIES"),EXP_DATA (String2Data "NMTOKEN"),
			       EXP_DATA (String2Data "NMTOKENS"),EXP_DATA (String2Data "NOTATION")]
	 val expBarRpar     = [EXP_CHAR 0wx29,EXP_CHAR 0wx7C]
	 val expCdata       = [EXP_DATA (String2Data "CDATA")]
	 fun expConCRpar c  = [EXP_CHAR 0wx29,EXP_CHAR c]
	 val expConRpar     = [EXP_CHAR 0wx29,EXP_CHAR 0wx2C,EXP_CHAR 0wx7C]
	 val expCondStatus  = [EXP_DATA (String2Data "IGNORE"),EXP_DATA (String2Data "INCLUDE")]
	 val expContSpec    = [EXP_CHAR 0wx28,EXP_DATA (String2Data "ANY"),
			       EXP_DATA (String2Data "EMPTY")]
	 val expElemLpar    = [EXP_STRING "an element name",EXP_CHAR 0wx28]
	 val expEncStand    = [EXP_DATA (String2Data "encoding"),
			       EXP_DATA (String2Data "standalone")]
	 val expDash        = [EXP_CHAR 0wx2D]
	 val expDashDocLbrk = [EXP_CHAR 0wx2D,EXP_CHAR 0wx5B,EXP_DATA (String2Data "DOCTYPE")]
	 val expDashLbrack  = [EXP_CHAR 0wx2D,EXP_CHAR 0wx5B]
	 val expDigitX      = [EXP_STRING "a digit",EXP_CHAR 0wx78]
	 val expEncoding    = [EXP_DATA (String2Data "encoding")]
	 val expEncVers     = [EXP_DATA (String2Data "encoding"),EXP_DATA (String2Data "version")]
	 val expEntNamePero = [EXP_STRING "an entity name",EXP_CHAR 0wx25]
	 val expEq          = [EXP_CHAR 0wx3D]
	 val expExclQuest   = [EXP_CHAR 0wx21,EXP_CHAR 0wx3F]
	 val expExtId       = [EXP_DATA (String2Data "PUBLIC"),EXP_DATA (String2Data "SYSTEM")]
	 val expGt          = [EXP_CHAR 0wx3E]
	 val expGtNdata     = [EXP_CHAR 0wx3E,EXP_DATA (String2Data "NDATA")]
	 val expHexDigit    = [EXP_STRING "a hexadecimal digit"]
	 val expInSubset    = [EXP_CHAR 0wx3C,EXP_CHAR 0wx5D,EXP_CHAR 0wx25,
			       EXP_STRING "white space"]
	 val expLbrack      = [EXP_CHAR 0wx5B]
	 val expLitQuote    = [EXP_CHAR 0wx22,EXP_CHAR 0wx27]
	 val expLitQuotExt  = [EXP_CHAR 0wx22,EXP_CHAR 0wx27,
			       EXP_DATA (String2Data "PUBLIC"),EXP_DATA (String2Data "SYSTEM")]
	 val expLpar        = [EXP_CHAR 0wx28]
	 val expNoYes       = [EXP_DATA (String2Data "no"),EXP_DATA (String2Data "yes")]
	 val expPcdata      = [EXP_DATA (String2Data "PCDATA")]
	 val expProcEnd     = [EXP_DATA (String2Data "?>")]
	 val expQuoteRni    = [EXP_CHAR 0wx22,EXP_CHAR 0wx27,EXP_CHAR 0wx23]
	 val expRbrack      = [EXP_CHAR 0wx5D]
	 val expRep         = [EXP_CHAR 0wx2A]
	 val expSemi        = [EXP_CHAR 0wx3B]
	 val expStandOpt    = [EXP_DATA (String2Data "standalone"),EXP_DATA (String2Data "?>")]
	 val expStartEnc    = [EXP_STRING "a letter"]
	 val expStartMarkup = [EXP_DATA (String2Data "--"),EXP_DATA (String2Data "ATTLIST"),
			       EXP_DATA (String2Data "ELEMENT"),EXP_DATA (String2Data "ENTITY"),
			       EXP_DATA (String2Data "NOTATION")]
	 val expVersion     = [EXP_DATA (String2Data "version")]
      end
   end

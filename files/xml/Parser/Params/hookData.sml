structure HookData =
   struct
      type StartEnd = Errors.Position * Errors.Position
      (*--------------------------------------------------------------------*)
      (* a text declaration consists of a version info and an encoding decl.*)
      (* an xml declaration has an additional standalone decl.              *) 
      (*--------------------------------------------------------------------*)
      type TextDecl = string option * string option
      type XmlDecl = string option * string option * bool option

      type XmlInfo       = Uri.Uri * Encoding.Encoding * XmlDecl option
      type ExtSubsetInfo = Uri.Uri * Encoding.Encoding * TextDecl option
      type SubsetInfo    = Errors.Position
      type EndDtdInfo    = Errors.Position

      type ErrorInfo     = Errors.Position * Errors.Error
      type WarningInfo   = Errors.Position * Errors.Warning
      type NoFileInfo    = string * string

      type CommentInfo   = StartEnd * UniChar.Vector
      type ProcInstInfo  = StartEnd * UniChar.Data * Errors.Position * UniChar.Vector

      type DtdInfo       = int * Base.ExternalId option

      datatype AttPresent =
	 AP_IMPLIED
       | AP_MISSING
       | AP_DEFAULT of UniChar.Vector * UniChar.Vector * Base.AttValue option
       | AP_PRESENT of UniChar.Vector * UniChar.Vector * Base.AttValue option

      type AttSpec = int * AttPresent * (UniChar.Data * UniChar.Data) option
      type AttSpecList = AttSpec list

      type EndTagInfo    = StartEnd * int * (int * UniChar.Data) option
      type StartTagInfo  = StartEnd * int * AttSpecList * UniChar.Data * bool
      type WhiteInfo     = UniChar.Vector
      type CDataInfo     = StartEnd * UniChar.Vector
      type DataInfo      = StartEnd * UniChar.Vector * bool 

      type CharRefInfo   = StartEnd * UniChar.Char * UniChar.Vector 
      type GenRefInfo    = StartEnd * int * Base.GenEntity * bool 
      type ParRefInfo    = StartEnd * int * Base.ParEntity * bool 
      type EntEndInfo    = Errors.Position

      datatype MarkupDecl = 
	 DEC_ATTLIST of int * (int * Base.AttType * Base.AttDefault) list * bool
       | DEC_ELEMENT of int * Base.ContentSpec * bool
       | DEC_GEN_ENT of int * Base.GenEntity * bool
       | DEC_PAR_ENT of int * Base.ParEntity * bool
       | DEC_NOTATION of int * Base.ExternalId * bool 
      type DeclInfo = StartEnd * MarkupDecl

      fun isExtDecl decl =
	 case decl
	   of DEC_ATTLIST(_,_,ext) => ext
	    | DEC_ELEMENT(_,_,ext) => ext
	    | DEC_GEN_ENT(_,_,ext) => ext
	    | DEC_PAR_ENT(_,_,ext) => ext
	    | DEC_NOTATION(_,_,ext) => ext
   end

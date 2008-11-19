(*--------------------------------------------------------------------------*)
(* Structure: Dtd                                                           *)
(*                                                                          *)
(* Depends on:                                                              *)
(*   UniChar                                                                *)
(*   DtdAttributes                                                          *)
(*   DtdElements                                                            *)
(*   DtdEntities                                                            *)
(*   DtdNotations                                                           *)
(*   DtdStandalone                                                          *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   initDtdTables     : none                                               *)
(*   AttIdx2String     : NoSuchSymbol		                            *)
(*   ElemIdx2String    : NoSuchIndex                                        *)         
(*   GenEntIdx2String  : NoSuchIndex		                            *)
(*   IdIdx2String      : NoSuchIndex		                            *)
(*   NotIdx2String     : NoSuchIndex 		                            *)
(*   GenEntity2String  : NoSuchIndex		                            *)
(*   ElemInfo2String   : NoSuchIndex NoSuchSymbol                           *)
(*   printGenEntTable  : NoSuchIndex		                            *)
(*   printElementTable : NoSuchIndex NoSuchSymbol                           *)
(*   printDtdTables    : NoSuchIndex NoSuchSymbol                           *)
(*--------------------------------------------------------------------------*)
signature DtdManager =
   sig
      include Entities
      include Dtd

      exception AttValue of AppData

      val makeAttValue  : Dtd -> AppData * State 
	 -> int * Base.AttType * bool * bool * UniChar.Data 
	 -> UniChar.Vector * (Base.AttValue option * AppData)
      val checkAttValue : Dtd -> AppData * State 
	 -> Base.AttDef * UniChar.Vector * UniChar.Data
	 -> HookData.AttPresent * AppData
      val genMissingAtts  : Dtd -> AppData * State 
	 -> Base.AttDefList * HookData.AttSpecList -> HookData.AttSpecList * AppData
      val handleUndeclAtt : Dtd -> AppData * State 
	 -> int * UniChar.Data * int * UniChar.Data -> AppData
      val handleUndeclElement : Dtd -> int -> Base.ElemInfo

      val checkAttName    : AppData * State -> UniChar.Data -> AppData
      val checkElemName   : AppData * State -> UniChar.Data -> AppData
      val checkDefinedIds : Dtd -> AppData * State -> AppData
      val checkMultEnum   : Dtd -> AppData * State -> AppData
      val checkPreDefined : Dtd -> AppData * State -> AppData
      val checkUnparsed   : Dtd -> AppData -> AppData

      val enterAttList : Dtd -> AppData * State -> int -> AppData

      val addAttribute : Dtd -> AppData * State -> int * Base.AttDef             -> AppData 
      val addElement   : Dtd -> AppData * State -> int * Base.ContentSpec * bool -> AppData 
      val addGenEnt    : Dtd -> AppData * State -> int * Base.GenEntity * bool   -> AppData
      val addNotation  : Dtd -> AppData * State -> int * Base.ExternalId         -> AppData
      val addParEnt    : Dtd -> AppData * State -> int * Base.ParEntity * bool   -> AppData
   end

functor DtdManager (structure Dtd           : Dtd
		    structure Hooks         : Hooks
		    structure ParserOptions : ParserOptions) : DtdManager =
   struct
      structure Entities      = Entities      (structure Hooks = Hooks)
      structure DtdAttributes = DtdAttributes (structure Dtd = Dtd
					       structure Entities = Entities
					       structure ParserOptions = ParserOptions)	 
      open 
	 Dtd
	 DtdAttributes
   end

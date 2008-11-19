


signature Hooks =
   sig
      type AppData 
      type AppFinal 
      
      val hookXml       : AppData * HookData.XmlInfo -> AppData
      val hookFinish    : AppData -> AppFinal
	 
      val hookError     : AppData * HookData.ErrorInfo -> AppData
      val hookWarning   : AppData * HookData.WarningInfo -> AppData

      val hookProcInst  : AppData * HookData.ProcInstInfo -> AppData
      val hookComment   : AppData * HookData.CommentInfo -> AppData
      val hookWhite     : AppData * HookData.WhiteInfo -> AppData
      val hookDecl      : AppData * HookData.DeclInfo -> AppData

      val hookStartTag  : AppData * HookData.StartTagInfo -> AppData
      val hookEndTag    : AppData * HookData.EndTagInfo   -> AppData
      val hookCData     : AppData * HookData.CDataInfo -> AppData
      val hookData      : AppData * HookData.DataInfo  -> AppData

      val hookCharRef   : AppData * HookData.CharRefInfo -> AppData
      val hookGenRef    : AppData * HookData.GenRefInfo -> AppData
      val hookParRef    : AppData * HookData.ParRefInfo -> AppData
      val hookEntEnd    : AppData * HookData.EntEndInfo -> AppData

      val hookDocType   : AppData * HookData.DtdInfo -> AppData
      val hookSubset    : AppData * HookData.SubsetInfo -> AppData
      val hookExtSubset : AppData * HookData.ExtSubsetInfo -> AppData
      val hookEndDtd    : AppData * HookData.EndDtdInfo -> AppData
   end

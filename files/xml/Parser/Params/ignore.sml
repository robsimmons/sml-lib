structure IgnoreHooks = 
   struct
      type AppData = unit
      type AppFinal = unit

      fun hookXml(a,_) = a
      fun hookFinish a = a
	 
      fun hookError(a,_) = a
      fun hookWarning(a,_) = a

      fun hookProcInst(a,_) = a
      fun hookComment(a,_) = a
      fun hookWhite(a,_) = a
      fun hookDecl (a,_) = a

      fun hookStartTag(a,_) = a
      fun hookEndTag(a,_) = a
      fun hookCData(a,_) = a
      fun hookData(a,_) = a

      fun hookCharRef(a,_) = a
      fun hookGenRef(a,_) = a
      fun hookParRef(a,_) = a
      fun hookEntEnd(a,_) = a

      fun hookDocType(a,_) = a
      fun hookSubset(a,_) = a
      fun hookExtSubset(a,_) = a
      fun hookEndDtd(a,_) = a
   end









structure KeyData : Key = 
   struct
      type Key = UniChar.Data

      val null     = UniChar.nullData
      val hash     = UniChar.hashData
      val compare  = UniChar.compareData
      val toString = UniChar.Data2String
   end

structure DataDict = Dict (structure Key = KeyData) 
structure DataSymTab = SymTable (structure Key = KeyData) 



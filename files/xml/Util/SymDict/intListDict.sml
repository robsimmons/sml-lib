





structure KeyIntList : Key = 
   struct
      type Key = int list

      val null     = nil
      val hash     = UtilHash.hashList Word.fromInt
      val compare  = UtilCompare.compareList Int.compare
      val toString = UtilString.List2String Int.toString
   end

structure IntListDict = Dict (structure Key = KeyIntList) 
structure IntListSymTab = SymTable (structure Key = KeyIntList) 



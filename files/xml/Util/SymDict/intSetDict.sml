






structure KeyIntSet : Key = 
   struct
      open IntSets UtilString

      type Key = IntSet

      val null     = emptyIntSet
      val hash     = hashIntSet
      val compare  = compareIntSets
      val toString = (List2xString ("{",",","}") Int2String) o IntSet2List
   end

structure IntSetDict = Dict (structure Key = KeyIntSet) 
structure IntSetSymTab = SymTable (structure Key = KeyIntSet) 



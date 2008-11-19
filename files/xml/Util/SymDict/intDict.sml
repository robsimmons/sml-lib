






structure KeyInt : Key = 
   struct
      type Key = int

      val null     = 0
      val hash     = Word.fromInt
      val compare  = Int.compare
      val toString = Int.toString
   end

structure IntDict = Dict (structure Key = KeyInt) 
structure IntSymTab = SymTable (structure Key = KeyInt) 


